import FuncGuessing.gameLoop

import scala.io.StdIn
import scala.util.{Random, Try}

object ImpGuessing {
  def main(): Unit = {
    println("What is your name?")

    val name = StdIn.readLine()

    println(s"Hello, $name, welcome to the guessing game.")

    var exec: Boolean = true
    var correctGuesses: Int = 0
    var totalGuesses: Int = 0

    while(exec) {
      val num = Random.nextInt(5) + 1
      println(s"$name, please guess a number between 1 and 5:")

      val guess: Option[Int] = Try { StdIn.readLine().toInt } toOption

      if(guess.isDefined) {
        if(guess.get == num) {
          correctGuesses += 1
          println("You guessed right")
        } else {
          totalGuesses += 1
          println(s"You guessed wrong, the number was $num")
        }
      } else {
        println("You entered an invalid number")
      }

      println("Do you want to continue?")

      StdIn.readLine().toLowerCase match {
        case "y" =>
          exec = true
        case "n" =>
          exec = false
          println(s"Thanks for playing. You had $correctGuesses guesses out of a total of $totalGuesses tries")
        case _   =>
          exec = false
          println("Y u stupid?")
      }
    }
  }
}

/*
* Looking for functions that are:
* 1. Total
* 2. Deterministic
* 3. Pure
*
* First step is function composition. For this there are different mechanisms.
* We will focus on `for comprehension` -> Scala's “for comprehensions” are syntactic sugar for composition of multiple operations with foreach , map , flatMap , filter or withFilter . Scala actually translates a for-expression into calls to those methods, so any class providing them, or a subset of them, can be used with for comprehensions.
*
* Pretty much we defer things higher up in the call stack, so a program can be understood higher up.
* Also we need to try to define the function signatures as well as possible.
*
* Examples of functions that do not have the listed attributes:
*
* def println(s: String): Unit = ??? //not pure, as it is interacting with the external word, so Unit not cool
* def readLine(): String = ??? // not deterministic
* */
object FuncGuessing {
  //1. low hanging fruits, Option used to deal with function partiality.
  //By eliminating function partiality with get rid of any NPEs that we might have hidden in the code.
  //So there is benefit in just doing this.
  val parseInt: String => Option[Int] = (s: String) => Try(s.toInt).toOption

  //4. unsafeRun describes an interaction with the external world that produces an A.
  //IO is an immutable data structure; it's just a value
  case class IO[A](unsafeRun: () => A) { self =>
    // handy things for function composition

    //map- if we can get from an A to a B, then given an IO[A] we can get to an IO[B]
    def map[B](f: A => B): IO[B] = IO(() => f(self.unsafeRun())) // we haven't done anything here, we just created a description

    //flatMap -
    def flatMap[B](f: A => IO[B]): IO[B] = IO(() => f(self.unsafeRun()).unsafeRun())
  }

  object IO {
    def point[A](a: => A): IO[A] = IO(() => a)
  }

  //5. Based on the defined data structures we can now model the effects
  val putStrLn: String => IO[Unit] = (line: String) => IO(() => println(line)) //builds a value of type IO[Unit]
  val getStrLn: IO[String] = IO(() => StdIn.readLine()) //builds a value of type IO[String]
  //Again these are just descriptions of interactions with the external world
  //7. Now let's change the printLns

  //12. extract random int in a method
  def nextInt(upper: Int): IO[Int] = IO(() => Random.nextInt(upper))

  //11. recursive helper function
  def checkContinue(name: String): IO[Boolean] =
    for {
      _     <- putStrLn("Do you want to continue?")
      input <- getStrLn.map(_.toLowerCase)
      cont     <- input match {
                  case "y" => IO.point(true)
                  case "n" => IO.point(false)
                  case _   => checkContinue(name)
               }
    } yield cont

//  def gameLoop(): IO[Unit] = ??? //use ??? because we don;t implement it yet
//  def gameLoop(name :String): IO[Unit] = {
//    //but we want to use for comprehension to take advantage of all its glory :)
//    for {
//      //in style of pure fp, we can't allow effects, therefore:
//      num   <- nextInt(5).map(_ + 1) //can no longer add 1 to the IO of Int
//      _     <- putStrLn(s"$name, please guess a number between 1 and 5:")
//      input <- getStrLn
//      //folding over the Option
//      _     <- parseInt(input).fold(
//                  putStrLn("You did not enter a number")
//               )(guess =>
//                 if(guess == num) putStrLn("You guessed right")
//                 else putStrLn(s"You guessed wrong, the number was $num")
//               )
//      cont  <- checkContinue(name)
//      _     <- if(cont) gameLoop(name) else IO.point(())
//    } yield ()
//    //not tail recursive because for comprehension applies a final map, so you will run out of heap.

    //13. Let's deal with state. Main idea, don't mutate, but use a technique of copy on update.
    case class GameState(correctGuesses: Int = 0, totalGuesses: Int = 0) { self =>
      def guessedCorrectly(): GameState = self.copy(correctGuesses = self.correctGuesses + 1, totalGuesses = self.totalGuesses + 1)
    }

    //with state
    def gameLoop(name :String, gameState: GameState): IO[Unit] = {
      for {
        num       <- nextInt(5).map(_ + 1)
        _         <- putStrLn(s"$name, please guess a number between 1 and 5:")
        input     <- getStrLn
        //folding over the Option
        newState  <- parseInt(input).fold(
                       putStrLn("You did not enter a number").map(_ => gameState)
                     )(guess =>
                       if(guess == num) putStrLn("You guessed right").map(_ => gameState.guessedCorrectly())
                       else putStrLn(s"You guessed wrong, the number was $num").map(_ => gameState.copy(totalGuesses = gameState.totalGuesses + 1))
                     )
        cont      <- checkContinue(name)
        _         <- if(cont) gameLoop(name, newState)
                     else putStrLn(s"Thanks for playing. You had ${newState.correctGuesses} correct guesses out of a total of ${newState.totalGuesses} tries").map(_ => IO.point(()))
      } yield ()
  }

  def main(): IO[Unit] = {
    for {
      _    <- putStrLn("What is your name?") // _ denotes that we don't want to use the return type of the function.
      name <- getStrLn
      _    <- putStrLn(s"Hello, $name, welcome to the guessing game.")
      _ <- gameLoop(name, new GameState)
    } yield () //yield unit.
  }


//  def main(): IO[Unit] = {
//    //8. this does not do anything, it just returns an IO of Unit
//    //we're just going to wait until the beginning of the universe for it to be executed.
//    //this allows us to program in a pure functional style except at the top level
//    //at the top level we call all the effects, so the spread of effects is limited
//    //    putStrLn("What is your name?")
//
//    //9. so in order to have it do something we need to flatMap it (????)
//    //so we need to have 2 methods on the IO case class, map and flatMap
//    //having that in place allows us to use the functions in a for comprehension.
//    for {
//      _    <- putStrLn("What is your name?") // _ denotes that we don't want to use the return type of the function.
//      name <- getStrLn
//      _    <- putStrLn(s"Hello, $name, welcome to the guessing game.")
//      //10. at the exec we hit a loop, loops can be translated into fp using recursion.
//      //in order to do that let's create a helper function
//      _ <- gameLoop(name)
//    } yield () //yield unit.
//  }
//    val name = StdIn.readLine()
//
//    println(s"Hello, $name, welcome to the guessing game.")

    //cut this into the game loop
//    var exec: Boolean = true
//    var correctGuesses: Int = 0
//    var totalGuesses: Int = 0
//
//    while(exec) {
//      //3. But we still got things that are not functions, like nextInt or StdIn.readLine(), printLn etc
//      //This means that there are parts of our code base where we can assume purity, but then we need to switch to a procedural mindset.
//      //We also lose the benefits like easier testing and programming without having to dive in the implementation
//      //So how can we address it? The answer is convert an effect (interaction with the external world) into a description.
//      //So we need to build a data structure => 4.
//      val num = Random.nextInt(5) + 1
//      println(s"$name, please guess a number between 1 and 5:")
//
//      val guess: Option[Int] = parseInt(StdIn.readLine())
//      //2. no ifs, rather pattern matching
//      guess match {
//        case None => println("You entered an invalid number")
//        case Some(g) =>
//          if(g == num) {
//            correctGuesses += 1
//            println("You guessed right")
//          } else {
//            totalGuesses += 1
//            println(s"You guessed wrong, the number was $num")
//          }
//      }
//
//      var cont = true
//      while(cont) {
//        cont = false
//        println("Do you want to continue?")
//        StdIn.readLine() match {
//          case "y" =>
//            exec = true
//          case "n" =>
//            exec = false
//            println(s"Thanks for playing. You had $correctGuesses correct guesses out of a total of $totalGuesses tries")
//          case _   =>
//            cont = true
//            println("Y u stupid?")
//        }
//      }
//    }
//  }
}

object TestApp extends App {
  /**
    * All functions are total now; and nobody has to write the IO stuff etc etc.
    * There are libraries that give you nice tools for dealing with effects.
    * There is something not quite satisfying. -> in the unsafeRun we pass chunks of code and pass them around
    * We don't have the ability to see what's inside of it, plus the chunk of code can contain pretty much anything.
    * And this does not provide the full power of a full functional approach. Our only option is just running it.
    * So there is difficulty in testing it. How are we going to test it?
    * There are also some reasoning problems, because the types that can be passed in the unsafeRun can be pretty much anything.
    * Thus an IO[Unit] can describe any scala code, and thus in order to see what it does we need to look at the implementation.
    * So a solution is to abstract over the IO -> meaning that we need to abstract stuff that "looks" like IO, but which are not IO.
    * Thus we can be more precise when describing the effects.
    * I don't want to get into this now as this advanced topic and the implementation depends a lot on the programming language used.
    * For example in Scala you would need stuff like higher kinded types, which are not present in Java.
    */
  FuncGuessing.main().unsafeRun()
}

