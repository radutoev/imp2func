import scala.io.StdIn
import scala.util.{Random, Try}

object ImpCoinFlip extends App {
  def main(): Unit = {
    println("What is your name?")

    val name = StdIn.readLine()

    println(s"Hello, $name, welcome to the game.")

    var exec: Boolean = true
    var correctGuesses: Int = 0
    var totalGuesses: Int = 0

    while(exec) {
      val num = Random.nextInt(5) + 1
      println(s"$name, please guess a number between 1 and 5:")

      val maybeGuess: Option[Int] = Try { StdIn.readLine().toInt } toOption

      if(maybeGuess.isDefined) {
        if(maybeGuess.get == num) {
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

      StdIn.readLine() match {
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

  main()
}

object FuncCoinFlip extends App {

}
