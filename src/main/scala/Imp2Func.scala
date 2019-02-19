import scala.io.StdIn
import scala.util.Random

object ImpCoinFlip extends App {
  def main(): Unit = {
    println("What is your name?")

    val name = StdIn.readLine()

    println(s"Hello, $name, welcome to the game")

    var exec = true
    while(exec) {
      val num = Random.nextInt(5) + 1
      println(s"$name, please guess a number between 1 to 5:")

      val guess = StdIn.readLine().toInt

      if(guess == num) println("You guessed right")
      else println(s"You guessed wrong, the number was $num")

      println("Do you want to continue?")

      StdIn.readLine() match {
        case "y" => exec = true
        case "n" => exec = false
        case _   => exec = false
      }
    }
  }

  main()
}

object FuncCoinFlip extends App {

}
