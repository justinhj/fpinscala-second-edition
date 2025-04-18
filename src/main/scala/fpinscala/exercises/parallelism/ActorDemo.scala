package fpinscala.exercises.parallelism

import java.util.concurrent.Executors

object ActorDemo:

  def main(args: Array[String]): Unit =
    val executor = Executors.newFixedThreadPool(2)

    // CandyMachine inputs
    enum Input:
      case InsertCoin
      case TurnCrank

    // Machine state
    case class MachineState(locked: Boolean, candies: Int, coins: Int)

    val actor = new Actor[Input](executor)(
      handler = input => {
        input match
          case Input.InsertCoin =>
            println("Coin inserted")
            MachineState(locked = false, candies = 10, coins = 1)
          case Input.TurnCrank =>
            println("Crank turned")
            MachineState(locked = true, candies = 9, coins = 1)
      },
      onError = ex => println(s"Error occurred: ${ex.getMessage}")
    )

    actor ! Input.InsertCoin
    actor ! Input.TurnCrank

    executor.shutdown()