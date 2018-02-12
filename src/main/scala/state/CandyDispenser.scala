package state

case class CandyDispenser(locked: Boolean, candies: Int, coins: Int)

object CandyDispenser {
  type State[S, +A] = S => (A, S)
  type Dispense = State[CandyDispenser, (Int, Int)]
  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input

  def idle: Dispense = (machine: CandyDispenser) => ((machine.coins, machine.candies), machine.copy())

  def insert: Dispense = (machine: CandyDispenser) =>
    if (machine.candies == 0) idle(machine)
    else if (!machine.locked) idle(machine)
    else ((machine.coins + 1, machine.candies), machine.copy(false, coins = machine.coins + 1))

  def turn: Dispense = (machine: CandyDispenser) =>
    if (machine.candies == 0) idle(machine)
    else if (!machine.locked) ((machine.coins, machine.candies - 1), machine.copy(true, machine.candies - 1))
    else idle(machine)

  def simulateMachine(inputs: List[Input]): Dispense = inputs.foldLeft(idle) { (state, cur) =>
    cur match {
      case Coin => (machine: CandyDispenser) => insert(state(machine)._2)
      case Turn => (machine: CandyDispenser) => turn(state(machine)._2)
    }
  }
}

