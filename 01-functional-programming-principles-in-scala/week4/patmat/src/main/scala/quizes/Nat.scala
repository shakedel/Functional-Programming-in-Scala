import scala.util.Try

abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat = new Succ(this)
  def + (that: Nat): Nat
  def - (that: Nat): Nat
}

object Zero extends Nat {
  override def toString = "."
  override def isZero = true
  override def predecessor = throw new Exception
  override def + (that: Nat) = that
  override def - (that: Nat) = throw new Exception
}

class Succ(n: Nat) extends Nat {
  override def toString = "|"+n
  override def isZero = false
  override def predecessor = n
  override def + (that: Nat) = if (that == Zero) this else successor + that.predecessor
  override def - (that: Nat) = if (that == Zero) this else predecessor - that.predecessor
}

object Main {
  def main(args: Array[String]): Unit = {
    val zero = Zero
    println("Zero: "+zero)
    val three = new Succ(new Succ(new Succ(Zero)))
    println("Three: "+three)
    val five = new Succ(new Succ(three))
    println("Five: "+five)
    println("Three+Five: "+(three+five))
    println("Five+Three: "+(five+three))
    println("Five-Three: "+(five-three))
    println("Three-Five: "+Try(three- five).getOrElse("Exception!"))

  }
}