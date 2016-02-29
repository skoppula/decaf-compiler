package compile.descriptors

abstract class Descriptor {
  val name : String

  override def equals(that: Any) : Boolean = {
    that match {
      case that : Descriptor => this.name == that.toString
      case _ => false
    }
  }

  override def toString: String = {
    this.name
  }
}
