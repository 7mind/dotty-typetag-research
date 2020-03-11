package izumi.reflect.dottyreflection

object Main {

  trait Y
  trait Z extends Y
  trait XS[+K]
  trait X[+A, -B <: Y] extends XS[A] {}

  def main(args: Array[String]): Unit = {
    class Foo[+F[_]]()
    class Bar[+F[+_, +_]]() extends Foo[F[Int, *]]
    class Baz() extends X[String, Z]

    val bazTag = Inspect.inspect[Baz]
    val bazTag2 = Inspect.inspect[Baz]

    val barXTag = Inspect.inspect[Bar[X]]

    println(s"Baz tag: ${bazTag}")
    println(s"Bar[X] tag: ${barXTag}")

    println(assert(bazTag =:= bazTag2))
    println(assert(!(bazTag =:= barXTag)))
    println(assert(bazTag <:< bazTag2))
    println(assert(!(bazTag <:< barXTag)))

    println("DONE")
  }

}
