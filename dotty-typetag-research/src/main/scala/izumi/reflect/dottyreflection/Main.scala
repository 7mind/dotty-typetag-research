package izumi.reflect.dottyreflection

object Main {

  trait Y
  trait Z extends Y
  trait XS[+K]
  trait X[+A, -B <: Y] extends XS[A] {}

  trait A
  trait B extends A

  trait Listoid[+K]

  def main(args: Array[String]): Unit = {
    class Foo[+F[_]]()
    class Bar[+F[+_, +_]]() extends Foo[F[Int, *]]
    class Baz() extends X[String, Z]

    val bazTag = Inspect.inspect[Baz]
    val bazTag2 = Inspect.inspect[Baz]

    val barXTag = Inspect.inspect[Bar[X]]

    println(s"Baz tag: ${bazTag}")
    println(s"Bar[X] tag: ${barXTag}")

    println(barXTag.debug())

    println(assert(bazTag =:= bazTag2))
    println(assert(!(bazTag =:= barXTag)))
    println(assert(bazTag <:< bazTag2))
    println(assert(!(bazTag <:< barXTag)))

    println(assert(Inspect.inspect[B] <:<  Inspect.inspect[A]))

    val listTag = Inspect.inspectK[Listoid]
    val intTag = Inspect.inspect[Int]
    val listIntTag = Inspect.inspect[Listoid[Int]]

    println(assert(listTag.combine(intTag) <:< listIntTag))

    println("DONE")
  }

}
