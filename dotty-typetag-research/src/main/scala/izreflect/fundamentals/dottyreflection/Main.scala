package izreflect.fundamentals.dottyreflection

object Main {

  trait Y
  trait Z extends Y
  trait X[+A, -B <: Y] {}

  def main(args: Array[String]): Unit = {
    class Foo[+F[_]]()
    class Bar[+F[+_, +_]]() extends Foo[F[Int, *]]
    class Baz() extends X[String, Z]

    Inspect.inspect[Baz]
    Inspect.inspect[Bar[X]]
  }

}
