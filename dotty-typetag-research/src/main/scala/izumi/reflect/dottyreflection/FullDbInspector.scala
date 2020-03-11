package izumi.reflect.dottyreflection

import izumi.reflect.internal.fundamentals.collections.IzCollections.toRich
import izumi.reflect.macrortti.LightTypeTagRef._

import scala.quoted._


abstract class FullDbInspector(protected val shift: Int) extends InspectorBase {
  self =>

  // @formatter:off
  import qctx.tasty.{Type => TType, given _, _}
  private lazy val inspector = new Inspector(0) { val qctx: FullDbInspector.this.qctx.type = FullDbInspector.this.qctx }
  // @formatter:on

  private def next() = new FullDbInspector(shift + 1) {
    val qctx: self.qctx.type = self.qctx
  }

  def buildFullDb[T <: AnyKind : Type]: Map[AbstractReference, Set[AbstractReference]] = {
    val tpe = implicitly[Type[T]]
    val uns = tpe.unseal
    inspectTreeToFull(uns)
      .toMultimap
      .map {
        case (t, parents) =>
          t -> parents.filterNot(_ == t)
      }
      .filterNot(_._2.isEmpty)
  }
  private def inspectTTypeToFullBases(tpe2: TType): List[(AbstractReference, AbstractReference)] = {
    val selfRef = inspector.inspectTType(tpe2)

    tpe2 match {
      case a: AppliedType =>
        val rref = inspector.inspectTType(a.tycon)

        next().inspectTTypeToFullBases(a.tycon).map {
          case (c, p) if c == rref =>
            (selfRef, p)
          case o =>
            o
        } //++  a.args.flatMap{x => next().inspectToBToFull(x)}


      case l: TypeLambda =>
        next().inspectTTypeToFullBases(l.resType)

      case a: AndType =>
        ???

      case o: OrType =>
        ???

      case r: TypeRef =>
        next().inspectSymbolToFull(r.typeSymbol)

      case o =>
        log(s"NME TTYPE, UNSUPPORTED: $o")
        List.empty
    }
  }

  private def inspectSymbolToFull(symbol: Symbol): List[(AbstractReference, AbstractReference)] = {
    symbol.tree match {
      case c: ClassDef =>
        val parentSymbols = c.parents.map(_.symbol).filterNot(_.isNoSymbol)
        val trees = c.parents.collect {
          case tt: TypeTree =>
            tt
        }
        val o = trees.flatMap(inspectTreeToFull)
        val selfRef = inspector.inspectSymbol(symbol) //NameReference(c.name)
        val p = trees.flatMap { t => List((selfRef, inspector.inspectTree(t))) }
        p ++ o

      case t: TypeDef =>
        next().inspectTreeToFull(t.rhs.asInstanceOf[TypeTree])
      case o =>
        List.empty
    }
  }

  private def inspectTreeToFull(uns: TypeTree): List[(AbstractReference, AbstractReference)] = {
    val symbol = uns.symbol
    val tpe2 = uns.tpe

    if (symbol.isNoSymbol)
      inspectTTypeToFullBases(tpe2)
    else
      inspectSymbolToFull(symbol)
  }

  private def inspectToBToFull(tpe: TypeOrBounds): List[(AbstractReference, AbstractReference)] = {
    tpe match {
      case t: TypeBounds =>
        ???
      case t: TType =>
        inspectTTypeToFullBases(t)
    }
  }


}

