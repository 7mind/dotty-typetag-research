package izumi.reflect.dottyreflection

import izumi.reflect.internal.fundamentals.collections.IzCollections.toRich
import izumi.reflect.macrortti.LightTypeTag.ParsedLightTypeTag.SubtypeDBs
import izumi.reflect.macrortti.LightTypeTagRef
import izumi.reflect.macrortti.LightTypeTagRef._

import scala.quoted._


abstract class DbInspector(protected val shift: Int) extends InspectorBase {
  self =>

  // @formatter:off
  import qctx.tasty.{Type => TType, given _, _}
  private lazy val inspector = new Inspector(0) { val qctx: DbInspector.this.qctx.type = DbInspector.this.qctx }
  // @formatter:on

  private def next() = new DbInspector(shift + 1) {
    val qctx: self.qctx.type = self.qctx
  }

  def buildNameDb[T <: AnyKind : Type]: Map[NameReference, Set[NameReference]] = {
    val tpe = implicitly[Type[T]]
    val uns = tpe.unseal
    inspectTreeToName(uns)
      .toMultimap
      .map {
        case (t, parents) =>
          t -> parents
            .collect {
              case r: AppliedNamedReference =>
                r.asName
            }
            .filterNot(_ == t)
      }
      .filterNot(_._2.isEmpty)
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



  private def inspectTTypeToNameBases(tpe2: TType): List[(NameReference, NameReference)] = {
    tpe2 match {
      case a: AppliedType =>
        //println(s"XXX: ${a.tycon}  ${a.tycon.getClass} => ${ next().inspectTTypeToNameBases(a.tycon)}")
        next().inspectTTypeToNameBases(a.tycon) //++  a.args.flatMap{x => next().inspectToBToName(x)}


      case l: TypeLambda =>
        next().inspectTTypeToNameBases(l.resType)

      case a: AndType =>
        ???

      case o: OrType =>
        ???

      case r: TypeRef =>
        next().inspectSymbolToName(r.typeSymbol)

      case o =>
        log(s"NME TTYPE, UNSUPPORTED: $o")
        List.empty
    }
  }

  private def inspectSymbolToName(symbol: Symbol): List[(NameReference, NameReference)] = {
    symbol.tree match {
      case c: ClassDef =>
        val parentSymbols = c.parents.map(_.symbol).filterNot(_.isNoSymbol)
        //val rest = c.parents.map(p => p.symbol).filterNot(_.isNoSymbol)
        //println(s"PP: ${c.name} <--- ${c.parents.map(_.getClass)}")

        val trees = c.parents.collect {
          case tt: TypeTree =>
            tt
        }
        val o = trees.flatMap(inspectTreeToName)
        val selfRef = NameReference(c.name)

        val p = trees.flatMap { t =>
          val tRef = inspector.inspectTree(t) /*new Inspector() {
            override val qctx: QuoteContext = _
          }.inspectTree(t)*/

          tRef match {
            case n: NameReference =>
              List((selfRef, n))
            case n: FullReference =>
              List((selfRef, n.asName))
            case _ =>
              List.empty
          }
        }

        p ++ o

      case t: TypeDef =>
        next().inspectTreeToName(t.rhs.asInstanceOf[TypeTree])
      case o =>
        List.empty
    }
  }

  private def inspectTreeToName(uns: TypeTree): List[(NameReference, NameReference)] = {
    val symbol = uns.symbol
    val tpe2 = uns.tpe

    if (symbol.isNoSymbol)
      inspectTTypeToNameBases(tpe2)
    else
      inspectSymbolToName(symbol)
  }

  private def inspectToBToName(tpe: TypeOrBounds): List[(NameReference, NameReference)] = {
    tpe match {
      case t: TypeBounds =>
        ???
      case t: TType =>
        inspectTTypeToNameBases(t)
    }
  }


  ///
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

