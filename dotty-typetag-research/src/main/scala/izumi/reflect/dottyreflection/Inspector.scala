package izumi.reflect.dottyreflection

import scala.deriving._
import scala.quoted._
import scala.quoted.matching._
import scala.compiletime.{erasedValue, summonFrom, constValue}
import izumi.reflect.macrortti.LightTypeTagRef
import izumi.reflect.macrortti.LightTypeTag
import izumi.reflect.macrortti.LightTypeTag.ParsedLightTypeTag.SubtypeDBs
import izumi.reflect.macrortti.LightTypeTagRef._
import reflect.Selectable.reflectiveSelectable
import java.nio.charset.StandardCharsets
import izumi.reflect.thirdparty.internal.boopickle.NoMacro.Pickler
import izumi.reflect.thirdparty.internal.boopickle.PickleImpl
import izumi.reflect.internal.fundamentals.collections.IzCollections.toRich



abstract class Inspector(protected val shift: Int) extends InspectorBase {
  self =>

  // @formatter:off
  import qctx.tasty.{Type => TType, given _, _}
  // @formatter:on

  private def next() = new Inspector(shift + 1) {
    val qctx: self.qctx.type = self.qctx
  }

  def buildTypeRef[T <: AnyKind : Type]: AbstractReference = {
    val tpe = implicitly[Type[T]]
    val uns = tpe.unseal
    log(s" -------- about to inspect ${tpe} --------")
    val v = inspectTree(uns)
    log(s" -------- done inspecting ${tpe} --------")
    v
  }

  private[dottyreflection] def inspectTType(tpe2: TType): AbstractReference = {
    tpe2 match {
      case a: AppliedType =>
        log(s"APPLIED: ${a.tycon};; ${a.args}")
        a.args match {
          case Nil =>
            NameReference(extractName(a.tycon))
          case o =>
            val args = a.args.map { x => next().inspectToB(x) }
            FullReference(extractName(a.tycon), args)
        }

      //next().inspectSymbol(a.tycon.typeSymbol)

      case l: TypeLambda =>
        log(s"LAMBDA: ${l.paramNames} ${l.resType}")
        val resType = next().inspectTType(l.resType)
        val paramNames = l.paramNames.map {
          LambdaParameter(_)
        }
        LightTypeTagRef.Lambda(paramNames, resType)

      case t: ParamRef =>
        log(s"PARAM REF: $t ")
        NameReference(extractName(t))
      case a: AndType =>
        log(s"AND: rhs[${a.left}], lhs[${a.right}]")
        IntersectionReference(flattenInspectAnd(a))

      case o: OrType =>
        log(s"OR: rhs[${o.left}], lhs[${o.right}]")
        UnionReference(flattenInspectOr(o))

      case r: TypeRef =>
        //log(s"TYPEREF: ${r.name} ${r.translucentSuperType}")
        log(s"TYPEREF: ${r.name} ${r.typeSymbol}")
        next().inspectSymbol(r.typeSymbol)

      case tb: TypeBounds => // weird thingy
        //NameReference("WTF")
        next().inspectTType(tb.hi)
      case o =>
        log(s"TTYPE, UNSUPPORTED: $o")
        NameReference("???")
      //???

    }
  }

  private[dottyreflection] def inspectTree(uns: TypeTree): AbstractReference = {
    val symbol = uns.symbol
    val tpe2 = uns.tpe
    logStart(s"INSPECT: $uns: ${uns.getClass}")
    if (symbol.isNoSymbol)
      inspectTType(tpe2)
    else
      inspectSymbol(symbol)
  }

  private[dottyreflection] def inspectSymbol(symbol: Symbol): AbstractReference = {
    log(s"SYMTPE: ${symbol}; covariant:${symbol.flags.is(Flags.Covariant)}")
    symbol.tree match {
      case c: ClassDef =>
        log(s"CLASSDEF, parents: ${c.parents}")
        NameReference(c.name)
      case t: TypeDef =>
        log(s"TYPEDEF: $t")
        next().inspectTree(t.rhs.asInstanceOf[TypeTree])
      case o =>
        log(s"SYMBOL, UNSUPPORTED: $o")
        ???
    }
  }

  private def inspectToB(tpe: TypeOrBounds): TypeParam = {
    tpe match {
      case t: TypeBounds =>
        ???
      case t: TType =>
        TypeParam(inspectTType(t), Variance.Invariant)
    }
  }

  private def flattenInspectAnd(and: AndType): Set[AppliedReference] = {
    val (andTypes, otherTypes) =
      and match {
        case AndType(l@AndType(_, _), r@AndType(_, _)) =>
          (Set(l, r), Set.empty[TType])
        case AndType(l@AndType(_, _), r) =>
          (Set(l), Set(r))
        case AndType(l, r@AndType(_, _)) =>
          (Set(r), Set(l))
        case AndType(l, r) =>
          (Set.empty[AndType], Set(l, r))
      }
    val andTypeTags = andTypes flatMap flattenInspectAnd
    val otherTypeTags = otherTypes map inspectTType map {
      _.asInstanceOf[AppliedReference]
    }
    andTypeTags ++ otherTypeTags
  }

  private def flattenInspectOr(or: OrType): Set[AppliedReference] = {
    val (orTypes, otherTypes) =
      or match {
        case OrType(l@OrType(_, _), r@OrType(_, _)) =>
          (Set(l, r), Set.empty[TType])
        case OrType(l@OrType(_, _), r) =>
          (Set(l), Set(r))
        case OrType(l, r@OrType(_, _)) =>
          (Set(r), Set(l))
        case OrType(l, r) =>
          (Set.empty[OrType], Set(l, r))
      }
    val orTypeTags = orTypes flatMap flattenInspectOr
    val otherTypeTags = otherTypes map inspectTType map {
      _.asInstanceOf[AppliedReference]
    }
    orTypeTags ++ otherTypeTags
  }

  private def extractName(t: TType): String = {
    t match {
      case ref: TypeRef =>
        ref.name
      case t: ParamRef =>
        t.binder.asInstanceOf[ {def paramNames: List[Object]}].paramNames(t.paramNum).toString
    }
  }
}

