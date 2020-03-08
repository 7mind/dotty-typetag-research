package izreflect.fundamentals.dottyreflection

import scala.deriving._
import scala.quoted._
import scala.quoted.matching._
import scala.compiletime.{erasedValue, summonFrom, constValue}
import izreflect.fundamentals.reflection.macrortti.LightTypeTagRef
import izreflect.fundamentals.reflection.macrortti.LightTypeTagRef._
import reflect.Selectable.reflectiveSelectable

object Inspect {
  inline def inspect[T]: AbstractReference = ${ labelImpl[T] }
  def labelImpl[T: Type](given qctx: QuoteContext): Expr[AbstractReference] = {
    println(InspectMacro.apply[T])
    Expr(null)
  }
}

abstract class Inspector(shift: Int) { self =>
  val qctx: QuoteContext
  given as qctx.type = qctx
  import qctx.tasty.{Type => TType, given _, _}

  def inspect[T](tpe: Type[T]): AbstractReference = {
      val uns = tpe.unseal
      val v = inspectTree(uns)
      println("--------")
      v
  }

  private def logStart(s: String) = println(" " * shift + s)
  private def log(s: String) = println(" " * shift + " -> " + s)
  private def next() = new Inspector(shift + 1) { val qctx: self.qctx.type = self.qctx }
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
    and match
      case AndType(l @ AndType(_,_), r  @ AndType(_,_)) => (Set(l,r),Set.empty[TType])
      case AndType(l @ AndType(_,_), r) =>  (Set(l),Set(r))
      case AndType(l, r  @ AndType(_,_)) => (Set(r),Set(l))
      case AndType(l, r) => (Set.empty[AndType], Set(l,r))
    val andTypeTags = andTypes flatMap flattenInspectAnd
    val otherTypeTags = otherTypes map inspectTType map {_.asInstanceOf[AppliedReference]}
    andTypeTags ++ otherTypeTags
  }

  private def flattenInspectOr(or: OrType): Set[AppliedReference] = {
    val (orTypes, otherTypes) =
    or match
      case OrType(l @ OrType(_,_), r  @ OrType(_,_)) => (Set(l,r),Set.empty[TType])
      case OrType(l @ OrType(_,_), r) =>  (Set(l),Set(r))
      case OrType(l, r  @ OrType(_,_)) => (Set(r),Set(l))
      case OrType(l, r) => (Set.empty[OrType], Set(l,r))
    val orTypeTags = orTypes flatMap flattenInspectOr
    val otherTypeTags = otherTypes map inspectTType map {_.asInstanceOf[AppliedReference]}
    orTypeTags ++ otherTypeTags
  }

  private def extractName(t: TType): String = {
    t match {
      case ref: TypeRef =>
        ref.name
      case t: ParamRef =>
        t.binder.asInstanceOf[{def paramNames: List[Object]}].paramNames(t.paramNum).toString
    }
  }

  private def inspectTType(tpe2: TType): AbstractReference = {
    tpe2 match {
      case a: AppliedType =>
        log(s"APPLIED: ${a.tycon};; ${a.args}")
        a.args match {
          case Nil =>
            NameReference(extractName(a.tycon))
          case o =>
            val args = a.args.map{x => next().inspectToB(x)}
            FullReference(extractName(a.tycon), args)
        }

        //next().inspectSymbol(a.tycon.typeSymbol)

      case l: TypeLambda =>
        log(s"LAMBDA: ${l.paramNames} ${l.resType}")
        val resType = next().inspectTType(l.resType)
        val paramNames = l.paramNames.map{LambdaParameter(_)}
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

      case o =>
        log(s"TTYPE, UNSUPPORTED: $o")
        ???
    }
  }

  private def inspectTree(uns: TypeTree): AbstractReference = {
      val symbol = uns.symbol
      val tpe2 = uns.tpe
      logStart(s"INSPECT: $uns: ${uns.getClass}")
      // println(s"TPE: ${tpe}: ${tpe.getClass}\n  * UNSEALED: ${uns}: ${uns.getClass}")
      // println(s"SYMBOL: ${symbol}: ${symbol.getClass}")
      // println(s"SYMBOL: ${symbol.show}")
      // println(s"TREETPE: ${tpe2}: ${tpe2.getClass}")
      // println(s"TREETPE: ${tpe2.show}")
      if (symbol.isNoSymbol)
        inspectTType(tpe2)
      else
        inspectSymbol(symbol)
  }

  private def inspectSymbol(symbol: Symbol): AbstractReference = {
    log(s"SYMTPE: ${symbol}; covariant:${symbol.flags.is(Flags.Covariant)}")
    symbol.tree match {
      case c: ClassDef =>
        log(s"CLASSDEF, parents: ${c.parents}")
        // c.parents.map {
        //   a =>
        //     next().inspectTree(a.asInstanceOf[TypeTree])
        // }
        NameReference(c.name)
      case t: TypeDef =>
        log(s"TYPEDEF: $t")
        next().inspectTree(t.rhs.asInstanceOf[TypeTree])
      case o =>
        log(s"SYMBOL, UNSUPPORTED: $o")
        ???
    }
  }

  private def tpeAttrs[T](uns: TypeTree): Unit = {
      val symbol = uns.symbol
      println(s"Attrs[$uns]: type=${symbol.isType}, term=${symbol.isTerm}, packageDef=${symbol.isPackageDef}, classDef=${symbol.isClassDef}, typeDef=${symbol.isValDef}, defdef=${symbol.isDefDef}, bind=${symbol.isBind}, nosymbol=${symbol.isNoSymbol}")
  }
}

object InspectMacro {
  def apply[T](given qctx0: QuoteContext, tpe: Type[T]): AbstractReference = new Inspector(0) { val qctx = qctx0 }.inspect(tpe)
}
