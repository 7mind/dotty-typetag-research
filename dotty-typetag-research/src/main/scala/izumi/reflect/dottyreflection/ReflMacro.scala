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

object Inspect {
  inline def inspect[T]: LightTypeTag = ${ typetagMacroImpl[T] }

  def typetagMacroImpl[T: Type](given qctx: QuoteContext): Expr[LightTypeTag] = {
    println("BEFORE")
    val ref = InspectMacro.apply[T]
    val dbs = InspectMacro.nameRefs[T]

    @inline def serialize[A: Pickler](a: A): String = {
      val bytes = PickleImpl(a).toByteBuffer.array()
      new String(bytes, 0, bytes.length, StandardCharsets.ISO_8859_1)
    }
    val strRef = serialize(ref)(LightTypeTag.lttRefSerializer)
    val strDbs = serialize(dbs)(LightTypeTag.subtypeDBsSerializer)

    def string2hex(str: String): String = {
        str.toList.map(_.toInt.toHexString).mkString
    }
    println(s"$ref => ${strRef.size} bytes, ${string2hex(strRef)}")
    println(s"$dbs => ${strDbs.size} bytes, ${string2hex(strDbs)}")
    println("AFTER")
    //prinltn(strDBs)
    '{ LightTypeTag.parse(${Expr(ref.hashCode())}, ${Expr(strRef)}, ${Expr(strDbs)}, 0) }
  }
}

abstract class Inspector(shift: Int) { self =>
  val qctx: QuoteContext
  given as qctx.type = qctx
  import qctx.tasty.{Type => TType, given _, _}

  def buildDbs[T](tpe: Type[T]): SubtypeDBs = {
      val uns = tpe.unseal
      val v = inspectTreeToName(uns)
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

      val f = inspectTreeToFull(uns)
        .toMultimap
        .map {
          case (t, parents) =>
            t -> parents.filterNot(_ == t)
        }
        .filterNot(_._2.isEmpty)
      println(s"DB0: ${v}")
      println(s"DB1: ${f}")

      // println("--------")
      SubtypeDBs(f, v)
  }

  def inspect[T](tpe: Type[T]): AbstractReference = {
      val uns = tpe.unseal
      println(s" -------- about to inspect ${tpe} --------")
      val v = inspectTree(uns)
      println(s" -------- done inspecting ${tpe} --------")
      v
  }

  private def logStart(s: String) = println(" " * shift + s)
  private def log(s: String) = println(" " * shift + " -> " + s)
  private def next() = new Inspector(shift + 1) { val qctx: self.qctx.type = self.qctx }

  private def inspectTTypeToNameBases(tpe2: TType): List[(NameReference, NameReference)] = {
    tpe2 match {
      case a: AppliedType =>
        //println(s"XXX: ${a.tycon}  ${a.tycon.getClass} => ${ next().inspectTTypeToNameBases(a.tycon)}")
        next().inspectTTypeToNameBases(a.tycon) ++  a.args.flatMap{x => next().inspectToBToName(x)}


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

        val p = trees.flatMap {t => inspectTree(t) match {
          case n: NameReference =>
            List((selfRef, n))
          case n: FullReference =>
            List((selfRef, n.asName))
          case _ =>
            List.empty
        }}

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
    val selfRef = inspectTType(tpe2)

    tpe2 match {
      case a: AppliedType =>
        val rref = inspectTType(a.tycon)

        next().inspectTTypeToFullBases(a.tycon).map {
          case (c, p) if c == rref =>
            (selfRef, p)
          case o =>
            o
        } ++  a.args.flatMap{x => next().inspectToBToFull(x)}


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
        val selfRef = inspectSymbol(symbol) //NameReference(c.name)
        val p = trees.flatMap {t => List((selfRef, inspectTree(t)))}
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
///

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

      case tb: TypeBounds => // weird thingy
        next().inspectTType(tb.hi)
      case o =>
        log(s"TTYPE, UNSUPPORTED: $o")
        ???
    }
  }

  private def inspectTree(uns: TypeTree): AbstractReference = {
      val symbol = uns.symbol
      val tpe2 = uns.tpe
      logStart(s"INSPECT: $uns: ${uns.getClass}")
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

  private def tpeAttrs[T](uns: TypeTree): Unit = {
      val symbol = uns.symbol
      println(s"Attrs[$uns]: type=${symbol.isType}, term=${symbol.isTerm}, packageDef=${symbol.isPackageDef}, classDef=${symbol.isClassDef}, typeDef=${symbol.isValDef}, defdef=${symbol.isDefDef}, bind=${symbol.isBind}, nosymbol=${symbol.isNoSymbol}")
  }
}

object InspectMacro {
  def apply[T](given qctx0: QuoteContext, tpe: Type[T]): AbstractReference = new Inspector(0) { val qctx = qctx0 }.inspect(tpe)
  def nameRefs[T](given qctx0: QuoteContext, tpe: Type[T]): SubtypeDBs = new Inspector(0) { val qctx = qctx0 }.buildDbs(tpe)
}
