// package izreflect.fundamentals.dottyreflection

// import scala.quoted._
// import scala.quoted.matching._

// object LightTypeTagRefPkg:
//   sealed trait LightTypeTagRef
//   sealed trait AbstractReference extends LightTypeTagRef

//   final case class Lambda(input: List[LambdaParameter], output: AbstractReference) extends AbstractReference


//   final case class LambdaParameter(name: String)

//   sealed trait AppliedReference extends AbstractReference

//   sealed trait AppliedNamedReference extends AppliedReference:
//     def asName: NameReference

//   final case class IntersectionReference (refs: Set[AppliedReference]) extends AppliedReference
//   final case class UnionReference(input: Set[AppliedReference]) extends AppliedReference

//   final case class NameReference(ref: SymName, boundaries: Boundaries = Boundaries.Empty, prefix: Option[AppliedReference] = None) extends AppliedNamedReference:
//     override def asName: NameReference = this

//   object NameReference:
//     def apply(tpeName: String): NameReference = NameReference(SymName.SymTypeName(tpeName))

//   final case class FullReference(ref: String, parameters: List[TypeParam], prefix: Option[AppliedReference] = None) extends AppliedNamedReference:
//     override def asName: NameReference = NameReference(SymName.SymTypeName(ref), prefix = prefix)

//   final case class TypeParam(ref: AbstractReference, variance: Variance)

//   sealed trait RefinementDecl

//   object RefinementDecl:
//     final case class Signature(name: String, input: List[AppliedReference], output: AppliedReference) extends RefinementDecl
//     final case class TypeMember(name: String, ref: AbstractReference) extends RefinementDecl

//   final case class Refinement(reference: AppliedReference, decls: Set[RefinementDecl]) extends AppliedReference

//   sealed trait Variance

//   object Variance:
//     case object Invariant extends Variance
//     case object Contravariant extends Variance
//     case object Covariant extends Variance


//   sealed trait Boundaries

//   object Boundaries:
//     final case class Defined(bottom: AbstractReference, top: AbstractReference) extends Boundaries
//     case object Empty extends Boundaries

//   sealed trait SymName:
//     def name: String

//   object SymName:
//     final case class SymTermName(name: String) extends SymName
//     final case class SymTypeName(name: String) extends SymName
//     final case class SymLiteral(name: String) extends SymName
//     object SymLiteral:
//       def apply(c: Any): SymLiteral =
//         val constant = c match
//           case s: String => "\"" + s + "\""
//           case o => o.toString
//         SymLiteral(constant)

//   // given liftLambdaParameter(given QuoteContext) as Liftable[LambdaParameter]:
//   //   def toExpr(x: LambdaParameter) = '{LambdaParameter(${Expr(x.name)})}

//   // given liftAppliedNamedReference(given QuoteContext) as Liftable[NameReference]:
//   //   def toExpr(x: NameReference) = '{NameReference(${Expr(x.name)})}

//   // def liftLambdaParameter(lp: LambdaParameter)(given QuoteContext): Expr[LambdaParameter] =
//     // '{LambdaParameter(${Expr(lp.name)})}
//   // def liftList[T](t: T)()

//   // def liftContext(ar: AbstractReference)(given QuoteContext): Expr[AbstractReference] =
//     // ar match
//       // case Lambda(parameters, output) => '{ ${parameters.map{liftLambdaParameter}.foldLeft(Expr)} +  }







