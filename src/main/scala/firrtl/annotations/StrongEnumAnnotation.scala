package firrtl.annotations
import firrtl.AnnotationSeq

/** An annotation for enum types (rather than enum ''instances'').
  *
  * @param typeName the name of the enum's type (e.g. ''"mypackage.MyEnum"'')
  * @param definition a map describing which integer values correspond to which enum names
  */
case class EnumDefAnnotation(typeName: String, definition: Map[String, BigInt]) extends NoTargetAnnotation

/** An annotation for strong enum instances that are ''not'' inside of Vecs
  *
  * @param target the enum instance being annotated
  * @param enumTypeName the name of the enum's type (e.g. ''"mypackage.MyEnum"'')
  */
case class EnumComponentAnnotation(target: Named, enumTypeName: String) extends SingleTargetAnnotation[Named] {
  def duplicate(n: Named): EnumComponentAnnotation = this.copy(target = n)
}

/** An annotation for Vecs of strong enums.
  *
  * The ''fields'' parameter deserves special attention, since it may be difficult to understand. Suppose you create a the following Vec:
  *
  *               {{{
  *               VecInit(new Bundle {
  *                 val e = MyEnum()
  *                 val b = new Bundle {
  *                   val inner_e = MyEnum()
  *                 }
  *                 val v = Vec(3, MyEnum())
  *               }
  *               }}}
  *
  *               Then, the ''fields'' parameter will be: ''Seq(Seq("e"), Seq("b", "inner_e"), Seq("v"))''. Note that for any Vec that doesn't contain Bundles, this field will simply be an empty Seq.
  *
  * @param target the Vec being annotated
  * @param typeName the name of the enum's type (e.g. ''"mypackage.MyEnum"'')
  * @param fields a list of all chains of elements leading from the Vec instance to its inner enum fields.
  */
case class EnumVecAnnotation(target: Named, typeName: String, fields: Seq[Seq[String]])
    extends SingleTargetAnnotation[Named] {
  def duplicate(n: Named): EnumVecAnnotation = this.copy(target = n)
}

object StrongEnumAnnotation {
  def getEnumComponents(annos: AnnotationSeq): Seq[(CompleteTarget, Map[String, BigInt])] = {
    val enumDefMap = annos.collect {
      case EnumDefAnnotation(typeName: String, definition: Map[String, BigInt]) => typeName -> definition
    }.toMap
    annos.collect {
      case EnumComponentAnnotation(target: Named, enumTypeName: String) => target.toTarget -> enumDefMap(enumTypeName)
    }
  }

  def getEnumVec(annos: AnnotationSeq): Seq[((CompleteTarget, Seq[Seq[String]]), Map[String, BigInt])] = {
    val enumDefMap = annos.collect {
      case EnumDefAnnotation(typeName: String, definition: Map[String, BigInt]) => typeName -> definition
    }.toMap
    annos.collect {
      case EnumVecAnnotation(target: Named, typeName: String, fields: Seq[Seq[String]]) => (target.toTarget, fields) -> enumDefMap(typeName)
    }
  }
}
