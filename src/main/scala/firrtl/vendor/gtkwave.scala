package firrtl.vendor
import firrtl.AnnotationSeq
import firrtl.annotations.{EnumComponentAnnotation, EnumDefAnnotation, EnumVecAnnotation, Named, StrongEnumAnnotation}

object gtkwave {
  def enumScript(annos: AnnotationSeq): String = {
    val enumComponents = StrongEnumAnnotation.getEnumComponents(annos)
    val enumVec = StrongEnumAnnotation.getEnumVec(annos)
    "// todo gtkwave scripts. "
  }
}
