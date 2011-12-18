package com.scalalabs.sdis

import reflect.NameTransformer


class MethodOut(method: String, clazz: Clazz, out: FormatedPrintStream) {

  implicit val cpool = clazz.cpool

  def disassemble() {
  }


  def formatMethods(out: FormatedPrintStream) {
    clazz.methods.filter( m => findName(m) == method).map(m => formatAMethod(m) )
  }



  def formatAMethod(m: MethodInfo) {
    out.print(methodName(m))
    out.println(methodAttributes(m))
    out.cmd(".end method")
    //    out.println( mkAttribString )
  }

  def methodAttributes(m: MethodInfo): String = {
    //    println("\n>>methodAttributes " + m.attributes )
    m.attributes.foldLeft("") {
      (s, attrib) => s + methodAttrib(attrib, m)
    }
  }

  def methodAttrib(attrib: AttributesInfo, m: MethodInfo): String = {
    //    println("\n>>methodAttrib " + attrib )
    attrib match {
      case c: CodeAttribute => JasminAttributeFormat.formatCode(c, clazz.cpool, m.attributes)
      case l: LineNumberTableAttribute => "\n" + JasminAttributeFormat.formatLineNumber(l)
      case i: SignatureAttribute => "" //JasminAttributeFormat.formatSignature(s, clazz.cpool)
      case l: LocalVariableTableAttribute => "\n.locals" + l
      case r: RuntimeVisibleAnnotationsAttribute => "\n.runtimeVisibleAnnotations " + r
      case _: DeprecatedAttribute => ""
      case x => "\n//Unknown " + x

    }
  }

  
  def findName(m: MethodInfo):String = {
    clazz.cpool.at(m.name_index) match {
      case ConstantUtf8(_, s) => s
      case _ => "unknown"
    }
  }

  def methodName(m: MethodInfo): String = {
    def findParams: String = clazz.cpool.at(m.descriptor_index) match {
      case ConstantUtf8(_, s) => s
      case _ => "unknown"
    }

    def mthdNm = findName(m)

    "\n; " + NameTransformer.decode(findName(m)) + "\n" +
      ".method " + AttribFlags.mkString(m.access_flags) + mthdNm + " " + findParams +
      isDeprecated(m.attributes) +
      isSignature(m.attributes)
  }

  def isSignature(attrib: List[AttributesInfo]): String = {
    attrib.foldLeft("")((s, x) => x match {
      case SignatureAttribute(signature_index) => s + "\n.signature \"" + cpool.at(signature_index) + "\""
      case _ => s
    })
  }

  def isDeprecated(attrib: List[AttributesInfo]): String = {
    attrib.foldLeft("")((s, x) => x match {
      case _: DeprecatedAttribute => s + "\n.deprecated"
      case _ => s
    })
  }


}