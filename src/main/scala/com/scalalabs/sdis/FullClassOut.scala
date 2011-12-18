package com.scalalabs.sdis

import scala.reflect.NameTransformer

//import java.io.PrintStream

//import scala.collection.mutable.Map

/**
 * Output in jasmin compatible format
 * @see http://jasmin.sourceforge.net/xt.html
 */
class FullClassOut(clazz: Clazz, out: FormatedPrintStream) {

  implicit val cpool = clazz.cpool

  def disassemble() {
    if (Preference.displayHeader) {
      out.cmd(".magic " + "0x" + clazz.magic.toHexString)
      out.cmd(".bytecode " + clazz.major_version + "." + clazz.minor_version)
      out.cmd(scalaSigName())
      sourceFile()
      out.print(clazzName())
      out.print(superName())
    }

    out.print(interfacesList)
    out.print(innerClazzList())
    if (Preference.displayConstPool) out.print(constantPoolList)
    mkFieldsString(out)
    formatMethods(out)
    out.print(mkClassAttribString)
    out.println("\n")
  }

  def interfacesList: String = {
    clazz.interfaces.foldLeft("") {
      (s, idx) =>
        s + ".implements " + interfaceName(idx).mkString + "\n"
    }
  }

  def innerClazzList(): String = {
    clazz.attributes.foldLeft("") {
      (s, v) =>
        s + (v match {
          case x: InnerClassesAttribute => JasminAttributeFormat.formatInnerClasses(x, cpool)
          case _ => ""
        })
    }
  }

  def mkFieldsString(out: FormatedPrintStream) {
    def cpFindString(idx: Int): String = clazz.cpool.at(idx) match {
      case ConstantUtf8(_, s) => s
      case _ => "unknown"
    }

    clazz.fields.foreach(f => {
      out.cmd(".field " + AttribFlags.mkString(f.access_flags) + cpFindString(f.name_index) + "" + cpFindString(f.descriptor_index))
      f.attributes.foreach(a => {
        a match {
          case v: ConstantValueAttribute => out.println("  .unknown " + v)
          case v: SyntheticAttribute => out.println("  .unknown " + v)
          case SignatureAttribute(signature_index) => out.println("  .signature \"" + cpool.at(signature_index) + "\"")
          case v: DeprecatedAttribute => out.println("  .deprecated ")
          case v: RuntimeVisibleAnnotationsAttribute => out.println("  .unknown " + v)
          case v: RuntimeInvisibleAnnotationsAttribute => out.println("  .unknown " + v)
          case v => out.println("  .unknown " + v)
        }

      })
      if (f.attributes.length > 0) {
        out.cmd(".end field")
      }
    })
  }

  def constantPoolList: String = {
    "\n\n//constant pool\n" +
      ".const_pool " + clazz.cpool.lst().zipWithIndex.map(x => ("" + (x._2 + 1) + " " + validCharsOnly(x._1.mkString))).mkString("\n.const_pool ") + "\n"
  }

  def validCharsOnly(s: String): String = {
    s.map(x => if (x.isLetterOrDigit) x else '.')
  }


  def formatMethods(out: FormatedPrintStream) {
    clazz.methods.map(m => formatAMethod(m)) //  _.mkString(c.cpool))
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

  //  def findLineNumbersAttrib(m: MethodInfo):LineNumberTableAttribute = {
  //    m.attributes.foldLeft(List[LineNumberTableAttribute]()) { (s, attrib) => {
  //    	attrib match {
  //    		case l: LineNumberTableAttribute => l :: s
  //    		case _ => s
  //    	}
  //    }
  //    }.head
  //    }

  def mkClassAttribString: String = {
    clazz.attributes.foldLeft("") {
      (s, v) =>
        s + (v match {
          case x: EnclosingMethodAttribute => JasminAttributeFormat.formatEnclosingMethod(x, cpool) + "\n"
          case x: SyntheticAttribute => ".synthetic" + "\n"
          //         case x:SignatureAttribute =>
          case x: SourceDebugExtensionAttribute => JasminAttributeFormat.formatSourceDebugExtension(x) + "\n"
          //         Deprecated
          case x: RuntimeInvisibleAnnotationsAttribute =>

          case _: SourceFileAttribute => "" // nothing
          case _: ScalaSigAttribute => "" // nothing
          case ScalaAttribute(b) => ".scala length " + b.length + " " + b.mkString(", ") + "\n" // nothing
          case RuntimeVisibleAnnotationsAttribute(x) => //"\n.runtimeVisibleAnnotations "  + (x.toList.mkString(","))
            x.foldLeft("\n") {
              (s1, z) => s1 + mkRuntimeVisibleAnnotationsAttributeString(z)
            } + "\n"
          case x: InnerClassesAttribute => "" //JasminAttributeFormat.formatInnerClasses(x, cpool)
          case x => "\n//Unknown " + x + " " + v + "\n"
        })
    }
  }


  def mkRuntimeVisibleAnnotationsAttributeString(item: RVAnnonItem): String = {
    val name = cpool.at(item.type_index)
    name match {
      case ConstantUtf8(_, "Lscala/reflect/ScalaSignature;") =>
        ".scalaSignature " +
          item.element_value_pairs.foldLeft(" ") {
            (s, v) =>

              val value = v.element_value_pairs.map(x => x match {
                case c: ConstElementValue =>
                  cpool.at(c.const_value_index) match {
                    case ConstantUtf8(_, txt) =>
                      scala.reflect.generic.ByteCodecs.decode(txt.getBytes)
                      txt.getBytes.map(x => x.toInt.toHexString).mkString(",")
                    //                scala.reflect.generic.ByteCodecs.decode( txt1 )
                    //                txt1.foldLeft("") { (a,b) => a + "," + (b.toInt & 0xff).toHexString + "(" + b.toChar + ")" }

                    case _ =>
                  }
                case e => "other: " + e
              })

              s + cpool.at(v.element_name_index) + " " + value
          }
      case ConstantUtf8(_, n) => ".runtimeVisibleAnnotations " + n
      case _ => ".runtimeVisibleAnnotations " + name
    }
  }

  def sourceFile() {
    def findTxt(idx: Int) = {
      ".source '" + cpConstantUtf8(idx).getOrElse("MISSING: cpool:" + idx) + "'"
    }

    clazz.attributes.map(x => x match {
      case SourceFileAttribute(i) => out.cmd(findTxt(i))
      case _ =>
    })
  }

  def clazzName() = {
    ".class '" + cpClazzName(clazz.this_class).getOrElse("MISSING: cpool:" + clazz.this_class) + "'\n"
  }

  def superName(): String = {
    ".super '" + cpClazzName(clazz.super_class).getOrElse("MISSING: cpool:" + clazz.super_class) + "'\n"
  }

  def cpClazzName(index: Int): Option[String] = {
    clazz.cpool.at(index) match {
      case ConstantClazz(idx) => cpConstantUtf8(idx)
      case _ => None
    }
  }

  def cpConstantUtf8(idx: Int): Option[String] = {
    cpool.at(idx) match {
      case ConstantUtf8(_, txt) => Some(txt)
      case _ => None
    }
  }

  def methodName(m: MethodInfo): String = {
    def findParams: String = clazz.cpool.at(m.descriptor_index) match {
      case ConstantUtf8(_, s) => s
      case _ => "unknown"
    }

    def findName = clazz.cpool.at(m.name_index) match {
      case ConstantUtf8(_, s) => s
      case _ => "unknown"
    }

    "\n; " + NameTransformer.decode(findName) + "\n" +
      ".method " + AttribFlags.mkString(m.access_flags) + findName + " " + findParams +
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

  def scalaSigName(): String = {
    clazz.attributes.foldLeft("")((s, v) => v match {
      case ScalaSigAttribute(major, minor, entries) => s + ".scalasig " + major + "." + minor + "  " + entries + "\n"
      case x => s
    })
  }

  def interfaceName(index: Int): Option[String] = {
    Some((clazz.cpool.at(index) match {
      case ConstantClazz(idx) => clazz.cpool.at(idx) match {
        case ConstantUtf8(_, txt) => "'" + txt + "'"
        case _ => "" + (clazz.this_class)
      }
      case _ => "" + (clazz.this_class
        )

    }).mkString(""))
  }

}