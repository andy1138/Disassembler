package com.scalalabs.sdis

import MethodInfo._

object JasminAttributeFormat {

  def formatCode(c: CodeAttribute, cpool: ConstPool, attrib: List[AttributesInfo]) = {
    val codeBlock = new DisasmCodeBlock(c.code, cpool)

    
    val codeBytes = if( Preference.displayCodeBytes) "\n.code bytes " + c.code.map(x => (x & 0xff).toInt.toHexString).mkString(",")  else ""
    
    val res = codeBlock.mkCode(c.code, c.attribs)

    val att = c.attribs.foldLeft("") {
      (s, v) => {
        val res1 = v match {
          case LocalVariableTableAttribute(l: List[LocalVariableTableItem]) => s + l.foldLeft("")((s1, i) => s1 + "\n.var " + cpool.at(i.name_index) + " " + cpool.at(i.descriptor_index) + "") + ""
          case _ => s
        }
        res1
        //      s + ", " + v}
      }
    }

    codeBytes + att + res
  }

  def formatLineNumber(lineTable: LineNumberTableAttribute) = {
    //    println(">>formatLineNumber called")
    lineTable.lines.foldLeft("") {
      (s, line) =>
        s + "\n.line " + line.lineNo + " pc " + line.startPC
    }
  }


  def formatEnclosingMethod(att: EnclosingMethodAttribute, cpool: ConstPool) = {
    def cpClazzName(index: Int): Option[String] = {
      cpool.at(index) match {
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

    val clazzName = cpClazzName(att.class_index)
    val methodName = cpClazzName(att.method_index)

    ".enclosing " + clazzName + " " + methodName
  }


  def formatSourceDebugExtension(x: SourceDebugExtensionAttribute) = {
    "TODO formatSourceDebugExtension "
  }

  def formatInnerClasses(innClazzList: InnerClassesAttribute, cpool: ConstPool) = {
    innClazzList.innClasses.foldLeft("") {
      (s, innClazz) =>
        s + ".inner " + clazzOrInterface(innClazz.inner_class_access_flags) + " " +
          AttribFlags.mkString(innClazz.inner_class_access_flags) + " " +
          (if (innClazz.inner_name_index > 0) cpool.at(innClazz.inner_name_index) else "") + " " +
          "inner " + (if (innClazz.inner_class_info_index > 0) cpClazzName(innClazz.inner_class_info_index, cpool) else "") + " " +
          "outer " + (if (innClazz.outer_class_info_index > 0) cpClazzName(innClazz.outer_class_info_index, cpool) else "") + " " +
          "\n"

    }
  }

  def cpClazzName(idx: Int, cpool: ConstPool): String = {
    def cpConstantUtf8(i: Int): String = {
      cpool.at(i) match {
        case ConstantUtf8(_, txt) => txt
        case _ => ""
      }
    }


    cpool.at(idx) match {
      case ConstantClazz(i) => cpConstantUtf8(i)
      case _ => ""
    }
  }

  def clazzOrInterface(flag: Int): String = {
    if ((flag & ACC_INTERFACE) > 0) "interface " else "class"
  }


}