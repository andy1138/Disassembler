package com.scalalabs.sdis

class ConstPool() {

  def this(p: List[ConstantType]) {
    this ()
    p.foreach(pool.append(_))
  }

  def this(p: ConstantType*) {
    this()
    p.foreach(pool.append(_))
  }

  private val pool = scala.collection.mutable.ListBuffer[ConstantType]()

  def append(value: ConstantType) {
    pool += value
  }

  def at(idx: Int): ConstantType = pool(idx - 1)

  def lst() = pool.toList


}


sealed abstract class ConstantType(val tag: Int) {
  def mkString(implicit cpool: ConstPool): String = {
    "ConstantType[" + tag + "]"
  }
}


case class ConstantClazz(nameIdx: Int) extends ConstantType(7) {
  override def mkString(implicit cpool: ConstPool): String = {
    cpool.at(nameIdx).mkString
  }
}

case class ConstantFieldref(class_index: Int, name_and_type_index: Int) extends ConstantType(9) {
  override def mkString(implicit cpool: ConstPool): String = {
    cpool.at(class_index).mkString + "." + cpool.at(name_and_type_index).mkString
  }
}

case class ConstantMethodref(class_index: Int, name_and_type_index: Int) extends ConstantType(10) {
  override def mkString(implicit cpool: ConstPool): String = {
    cpool.at(class_index).mkString + "." + cpool.at(name_and_type_index).mkString
  }

}

case class ConstantInterfaceMethodref(class_index: Int, name_and_type_index: Int) extends ConstantType(11) {
  override def mkString(implicit cpool: ConstPool): String = {
    cpool.at(class_index).mkString + "." + cpool.at(name_and_type_index).mkString
  }
}

case class ConstantString(string_index: Int) extends ConstantType(8) {
  override def mkString(implicit cpool: ConstPool): String = {
    "'" + cpool.at(string_index).mkString + "'"
  }
}


case class ConstantInteger(i: Int) extends ConstantType(3) {
  override def mkString(implicit cpool: ConstPool): String = i.toString
}


case class ConstantFloat(f: Float) extends ConstantType(4) {
  override def mkString(implicit cpool: ConstPool): String = f.toString
}

case class ConstantLong(l: Long) extends ConstantType(5) {
  override def mkString(implicit cpool: ConstPool): String = l.toString
}

case class ConstantFILL() extends ConstantType(5) {
  override def mkString(implicit cpool: ConstPool): String = "FILL"
}

case class ConstantDouble(d: Double) extends ConstantType(6) {
  override def mkString(implicit cpool: ConstPool): String = d.toString
}

case class ConstantNameAndType(name_index: Int, descriptor_index: Int) extends ConstantType(12) {
  override def mkString(implicit cpool: ConstPool): String = {
    cpool.at(name_index).mkString + cpool.at(descriptor_index).mkString
  }
}

case class ConstantUtf8(length: Int, txt: String) extends ConstantType(1) {
  override def mkString(implicit cpool: ConstPool): String = txt

  override def toString = txt
}


