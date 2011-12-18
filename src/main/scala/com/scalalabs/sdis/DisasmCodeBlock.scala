package com.scalalabs.sdis


object DisasmCodeBlock {

  def unsign(b: Byte): Int = (b & 0xff)

  def readOpcode(code: Array[Byte], pc: Int): Int = unsign(code(pc))

  def readByte(code: Array[Byte], pc: Int): Int = unsign(code(pc + 1))

  def readShort(code: Array[Byte], pc: Int): Int = (unsign(code(pc + 1)) << 8) + unsign(code(pc + 2)).toShort

  def readInt(code: Array[Byte], pc: Int): Int = (unsign(code(pc + 1)) << 24) + (unsign(code(pc + 2)) << 16) + (unsign(code(pc + 3)) << 8) + unsign(code(pc + 4))
}

class DisasmCodeBlock(code: Array[Byte], implicit val cpool: ConstPool) {

  import DisasmCodeBlock._

  class HexString(n: Int) {
    //    def toHextStringX:String = n.toHexString
    def toHexString(len: Int): String = {
      val s = n.toHexString
      "0x" + ("0" * (len - s.length)) + s
    }
  }

  implicit def intToHexString(n: Int) = new HexString(n)

  abstract class DynamicProc {
    def addLowHigh(low: Int, high: Int)

    def addJump(idx: Int, jump: Int)

    def addDefault(default: Int)
  }

  def optDynamic(code: Array[Byte], pc1: Int, handle: DynamicProc): (String, Int) = {
    var pc = pc1
    val offset = 0 // pc % 4
    pc += offset
    val defaultByte = readInt(code, pc)
    val lowByte = readInt(code, pc + 4)
    val highByte = readInt(code, pc + 8)

    pc += 13

    handle.addLowHigh(lowByte, highByte)
    for (i <- lowByte to highByte) {
      val jump = 3 + readInt(code, pc - 1)
      handle.addJump(i, jump)
      pc += 4
    }
    handle.addDefault(defaultByte)

    ("", pc) // -1,
  }

  def optCPRefW(code: Array[Byte], pc: Int): String = {
    val idx = DisasmCodeBlock.readShort(code, pc)
    cpool.at(idx).mkString 
  }

  class AsmLine(val pc: Int, val b: Array[Byte], val txt: String)


  def mkCode(code: Array[Byte], attr: List[AttributesInfo]): String = {
    import com.scalalabs.sdis.opcode.Opcode._

    var buff = ""
    var pc = 0;

    val lines = createLines(attr)
    val branches = createBranches(code)
    //    buff = buff + ">>branches count: " + branches.size + " " + branches


    while (pc < code.length) {
      if (Preference.displayLineNosInline) {
        lines.get(pc) match {
          case Some(label) => buff = buff + "\n" + label.toString
          case _ =>
        }
      }
      branches.get(pc) match {
        case Some(label) => buff = buff + "\n" + label.toString + ":"
        case _ =>
      }
      val opVal: Int = readOpcode(code, pc)

      buff = buff + "\n" + (if (Preference.displayCodePoint) {
        pc.toHexString(4) + " " + opcode(opVal)._1
      } else {
        "     " + opcode(opVal)._1
      })

      val handle = new DynamicProc {
        def addLowHigh(low: Int, high: Int) {
          buff = buff + " " + low + " " + high
        }

        def addJump(idx: Int, jump: Int) {
          buff = buff + "\n    " + idx + " " + branches.getOrElse(jump, jump.toHexString(4))
        }

        def addDefault(default: Int) {
          buff = buff + "\n    default: " + branches.getOrElse(default, default.toHexString(4))
        }
      }

      opcode(opVal)._3 match {
        case Kind.Dynamic =>
          val (_, pc1) = optDynamic(code, pc, handle)
          pc = pc1
        case _ =>
          buff = buff + " " + addKindInfo(opcode(opVal)._3, code, pc, branches)
          pc += 1 + opsize(opcode(opVal)._3)

      }

    }
    buff
  }

  import com.scalalabs.sdis.opcode.Opcode._
  import DisasmCodeBlock._

  def addKindInfo(t: Kind.Value, code: Array[Byte], pc: Int, branches: Map[Int, String]): String = {


    t match {

      case Kind.AType => arrayTypeStr(code(pc + 1)) // 1,
      case Kind.Branch =>
        val p = ((pc) + readShort(code, pc)).toShort // 2,
        branchStr(p, branches)

      case Kind.BranchW =>
        val p = ((pc) + readInt(code, pc)) // 4,
        branchStr(p, branches)
      case Kind.Byte => code(pc + 1).toHexString(2) // 1,
      case Kind.CPRef =>
        val idx = readByte(code, pc)
        cpool.at(idx).mkString // 1,
      case Kind.CPRefWUByte => {
        val opVal2 = readByte(code, pc)
        val opVal3 = readByte(code, pc + 1)
        cpool.at(opVal2).mkString + " " + opVal3.toHexString(2)
      }
      case Kind.CPRefWUByteZero => {
        // 4th byte is always 0
        val opVal2 = readShort(code, pc)
        /*val opVal3 =*/ readByte(code, pc + 2)
        cpool.at(opVal2).mkString + " " + opVal2
      }
      //        case Kind.Dynamic => optDynamic(code, pc) 
      case Kind.Local => readByte(code, pc).toHexString(2) // 1,
      case Kind.LocalByte => readByte(code, pc).toHexString(2) // 1,
      case Kind.Short => readShort(code, pc).toHexString(4) // 2,
      case Kind.WideCPRefW => readByte(code, pc).toHexString(2) + " " + readByte(code, pc).toHexString(2) // 2,
      case Kind.WideCPRefWShort => readByte(code, pc).toHexString(2) + " " + readByte(code, pc).toHexString(2) // 2,
      case Kind.CPRefW => optCPRefW(code, pc)

      case _ => ""
    }
  }

  def arrayTypeStr(n: Int): String = {
    n match {
      case 4 => "T_BOOLEAN"
      case 5 => "T_CHAR"
      case 6 => "T_FLOAT"
      case 7 => "T_DOUBLE"
      case 8 => "T_BYTE"
      case 9 => "T_SHORT"
      case 10 => "T_INT"
      case 11 => "T_LONG"
      case x => "UNKNOWN[" + x + "]"
    }
  }

  def branchStr(pc: Int, branches: Map[Int, String]): String =
    branches.get(pc) match {
      case Some(label) => label.toString
      case _ => "0xQQ" + pc.toHexString(4)
    }

  def createBranches(code: Array[Byte]): Map[Int, String] = {
    import com.scalalabs.sdis.opcode.Opcode._

    var loops = Map[Int, String]()
    var loopidx = 0
    var pc = 0;

    def createLabel(idx: Int): String = {
      val label = "Loop" + loopidx
      loopidx += 1
      loops += (idx -> label)
      label;
    }

    while (pc < code.length) {
      val opVal: Int = readOpcode(code, pc)
      opcode(opVal)._3 match {
        case Kind.Branch => {
          val offset = readShort(code, pc).toShort
          //          println("\n>> offset " + offset)
          val i: Int = pc + offset
          createLabel(i)
        }
        case Kind.BranchW => {
          val i: Int = pc + readInt(code, pc)
          createLabel(i)
        }
        case Kind.Dynamic => {
          val handle = new DynamicProc {
            def addLowHigh(low: Int, high: Int) {}

            def addJump(idx: Int, jump: Int) {
              createLabel(jump)
            }

            def addDefault(default: Int) {
              createLabel(default)
            }
          }

          val (_, y) = optDynamic(code, pc, handle)
          pc = y
        }
        case _ =>
      }
      // todo: Kind.Dynamic size
      pc += 1 + opsize(opcode(opVal)._3)

    }

    loops
  }


  def createLines(attr: List[AttributesInfo]): Map[Int, String] = {
    var lines = scala.collection.mutable.Map[Int, String]()
    val ret = attr.map(x => x match {
      case LineNumberTableAttribute(line) =>
        line.flatMap(l => lines += (l.startPC -> (".line " + l.lineNo)))
      case _ =>
    })
    lines.toMap
  }

}