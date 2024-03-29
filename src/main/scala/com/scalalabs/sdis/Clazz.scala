/**
 * Disassembler
 * (c)2011 andy hicks 
 */


package com.scalalabs.sdis

//import scala.reflect.NameTransformer

class Clazz extends ByteFormat {

  var magic: u4 = 0
  var minor_version: u2 = 0
  var major_version: u2 = 0

  //  var constant_pool_count = 0
  var cpool = new ConstPool // List[ConstantType] = _

  var access_flags: u2 = 0
  var this_class: u2 = 0
  var super_class: u2 = 0

  //  var interfaces_count: u2 = 0
  var interfaces: List[u2] = List[u2]()

  //  var fields_count: u2 = 0
  var fields: List[FieldInfo] = List[FieldInfo]()

  //  var methods_count: u2 = 0
  var methods: List[MethodInfo] = List[MethodInfo]()

  //  var attributes_count: u2 = 0
  var attributes: List[AttributesInfo] = List[AttributesInfo]()

}

sealed class CONSTANT_tag(val tag: Int)

case class CONSTANT_Class() extends CONSTANT_tag(7)

case class CONSTANT_Fieldref() extends CONSTANT_tag(9)

case class CONSTANT_Methodref() extends CONSTANT_tag(10)

case class CONSTANT_InterfaceMethodref() extends CONSTANT_tag(11)

case class CONSTANT_String() extends CONSTANT_tag(8)

case class CONSTANT_Integer() extends CONSTANT_tag(3)

case class CONSTANT_Float() extends CONSTANT_tag(4)

case class CONSTANT_Long() extends CONSTANT_tag(5)

case class CONSTANT_Double() extends CONSTANT_tag(6)

case class CONSTANT_NameAndType() extends CONSTANT_tag(12)

case class CONSTANT_Utf8() extends CONSTANT_tag(1)


case class FieldInfo(access_flags: Int, name_index: Int, descriptor_index: Int, attributes: List[AttributesInfo]) extends ByteFormat

object MethodInfo {
  val ACC_PUBLIC = 0x0001 // Declared public; may be accessed from outside its package.
  val ACC_PRIVATE = 0x0002 // Declared private; accessible only within the defining class.
  val ACC_PROTECTED = 0x0004 // Declared protected; may be accessed within subclasses.
  val ACC_STATIC = 0x0008 // Declared static.
  val ACC_FINAL = 0x0010 // Declared final; may not be overridden.
  val ACC_SYNCHRONIZED = 0x0020 // Declared synchronized; invocation is wrapped in a monitor lock.
  val ACC_BRIDGE = 0x0040 // A bridge method, generated by the compiler.
  val ACC_VARARGS = 0x0080 // Declared with variable number of arguments.
  val ACC_NATIVE = 0x0100 // Declared native; implemented in a language other than Java.
  val ACC_INTERFACE = 0x0200 // Is an interface, not a class.
  val ACC_ABSTRACT = 0x0400 // Declared abstract; no implementation is provided.
  val ACC_STRICT = 0x0800 // Declared strictfp; floating-point mode is FP-strict
  val ACC_SYNTHETIC = 0x1000 // Declared synthetic; not present in the source code.
  val ACC_ANNOTATION = 0x2000 // Declared as an annotation type.
  val ACC_ENUM = 0x4000 // Declared as an enum type.
}


class MethodInfo(val access_flags: Int, val name_index: Int, val descriptor_index: Int, val attributes: List[AttributesInfo])

trait AttributesInfo


// ConstantValue (4.7.2)
case class ConstantValueAttribute(constantvalue_index: Int) extends AttributesInfo


// Code (4.7.3), 
case class CodeAttribute(max_stack: Int, max_locals: Int, code: Array[Byte], exception_table: List[ExceptionInfo], attribs: List[AttributesInfo]) extends AttributesInfo


// StackMapTable (4.7.4) 
// Exceptions (4.7.5), 
case class ExceptionsAttribute(info1: Array[Byte]) extends AttributesInfo

// InnerClasses (4.7.6), 
case class InnerClasses(inner_class_info_index: Int, outer_class_info_index: Int, inner_name_index: Int, inner_class_access_flags: Int)

case class InnerClassesAttribute(innClasses: List[InnerClasses]) extends AttributesInfo

// EnclosingMethod (4.7.7), 
case class EnclosingMethodAttribute(class_index: Int, method_index: Int) extends AttributesInfo

// Synthetic (4.7.8),
case class SyntheticAttribute() extends AttributesInfo

// Signature (4.7.9),
case class SignatureAttribute(signature_index: Int) extends AttributesInfo

// SourceFile(4.7.10), 
case class SourceFileAttribute(idx: Int) extends AttributesInfo

// SourceDebugExtension (4.7.11), 
case class SourceDebugExtensionAttribute(debug_extension: Array[Byte]) extends AttributesInfo

// LineNumberTable (4.7.12), 
case class LineNumberItem(startPC: Int, lineNo: Int)

case class LineNumberTableAttribute(lines: List[LineNumberItem]) extends AttributesInfo


// LocalVariableTable (4.7.13), 
case class LocalVariableTableItem(start_pc: Int, length: Int, name_index: Int, descriptor_index: Int, index: Int)

case class LocalVariableTableAttribute(list: List[LocalVariableTableItem]) extends AttributesInfo

// LocalVariableTypeTable (4.7.14), 
case class LocalVariableTypeTableItem(start_pc: Int, length: Int, name_index: Int, signature_index: Int, index: Int)

case class LocalVariableTypeTableAttribute(lines: List[LocalVariableTypeTableItem]) extends AttributesInfo


// Deprecated (4.7.15), 
case class DeprecatedAttribute() extends AttributesInfo

// RuntimeVisibleAnnotations (4.7.16)
class ElementValue(tag1: Int)

case class ConstElementValue(tag: Int, const_value_index: Int) extends ElementValue(tag: Int)

case class EumConstElementValue(tag: Int, type_name_index: Int, const_name_index: Int) extends ElementValue(tag: Int)

case class ClassInfoElementValue(tag: Int, class_info_index: Int) extends ElementValue(tag: Int)

case class AnnotationElementValue(tag: Int, annotation_value: RVAnnonItem) extends ElementValue(tag: Int)

case class ArrayElementValue(tag: Int, element_value: ElementValue) extends ElementValue(tag: Int)

case class RVElementValuePairsItem(element_name_index: Int, element_value_pairs: List[ElementValue])

case class RVAnnonItem(type_index: Int, element_value_pairs: List[RVElementValuePairsItem])

case class RuntimeVisibleAnnotationsAttribute(annotations: List[RVAnnonItem]) extends AttributesInfo

// RuntimeInvisibleAnnotations (4.7.17) 
case class RuntimeInvisibleAnnotationsAttribute(annotations: List[RVAnnonItem]) extends AttributesInfo

// RuntimeVisibleParameterAnnotations (4.7.18)
// RuntimeInvisibleParameterAnnotations (4.7.19) 
// AnnotationDefault (4.7.20)
// SourceID (4.7.21)
// CompilationID (4.7.22)


case class ScalaSigAttribute(major: Int, minor: Int, entries: Int) extends AttributesInfo

case class ScalaAttribute(info: Array[Byte]) extends AttributesInfo


case class UnknownAttrib(name: String, info1: Array[Byte]) extends AttributesInfo


class ExceptionInfo(val start_pc: Int, val end_pc: Int, val handler_pc: Int, val catch_type: Int)


case class AnnotationInfo(type_index: Short, num_element_value_pairs: Short, annotationelement: List[AnnotationElementInfo])

case class AnnotationElementInfo(element_name_index: Short)

//  element_value {
//    u1 tag;
//    union {
//      u2   const_value_index;
//      {
//        u2 type_name_index;
//        u2 const_name_index;
//      } enum_const_value;
//      u2   class_info_index;
//      annotation annotation_value;
//      {
//        u2    num_values;
//        element_value values[num_values];
//      } array_value;
//    } value;
//  }





