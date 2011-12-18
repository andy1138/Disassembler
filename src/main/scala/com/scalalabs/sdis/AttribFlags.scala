package com.scalalabs.sdis

object AttribFlags {

  def mkString(flag: Int): String = {
    import MethodInfo._
    def mkStr(f: Int, s: String) = if ((flag & f) > 0) s + " " else ""

    def strPublic = mkStr(ACC_PUBLIC, "public")
    def strPrivate = mkStr(ACC_PRIVATE, "private")
    def strProtected = mkStr(ACC_PROTECTED, "protected")
    def strStatic = mkStr(ACC_STATIC, "static")
    def strFinal = mkStr(ACC_FINAL, "final")

    def strSynchronized = mkStr(ACC_SYNCHRONIZED, "synchronized")
    def strBridge = mkStr(ACC_BRIDGE, "bridge")
    def strVarargs = mkStr(ACC_VARARGS, "varargs")
    def strNative = mkStr(ACC_NATIVE, "native")
    def strInterface = mkStr(ACC_INTERFACE, "interface")
    def strAbstract = mkStr(ACC_ABSTRACT, "abstract")
    def strStrict = mkStr(ACC_STRICT, "strict")
    def strSynthetic = mkStr(ACC_SYNTHETIC, "synthetic")
    def strAnnotation = mkStr(ACC_ANNOTATION, "annotation")
    def strEnum = mkStr(ACC_ENUM, "enum")

    strPublic + strPrivate + strProtected + strStatic + strFinal +
      strSynchronized + strBridge + strVarargs + strNative + strInterface +
      strAbstract + strStrict + strSynthetic + strAnnotation + strEnum
  }


}