package com.scalalabs.sdis

//import scala.io.Source
import java.io.{File, FileInputStream}


object Scalap {

  val hello = "Hello"
    
  def main( args:Array[String]) {
    println("Scala Disassembler")
    
    args.size match {
      case 1 =>  disClazz( args(0),  new FormatedPrintStream(Console.out))
      case 2 => disMethod( args(0)+".class", args(1), new FormatedPrintStream(Console.out) )
      case _ => println("ERR: Invalid params")
    }
    
    
//    val mainClass = if( args.size >= 1) Some(args(0)) else None
//    val mainMethod = if( args.size >= 2) Some(args(1)) else None
//    println("Class: " + mainClass )
//    println("Method: " + mainMethod )
//
//    mainClass match {
//      case Some(clazz:String) => disClazz( clazz, "", new FormatedPrintStream(Console.out))
//      case None => println("No file")
//    }
    
  }  
    
    
  def fileToBytes(f:File):Array[Byte] = {
    val len:Int  = f.length.toInt 
    val buff = new Array[Byte](len)
    val in = new FileInputStream(f)
    in.read(buff)
    buff
  }
  
  
  def disClazz( clazzRoot: String,  out: FormatedPrintStream) {

    Preference.displayHeader = false
    Preference.displayCodeBytes = false
    
    Preference.displayConstPool = false
    Preference.displayCodePoint = false 
    
    out.println("")
    
    fileDis( clazzRoot + """.class""", out )
    out.println("")
    fileDis( clazzRoot + """$.class""", out )
  }


  def fileDis( filename:String,  out: FormatedPrintStream) {
    println("File: " + filename)
//    println( "cpool length = " + ClazzReader.clazz.cpool.lst().length)
    val file = new java.io.File( filename  )//in.toURI)
    val clazz = ClazzReader.readClazz(new RawClazz(Scalap.fileToBytes(file)))
    new FullClassOut(clazz, out).disassemble()
  }


  def disMethod( filename: String, method: String,  out: FormatedPrintStream) {

    Preference.displayHeader = false
    Preference.displayCodeBytes = true
    Preference.displayConstPool = false
    Preference.displayCodePoint = false

    out.println("")

    val file = new java.io.File( filename  )//in.toURI)
    val clazz = ClazzReader.readClazz(new RawClazz(Scalap.fileToBytes(file)))
    new MethodOut(method, clazz, out).disassemble()
  }


  
}