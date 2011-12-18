package com.scalalabs.sdis

import java.io.PrintStream


class FormatedPrintStream(out: PrintStream) {

  /**cmds always start at start of line and rest current tab values
   */
  def cmd(s: String) {
    // only print if there is something to print
    if (s.length > 0) out.println(s)
  }

  /**asm have a 4 space tab
   */
  def asm(s: String) {
    out.println("    " + s)
  }


  def println(s: String) {
    out.println(s)
  }

  def print(s: String) {
    out.print(s)
  }
}