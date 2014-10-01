/*
 Copyright (c) 2011, 2012, 2013 The Regents of the University of
 California (Regents). All Rights Reserved.  Redistribution and use in
 source and binary forms, with or without modification, are permitted
 provided that the following conditions are met:

    * Redistributions of source code must retain the above
      copyright notice, this list of conditions and the following
      two paragraphs of disclaimer.
    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      two paragraphs of disclaimer in the documentation and/or other materials
      provided with the distribution.
    * Neither the name of the Regents nor the names of its contributors
      may be used to endorse or promote products derived from this
      software without specific prior written permission.

 IN NO EVENT SHALL REGENTS BE LIABLE TO ANY PARTY FOR DIRECT, INDIRECT,
 SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES, INCLUDING LOST PROFITS,
 ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF
 REGENTS HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

 REGENTS SPECIFICALLY DISCLAIMS ANY WARRANTIES, INCLUDING, BUT NOT
 LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 A PARTICULAR PURPOSE. THE SOFTWARE AND ACCOMPANYING DOCUMENTATION, IF
 ANY, PROVIDED HEREUNDER IS PROVIDED "AS IS". REGENTS HAS NO OBLIGATION
 TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR
 MODIFICATIONS.
*/

import org.junit.Assert._
import org.junit.Test
import org.junit.Ignore
import Chisel._

class WhenSuite extends TestSuite {

  // Using a single when
  @Test def testWhenStatement() {
    class WhenModule extends Module {
      val io = new Bundle {
        val en = Bool(INPUT)
        val in = UInt(INPUT,4)
        val out = UInt(OUTPUT,4)
      }
      io.out := UInt(0)
      when(io.en) { io.out := io.in }
    }

    class WhenModuleTests(m: WhenModule) extends Tester(m) {
      List(false,true,false,true,false,false,false,true).zipWithIndex.map { 
        case (en, i) =>
          poke(m.io.en, int(en))
          poke(m.io.in, i)
          step(1)
          expect(m.io.out, if(en) i else 0)
      }
    }

    launchCppTester((m: WhenModule) => new WhenModuleTests(m))
  }

  // Put a when inside another when
  @Test def testEmbedWhenStatement() {
    class EmbedWhenModule extends Module {
      val io = new Bundle {
        val en0 = Bool(INPUT)
        val en1 = Bool(INPUT)
        val in = UInt(INPUT,4)
        val out = UInt(OUTPUT,4)
      }
      io.out := UInt(0)
      when(io.en0) { when(io.en1) { io.out := io.in } }
    }

    class EmbedWhenModuleTests(m: EmbedWhenModule) extends Tester(m) {
      List(false, true, false, true,  true, false, true,  true).zip(
      List(false, true, true,  false, true, true,  false, true)).zipWithIndex.map { 
        case ((en0, en1), i) =>
          poke(m.io.en0, int(en0))
          poke(m.io.en1, int(en1))
          poke(m.io.in,  i)
          step(1)
          expect(m.io.out, if(en0 && en1) i else 0)
      } 
    }

    launchCppTester((m: EmbedWhenModule) => new EmbedWhenModuleTests(m))
  }

  // When statement with elsewhen and otherwise clause.
  @Test def testElsewhen() {
    class ElsewhenModule extends Module {
      val io = new Bundle {
        val en0 = Bool(INPUT)
        val en1 = Bool(INPUT)
        val in0 = UInt(INPUT,4)
        val in1 = UInt(INPUT,4)
        val out = UInt(OUTPUT,4)
      }
      when(io.en0) {
        io.out := io.in0
      } .elsewhen(io.en1) {
        io.out := io.in1
      } .otherwise {
        io.out := UInt(0)
      }
    }

    class ElsewhenModuleTests(m: ElsewhenModule) extends Tester(m) {
      List(false, true, false, true,  true, false, true,  true).zip(
      List(false, true, true,  false, true, true,  false, true)).zipWithIndex.map { 
        case ((en0, en1), i) =>
          poke(m.io.en0, int(en0))
          poke(m.io.en1, int(en1))
          poke(m.io.in0, i)
          poke(m.io.in1, i+1)
          step(1)
          expect(m.io.out, if(en0) i else if(en1) i+1 else 0)
      } 
    }

    // Generate a dot file for this case.
    chiselMain(Array[String]("--backend", "dot",
      "--targetDir", dir.getPath.toString()),
      () => Module(new ElsewhenModule()))

    launchCppTester((m: ElsewhenModule) => new ElsewhenModuleTests(m))
  }

  /** instantiate module in a when block.
    */
  @Test def testModuleInWhenBlock() {
    class Submodule extends Module {
      val io = new Bundle {
        val in = UInt(INPUT,4)
        val out = UInt(OUTPUT,4)
      }
      io.out := io.in
    }

    class SubmoduleInWhenBlock extends Module {
      val io = new Bundle {
        val en = Bool(INPUT)
        val in = UInt(INPUT,4)
        val out = UInt(OUTPUT,4)
      }
      io.out := UInt(0)
      when( io.en ) {
        val sub = Module(new Submodule)
        io.out := sub.io.out
        io <> sub.io /* connect only io.in to sub.io.in */
      }
    }

    class SubmoduleInWhenBlockTests(m: SubmoduleInWhenBlock) extends Tester(m) {
      List(false,true,false,true,false,false,false,true).zipWithIndex.map { 
        case (en, i) =>
          poke(m.io.en, int(en))
          poke(m.io.in, i)
          step(1)
          expect(m.io.out, if(en) i else 0)
      }
    }

    launchCppTester((m: SubmoduleInWhenBlock) => new SubmoduleInWhenBlockTests(m))
  }

  // Unless statement with elsewhen and otherwise clause
  @Test def testUnless() {
    class UnlessModule extends Module {
      val io = new Bundle {
        val en = Bool(INPUT)
        val in = UInt(INPUT,4)
        val out = UInt(OUTPUT,4)
      }
      io.out := io.in
      unless(io.en) { io.out := UInt(0) }
    }

    class UnlessModuleTests(m: UnlessModule) extends Tester(m) {
      List(false,true,false,true,false,false,false,true).zipWithIndex.map { 
        case (en, i) =>
          poke(m.io.en, int(en))
          poke(m.io.in, i)
          step(1)
          expect(m.io.out, if(en) i else 0)
      }
    }

    launchCppTester((m: UnlessModule) => new UnlessModuleTests(m))
  }


  // switch statement, is clauses, and ? literals
  @Test def testSwitch() {
    class SwitchModule extends Module {
      val io = new Bundle {
        val in = UInt(INPUT,4)
        val out = Bool(OUTPUT)
      }
      io.out := Bool(false)
      switch(io.in) {
        is(UInt(0)) { io.out := Bool(true) }
        is(Bits("b???1")) { io.out := Bool(true) }
      }
    }

    class SwitchModuleTests(m: SwitchModule) extends Tester(m) {
      (0 until 8).map { i =>
        poke(m.io.in, i)
        step(1)
        expect(m.io.out, if(i == 0) (1) else (i % 2))
      } 
    }

    launchCppTester((m: SwitchModule) => new SwitchModuleTests(m))
  }

  // Bad code generated for nested when. Issue #265
  @Test def testNestedWhen265() {
    class testNestedWhen265_1 extends Module {
      val io = new Bundle {
        val cond1 = Bool(INPUT)
        val cond2 = Bool(INPUT)
        val in = Bool(INPUT)
        val out = Bool(OUTPUT)
      }
    
      val register = Reg(Bool())
    
      when(io.cond1){
        when(io.cond2){
          register := io.in
        } otherwise {
          register := Bool(false)
        }
      }
    
      io.out := register
    }

    class NestedWhenTests_1(m: testNestedWhen265_1) extends Tester(m) {
      case class TruthTable(cond1: Int, cond2: Int, in: Int, out:Int)
      val truthTable = Array[TruthTable](
          TruthTable(0, 0, 1, 0),
          TruthTable(0, 1, 1, 0),
          TruthTable(1, 0, 1, 0),
          TruthTable(1, 1, 0, 0),
          TruthTable(1, 1, 1, 1),
          TruthTable(0, 0, 1, 1), // Register should retain its previous value
          TruthTable(0, 1, 0, 1), // Register should retain its previous value
          TruthTable(0, 0, 0, 1)  // Register should retain its previous value
          )
      for (tv <- truthTable) {
        poke(m.io.in, tv.in)
        poke(m.io.cond1, tv.cond1)
        poke(m.io.cond2, tv.cond2)
        step(1)
        expect(m.io.out, tv.out)
      }
    }

    class testNestedWhen265_2 extends Module {
      val io = new Bundle {
        val cond1 = Bool(INPUT)
        val cond2 = Bool(INPUT)
        val in = Bool(INPUT)
        val out = Bool(OUTPUT)
      }
    
      val register = Reg(Bool())
    
      when(io.cond1){
        when(io.cond2){
          register := io.in
        }
      } otherwise {
        register := Bool(false)
      }
    
      io.out := register
    }

    class NestedWhenTests_2(m: testNestedWhen265_2) extends Tester(m) {
      case class TruthTable(cond1: Int, cond2: Int, in: Int, out:Int)
      val truthTable = Array[TruthTable](
          TruthTable(0, 0, 1, 0),
          TruthTable(0, 1, 1, 0),
          TruthTable(1, 0, 1, 0),
          TruthTable(1, 1, 1, 1),
          TruthTable(1, 0, 1, 1), // Register should retain its previous value
          TruthTable(1, 0, 0, 1)  // Register should retain its previous value
          )
      for (tv <- truthTable) {
        poke(m.io.in, tv.in)
        poke(m.io.cond1, tv.cond1)
        poke(m.io.cond2, tv.cond2)
        step(1)
        expect(m.io.out, tv.out)
      }
    }

    // Generate a dot file for these cases.
    chiselMain(Array[String]("--backend", "dot",
      "--targetDir", dir.getPath.toString()),
      () => Module(new testNestedWhen265_1()))

    launchCppTester((m: testNestedWhen265_1) => new NestedWhenTests_1(m))

    // Generate a dot file for these cases.
    chiselMain(Array[String]("--backend", "dot",
      "--targetDir", dir.getPath.toString()),
      () => Module(new testNestedWhen265_2()))

    launchCppTester((m: testNestedWhen265_2) => new NestedWhenTests_2(m))
  }

}
