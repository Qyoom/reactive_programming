package simulations

import common._

class Wire {
  private var sigVal = false
  private var actions: List[Simulator#Action] = List()

  def getSignal: Boolean = sigVal

  def setSignal(s: Boolean) {
    if (s != sigVal) {
      sigVal = s
      actions.foreach(action => action())
    }
  }

  def addAction(a: Simulator#Action) {
    actions = a :: actions
    a()
  }
}

abstract class CircuitSimulator extends Simulator {

  val InverterDelay: Int
  val AndGateDelay: Int
  val OrGateDelay: Int

  def probe(name: String, wire: Wire) {
    wire addAction {
      () => afterDelay(0) {
        println(
          "  " + currentTime + ": " + name + " -> " +  wire.getSignal)
      }
    }
  }

  def inverter(input: Wire, output: Wire) {
    def invertAction() {
      val inputSig = input.getSignal
      afterDelay(InverterDelay) { output.setSignal(!inputSig) }
    }
    input addAction invertAction
  }

  def andGate(a1: Wire, a2: Wire, output: Wire) {
    def andAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(AndGateDelay) { output.setSignal(a1Sig & a2Sig) }
    }
    a1 addAction andAction
    a2 addAction andAction
  }

  /* orGate should be implemented from scratch, following the pattern of the andGate method. */ 
  /* Implement orGate just like in the lesson videos) */
  def orGate(a1: Wire, a2: Wire, output: Wire) {
    def orAction() {
      // First get the signals
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal

      // Add the action with a delay
      afterDelay(OrGateDelay) { output.setSignal(a1Sig | a2Sig) }
    }
    a1 addAction orAction
    a2 addAction orAction
  }

  // orGate2 should be implemented solely in terms of andGate and inverter. 
  // (invert both inputs, and then to a nand)
  def orGate2(a1: Wire, a2: Wire, output: Wire) {
    val notA1, notA2, notOutput = new Wire
    inverter(a1, notA1); inverter(a2, notA2)
    andGate(notA1, notA2, notOutput)
    inverter(notOutput, output)
  }

  // Demultiplexer
  // with n control wires (and 2 to the power n output wires). The demultiplexer directs the signal from an input wire based on the control
  // signal. The rest of the output signals are set to 0.
  def demux(in: Wire, c: List[Wire], out: List[Wire]) {
    c match {
      case smallC :: xs => {
        val inL, inR = new Wire /* Division in the diagram */
        val notSmallC = new Wire
        andGate(in, smallC, inL)
        inverter(smallC, notSmallC);
        andGate(in, notSmallC, inR)

        // Because it works in powers of two
        val n = out.length / 2

        // Now the recursive part as done in the drawing
        val rightDemux = out take n
        val leftDemux = out drop n

        demux(inL, xs, rightDemux) /* Could we do it sequencially and not dividing by half ? */
        demux(inR, xs, leftDemux)
      }

      // If we are at the simplest multiplexer then we pass the input to the output
      case Nil => {
        val notIn = new Wire
        inverter(in, notIn)
        inverter(notIn, out(0))  // Double inverts give the same value
      }
    }
  }
}

object Circuit extends CircuitSimulator {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5

  def andGateExample {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    probe("in1", in1)
    probe("in2", in2)
    probe("out", out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    in1.setSignal(true)
    run

    in2.setSignal(true)
    run
  }

  //
  // to complete with orGateExample and demuxExample...
  //
}

object CircuitMain extends App {
  // You can write tests either here, or better in the test class CircuitSuite.
  Circuit.andGateExample
}
