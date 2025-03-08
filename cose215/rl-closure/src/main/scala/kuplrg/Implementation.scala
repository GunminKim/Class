package kuplrg

object Implementation extends Template {

  import RE.*
  import Fuzzer.*

  /**
   * This is the playground for you to run your implementation.
   * Do whatever you want here and run `sbt run` to see the result.
   */
  @main def playground: Unit = {
    println("------------------- PLAYGROUND -------------------")

    // example RE
    val re1 = RE("</>")
    val re2 = RE("a|b")
    val re3 = RE("0*")

    // example DFA
    val dfa1 = DFA(1, "01", "0", "1")
    val dfa2 = DFA(2, "01", "3", "2")
    val dfa3 = DFA(2, "ab", "5", "2")
	
    val cdfa1 = complementDFA(dfa1)

    // example ENFA
    val enfa1 = ENFA(4, "ab", "fs8wsgw", "8")
    val enfa2 = ENFA(6, "ab", "ost0183xsrlx05m", "w")
    val enfa3 = ENFA(6, "01", "urgp7qvlhgmm39c", "w")

    // You can dump an FA to see it in the automaton viewer.
    // After running this program, open `viewer/index.html` in your browser.
    //cdfa1.dump

    println("--------------------------------------------------")

    // You can dump an RE to see its string form and original form.
    reverseRE(re2).dump

    println("--------------------------------------------------")

    // You can generate random FA or RE to test your implementation.
    //
    // fuzzDFA(n = 3, symbols = "01").dump
    // fuzzENFA(n = 3, symbols = "01").dump
    fuzzRE(depth = 3, symbols = "01").dump

    println("--------------------------------------------------")
  }

  /** Complment operation for DFA */
  def complementDFA(dfa: DFA): DFA = DFA(
    states      = dfa.states,
    symbols     = dfa.symbols,
    trans       = dfa.trans,
    initState   = dfa.initState,
    finalStates = dfa.states.diff(dfa.finalStates),
  )
  
/*
 case class DFA(
  states: Set[State],
  symbols: Set[Symbol],
  trans: Map[(State, Symbol), State],
  initState: State,
  finalStates: Set[State],
) extends FA:
*/

  /** Intersection operation for DFA */
  def intersectDFA(ldfa: DFA, rdfa: DFA): DFA = ???
 

  /** Reverse operation for ENFA */
  def reverseENFA(enfa: ENFA): ENFA = ???


/*
  /** Empty */
  case Emp

  /** Epsilon */
  case Eps

  /** Symbol */
  case Sym(symbol: Symbol)

  /** Union */
  case Union(left: RE, right: RE)

  /** Concatenation */
  case Concat(left: RE, right: RE)

  /** Kleene star */
  case Star(re: RE)
 */

  /** Reverse operation for RE */
  def reverseRE(re: RE): RE = re match {
  case Union(left, right) => Union(reverseRE(left),reverseRE(right))
  case Concat(left,right) => Concat(reverseRE(right),reverseRE(left))
  case Star(th) => Star(reverseRE(th))
  case _ => re
  }

  /** Homomorphism operation for RE */
  def homRE(re: RE, h: Hom): RE = re match {
  case Union(left, right) => Union(homRE(left,h),homRE(right,h))
  case Concat(left,right) => Concat(homRE(right,h),homRE(left,h))
  case Star(th) => Star(homRE(th,h))
  //case Sym(th) => homRE(RE(th),h)
  case _ => re
  }

/*
  /** Inverse homomorphism operation for DFA */
  def ihomDFA(dfa: DFA, h: Hom): DFA = ???

*/
/*
/** Inverse homomorphism operation */
  def ihom(h: Hom): Lang = Lang(
    h.keySet,
    w => this.contains(w.map(h).mkString),
  )
*/

}