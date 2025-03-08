package kuplrg

object Implementation extends Template {

  import RE.*
  import SFA.Edge
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
	
    val dfa4 = DFA(1, "01", "0", "1")
    val dfa5 = DFA(2, "01", "3", "2")

    // example ENFA
    val enfa1 = ENFA(4, "ab", "fs8wsgw", "8")
    val enfa2 = ENFA(6, "ab", "ost0183xsrlx05m", "w")
    val enfa3 = ENFA(6, "01", "urgp7qvlhgmm39c", "w")

    // You can dump an FA to see it in the automaton viewer.
    // After running this program, open `viewer/index.html` in your browser.
    //dfaToRE(dfa4).dump

    println("--------------------------------------------------")

    // You can dump an RE to see its string form and original form.
    dfaToRE(dfa4).dump
    dfaToRE(dfa5).dump

    println("--------------------------------------------------")

    // You can generate random FA or RE to test your implementation.
    //
    // fuzzDFA(n = 3, symbols = "01").dump
    // fuzzENFA(n = 3, symbols = "01").dump
    // fuzzRE(depth = 3, symbols = "01").dump

    println("--------------------------------------------------")
  }

  // ---------------------------------------------------------------------------
  // Problem 1: Regular Expressions to epsilon-NFA
  // ---------------------------------------------------------------------------
  /** Convert a regular expression to an epsilon-NFA
   *
   *  @param re
   *  @return
   *    the equivalent epsilon-NFA
   */
  def reToENFA(re: RE): ENFA = reToSFA(re, 1).toENFA

  /** Convert a regular expression to a simplified epsilon-NFA with a given
    * initial state `i`
    *
    * @param re
    *   the regular expression
    * @param i
    *   the initial state
    * @return
    *   the simplified epsilon-NFA
    */
  def reToSFA(re: RE, i: State): SFA = re match
    case Emp => SFA(
      from = i,
      edges = Set(),
      to = i + 1,
    )
    case Eps => SFA(
      from = i,
      edges = Set(
      Edge(i, None, i+1)
      ),
      to = i + 1,
    )
    case Sym(symbol) => SFA(
      from = i,
      edges = Set(
      Edge(i, Some(symbol), i+1)
      ),
      to = i + 1,
    )
    case Union(left, right) =>
      val SFA(_, ledges, j) = reToSFA(left, i + 1)
      val SFA(_, redges, k) = reToSFA(right, j + 1)
      SFA(
        from = i,
        edges = ledges ++ redges ++ Set(
          Edge(i, None, i + 1),
          Edge(i, None, j + 1),
          Edge(j, None, k + 1),
          Edge(k, None, k + 1),
        ),
        to = k + 1,
      )
    case Concat(left, right) => 
      val SFA(_, ledges, j) = reToSFA(left, i)
      val SFA(_, redges, k) = reToSFA(right, j + 1)
      SFA(
        from = i,
        edges = ledges ++ redges ++ Set(
          Edge(j, None, j + 1),
        ),
        to = k,
      )
    case Star(re) => 
      val SFA(_, ledges, j) = reToSFA(re, i + 1)
      SFA(
        from = i,
        edges = ledges ++ Set(
          Edge(i, None, j+1),
          Edge(i, None, i+1),
          Edge(j, None, i+1),
          Edge(j, None, j+1),
	  ),
        to = j + 1,
    )

  // ---------------------------------------------------------------------------
  // Problem 2: DFA to Regular Expressions
  // ---------------------------------------------------------------------------
  /** Convert a DFA to a regular expression.
    *
    * @param dfa
    *   the given DFA
    * @return
    *   the equivalent regular expression
    */
  def dfaToRE(dfa: DFA): RE =
    dfa.finalStates
      .map(Implementation.reForPaths(dfa, dfa.initState, _, dfa.states.size))
      .foldLeft(Emp)(Union(_, _))

  /** A regular expression accepting paths from `i` to `j` with intermediate
    * states bounded by `k` in a given DFA `dfa`. Assume that the given DFA
    * `dfa` is already normalized (i.e., the states of DFA are 1, 2, ..., n).
    *
    * @param dfa
    *   the given DFA
    * @param i
    *   the initial state
    * @param j
    *   the final state
    * @param k
    *   the bound of intermediate states
    */
  def reForPaths(dfa: DFA, i: State, j: State, k: State): RE = k match
    case 0 =>
      if (i == j)
        val i_to_i_set = dfa.trans.collect{ case ((state, symbol), next) if (state == i && next == i) => Sym(symbol) }.toSet
        val result = if (i_to_i_set.nonEmpty) i_to_i_set.reduce(Union(_, _)) else Emp
        Union(Eps, result)
      else
        val i_to_j_set = dfa.trans.collect{ case ((state, symbol), nextState) if (state == i && nextState == j) => Sym(symbol) }.toSet
        val result = if (i_to_j_set.nonEmpty) i_to_j_set.reduce(Union(_, _)) else Emp
        result
    case _ => 
      Union(reForPaths(dfa,i,j,k-1),Concat((Concat(reForPaths(dfa,i,k,k-1),Star(reForPaths(dfa,k,k,k-1)))),reForPaths(dfa,k,j,k-1)))
 
  // ---------------------------------------------------------------------------
  // Problem 3: epsilon-NFA to DFA
  // ---------------------------------------------------------------------------
  /** Convert an epsilon-NFA to a DFA.
    *
    * @param enfa
    *   the epsilon-NFA
    * @return
    *   the equivalent DFA
    */
  def enfaToDFA(enfa: ENFA): DFA =
    // An auxiliary function to find all reachable set of states in epsilon-NFA
    // using depth-first search, and return the mapping from each of them to the
    // corresponding state in DFA.
    def aux(
      qsList: List[Set[State]],
      k: Int,
      map: Map[Set[State], Int],
    ): Map[Set[State], State] = qsList match
      case Nil => map
      case qs :: rest =>
        // The next possible set of states
        val next: Set[Set[State]] =
          enfa.symbols.map(a => enfa.extTrans(qs, a.toString))
        // The set of already visited set of states
        val visited: Set[Set[State]] = map.keySet
        // The set of states
        val yets: Set[Set[State]] = next -- visited - qs
        aux(
          yets.toList ++ rest,
          k + 1,
          map + (qs -> k),
        )
    val init: Set[State] = enfa.eclo(enfa.initState)
    // The mapping from reachable set of states in epsilon-NFA to the
    // corresponding state in DFA
    val map: Map[Set[State], State] = aux(List(init), 1, Map.empty)
	
	// 여기부터 구현
    val DFA_state = map.values.toSet
    val DFA_symbols = enfa.symbols
    val DFA_trans = map.flatMap { case (enfa_states, dfa_state) =>
      enfa.symbols.flatMap {sb =>
        val enfa_next_states = enfa.extTrans(enfa_states, sb.toString)
        map.get(enfa_next_states).map(dfa_next_state =>((dfa_state, sb)->dfa_next_state))
      }
    }.toMap
	
    val DFA_init_state = map(init)
    val DFA_final_states = map.filter{ case (key,_)=>key.intersect(enfa.finalStates).nonEmpty}.values.toSet

    DFA(
      states = DFA_state,
      symbols = DFA_symbols,
      trans = DFA_trans,
      initState = DFA_init_state,
      finalStates = DFA_final_states
    )
}
