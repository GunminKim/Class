package kuplrg

object Implementation extends Template {

  /** This is the playground for you to run your implementation. Do whatever you
    * want here and run `sbt run` to see the result.
    */
  @main def playground: Unit = {
    println("------------------- PLAYGROUND -------------------")

    // You can check your implementation here.
    val pda17: PDA = PDA(
      initState = 0,
      initAlphabet = "Z",
      finalStates = Set(),
      (0, Some('a'), "Z") -> (0, List("X", "Z")),
      (0, Some('a'), "X") -> (0, List("X", "X")),
      (0, None, "Z") -> (1, List("Z")),
      (0, None, "X") -> (1, List("X")),
      (1, Some('b'), "X") -> (2, List()),
      (2, Some('b'), "X") -> (1, List()),
      (2, None, "Z") -> (2, List()),
    )
    pda17.dump
    pdaes2cfg(pda17).dump
    println("--------------------------------------------------")
  }

  // Convert a PDA with final states to a PDA with empty stacks
  def pdafs2es(pda: PDA): PDA = ???

  // Convert a PDA with empty stacks to a PDA with final states
  def pdaes2fs(pda: PDA): PDA = ???

  // Convert a CFG to a PDA with empty stacks
  def cfg2pdaes(cfg: CFG): PDA = ???

  // Convert a PDA with empty stacks to a CFG
  def pdaes2cfg(pda: PDA): CFG = 
    def f(l: List[Alphabet], from: State, end: State): List[Rhs] = l match
      case Nil => List(Rhs(List()))
      case x :: Nil => List(Rhs(List(from.toString + x + end.toString)))
      case x :: xs => pda.states.flatMap(s => f(xs, s, end).map(r => Rhs(List(from.toString + x + s.toString) ++ r.seq))).toList
    val tmp: Set[Nt] = pda.states.flatMap(i => pda.states.flatMap(j => pda.alphabets.map(i.toString + _ + j.toString)))
    val list1: List[(Nt, Rhs)] = pda.trans.toList.flatMap({
      case ((from, sym, al), v) => v.flatMap({
        case (to, l) => l match
          case Nil => List((from.toString + al + to.toString, sym match
            case None => Rhs(List())
            case Some(a) => Rhs(List(a))))
          case l => pda.states.flatMap(s => sym match
            case None => f(l, to, s).map((from.toString + al + s.toString, _))
            case Some(a) => f(l, to, s).map(i => (from.toString + al + s.toString, Rhs(List(a) ++ i.seq)))
          )
      })
    })
    val keySet1: Set[Nt] = list1.map(_._1).toSet + "S"
    val map2: Map[Nt, List[Rhs]] = list1.filter(_._2.seq.forall({
      case n: Nt => keySet1.contains(n)
      case _ => true
      })).groupBy(_._1).view.mapValues(_.map(_._2)).toMap + ("S" -> pda.states.filter(s => keySet1.contains(pda.initState.toString + pda.initAlphabet + s.toString())).toList.map(s => Rhs(List(pda.initState.toString + pda.initAlphabet + s.toString))))

   
    
    CFG(
      nts = map2.keySet,
      symbols = pda.symbols,
      start = "S",
      rules = map2,
    )

}
