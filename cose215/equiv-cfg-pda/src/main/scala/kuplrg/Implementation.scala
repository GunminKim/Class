package kuplrg

object Implementation extends Template {

  /** This is the playground for you to run your implementation. Do whatever you
    * want here and run `sbt run` to see the result.
    */
  @main def playground: Unit = {
    println("------------------- PLAYGROUND -------------------")

    // You can check your implementation here.
    val pda: PDA = PDA(
      states = Set(0, 1),
      symbols = Set('a', 'b'),
      alphabets = Set("X", "Z"),
      trans = Map(
        (0, Some('a'), "Z") -> Set((0, List("X", "Z"))),
        (0, Some('a'), "X") -> Set((0, List("X", "X"))),
        (0, None, "Z") -> Set((1, List("Z"))),
        (0, None, "X") -> Set((1, List("X"))),
        (1, Some('b'), "X") -> Set((1, List())),
        (1, None, "Z") -> Set((1, List())),
      ).withDefaultValue(Set()),
      initState = 0,
      initAlphabet = "Z",
      finalStates = Set(),
    )

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
  def pdafs2es(pda: PDA): PDA = PDA(
    states = pda.states + (-1) + (100),
    symbols = pda.symbols,
    alphabets = pda.alphabets + "p",
    trans = {
      val new1 = (pda.alphabets + "p").flatMap { alpha =>
        pda.finalStates.map { state =>
          ((state, None, alpha), Set((100, List.empty[Alphabet])))
        }
      }.toMap
      
      val new2 = (pda.alphabets + "p").map { alpha2 =>
        ((100, None, alpha2), Set((100, List.empty[Alphabet])))
      }.toMap
    
      val new_trans = pda.trans + (
        ((-1, None, "p")) -> Set((pda.initState, List(pda.initAlphabet,"p"): List[Alphabet]))
      ) ++ new1 ++ new2
      new_trans.withDefaultValue(Set())
    },
    initState = -1,
    initAlphabet = "p",
    finalStates = Set()
    )

  // Convert a PDA with empty stacks to a PDA with final states
  def pdaes2fs(pda: PDA): PDA = PDA(
    states = pda.states + (-1) + (100),
    symbols = pda.symbols,
    alphabets = pda.alphabets + "p",
    trans = {
      val new1 = (pda.states).map { state =>
        ((state, None, "p"), Set((100, List("p"))))
      }.toMap
    
      val new_trans = pda.trans + (
        ((-1, None, "p")) -> Set((pda.initState, List(pda.initAlphabet,"p"): List[Alphabet]))
      ) ++ new1
	  
      new_trans.withDefaultValue(Set())
    },
    initState = -1,
    initAlphabet = "p",
    finalStates = Set(100)
    )

  // Convert a CFG to a PDA with empty stacks
  def cfg2pdaes(cfg: CFG): PDA = PDA(
    states = Set(0),
    symbols = cfg.symbols,
    alphabets = cfg.nts ++ cfg.symbols.map(_.toString),
	

    trans = {
      val new1: Map[(Int, Option[Symbol], String), Set[(Int, List[String])]] = {
        cfg.nts.flatMap { variable =>
          cfg.rules(variable).map { rhs =>
            (0,None,variable) -> (0,rhs.seq.map(_.toString))
          }
        }.toList.groupBy(_._1).map {
          case (key,values) =>
          key -> values.map(_._2).toSet
        }
      }
      
      val new2: Map[(Int, Option[Symbol], String), Set[(Int, List[String])]] = {
        cfg.symbols.map { sym =>
          ((0,Some(sym),sym.toString),Set((0,List())))
        }.toMap
      }
      
      val new_trans = new1++new2
      new_trans.withDefaultValue(Set())
    },
	
    initState = 0,
    initAlphabet = "S",
    finalStates = Set(),
    )

  // Convert a PDA with empty stacks to a CFG
  def pdaes2cfg(pda: PDA): CFG = CFG(
    nts = { 
      val nts = for {
        state1 <- pda.states
        alpha <- pda.alphabets
        state2 <- pda.states
      } yield state1.toString + alpha + state2.toString
      nts + "S"
    },
	
    symbols = pda.symbols,
	
    start = "S",
	
    rules = {
      def make_rule(src: State, dest: State, stack: List[Alphabet], stset:Set[State]): List[List[Alphabet]] = 
        stack match {
          case List() => List(List())
          case x:: List() => List(List(src.toString + x + dest.toString))
          case x:: rest =>
            val results = for {
              state <- stset
              rhs <- make_rule(state, dest, rest, stset)
            } yield List(src.toString + x + state.toString) ++ rhs
            results.toList
        }
	
      val some_list = pda.trans.toList.filter { case ((_,sym,_),_) => sym.isDefined}
      val none_list = pda.trans.toList.filter { case ((_,sym,_),_) => sym.isEmpty}
    
      val some_rules = for {
        ((src_st, symb, src_alpha), set) <- some_list
        (dest_st, stack) <- set
        state <- pda.states
        result  <- stack match
          case List() => List(Rhs(List(src_st.toString + src_alpha + dest_st.toString):::List(symb.get)))
          case ll => make_rule(dest_st,state,stack, pda.states).map( l => Rhs(List(src_st.toString + src_alpha + state.toString):::List(symb.get) ++ l))
      } yield result
	
      val none_rules = for {
        ((src_st, symb, src_alpha), set) <- none_list
        (dest_st, stack) <- set
        state <- pda.states
        result <- stack match
          case List() => List(Rhs(List(src_st.toString + src_alpha + dest_st.toString)))
          case ll => make_rule(dest_st,state,stack, pda.states).map( l => Rhs(List(src_st.toString + src_alpha + state.toString) ++ l))
      } yield result
	
      val some_map = some_rules.map { rhs =>
        val head1 = rhs.seq.head.toString
        val rest1 = rhs.seq.tail
        (head1,Rhs(rest1))
      }
	  
      val none_map = none_rules.map { rhs =>
        val head2 = rhs.seq.head.toString
        val rest2 = rhs.seq.tail
        (head2,Rhs(rest2))
      }
	
      val total_map = some_map ++ none_map
	
	  // 중복키 값을 포함하며 map 생성
	  // getOrElse는 map에서 키값 가져오는데, 없으면 List() 반환
      val total_rules = total_map.foldLeft(Map.empty[String, List[Rhs]]) {
        case (map, (vari, rhs)) => map + (vari -> (map.getOrElse(vari, List()):+rhs))
      }
	
      val start_rules = pda.states.map { state =>
        Rhs(List(pda.initState.toString+pda.initAlphabet+state.toString))
      }.toList
	  
      val new_rules = total_rules.withDefaultValue(List()) ++ Map("S" -> start_rules)
    
      new_rules
    }
    )
}
