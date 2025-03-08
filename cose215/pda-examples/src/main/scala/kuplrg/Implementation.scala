package kuplrg

object Implementation extends Template {

  /** This is the playground for you to run your implementation. Do whatever you
    * want here and run `sbt run` to see the result.
    */
  @main def playground: Unit = {
    println("------------------- PLAYGROUND -------------------")

    // You can check your implementation here.
    val accept = pda_abc_ij_ik_final.acceptByFinalState
    println(s"pda_abc_ij_ik_final.acceptByFinalState(\"b\")   = ${accept("b")}")
    println(s"pda_abc_ij_ik_final.acceptByFinalState(\"aacc\")  = ${accept("aacc")}")
    println(s"pda_abc_ij_ik_final.acceptByFinalState(\"aaabbbcc\") = ${accept("aaabbbcc")}")
    println(s"pda_abc_ij_ik_final.acceptByFinalState(\"aaabbccc\") = ${accept("aaabbccc")}")
    println(s"pda_abc_ij_ik_final.acceptByFinalState(\"c\") = ${accept("c")}")

    println("--------------------------------------------------")
  }

  // PDA accpeting L = { a^n b^n | n >= 0 } by final states
  val pda_an_bn_final: PDA = PDA(
    states = Set(0, 1, 2),
    symbols = Set('a', 'b'),
    alphabets = Set("X", "Z"),
    trans = Map(
      (0, Some('a'), "Z") -> Set((0, List("X", "Z"))),
      (0, Some('a'), "X") -> Set((0, List("X", "X"))),
      (0, None, "Z") -> Set((1, List("Z"))),
      (0, None, "X") -> Set((1, List("X"))),
      (1, Some('b'), "X") -> Set((1, List())),
      (1, None, "Z") -> Set((2, List("Z"))),
    ).withDefaultValue(Set()),
    initState = 0,
    initAlphabet = "Z",
    finalStates = Set(2),
  )

  // PDA accepting L = { w w^R | w \in {0, 1}* } by final states
  def pda_even_pal_final: PDA = PDA(
    states = Set(0,1,2),
    symbols = Set('0','1'),
    alphabets = Set("X","Y","Z"),
    trans = Map(
      (0, Some('0'), "Z") -> Set((0, List("X", "Z"))),
      (0, Some('0'), "X") -> Set((0, List("X", "X"))),
      (0, Some('0'), "Y") -> Set((0, List("X", "Y"))),
      (0, Some('1'), "Z") -> Set((0, List("Y", "Z"))),
      (0, Some('1'), "X") -> Set((0, List("Y", "X"))),
      (0, Some('1'), "Y") -> Set((0, List("Y", "Y"))),
      (0, None, "Z") -> Set((1, List("Z"))),
      (0, None, "X") -> Set((1, List("X"))),
      (0, None, "Y") -> Set((1, List("Y"))),
      (1, Some('0'), "X") -> Set((1, List())),
      (1, Some('1'), "Y") -> Set((1, List())),
      (1, None, "Z") -> Set((2, List("Z"))),
    ).withDefaultValue(Set()),
    initState = 0,
    initAlphabet = "Z",
    finalStates = Set(2),
	)

  // PDA accpeting L = { w \in {a, b}* | N_a(w) > N_b(w) } by empty stacks
  def pda_more_as_empty: PDA = PDA(
    states = Set(0,1),
    symbols = Set('a','b'),
    alphabets = Set("Z","P","N"),
    trans = Map(
      (0, Some('a'), "Z") -> Set((0, List("P", "Z"))),
      (0, Some('a'), "P") -> Set((0, List("P", "P"))),
      (0, Some('a'), "N") -> Set((0, List())),
      (0, Some('b'), "Z") -> Set((0, List("N", "Z"))),
      (0, Some('b'), "P") -> Set((0, List())),
      (0, Some('b'), "N") -> Set((0, List("N","N"))),
      (0, None, "P") -> Set((1, List())),
      (1, None, "Z") -> Set((1, List())),
      (1, None, "P") -> Set((1, List())),
      (1, None, "N") -> Set((1, List())),
    ).withDefaultValue(Set()),
    initState = 0,
    initAlphabet = "Z",
    finalStates = Set(),
    )

  // PDA accepting L = { a^i b^j c^k | i, j, k >= 0 and (i = j or i = k) } by
  // final states
  def pda_abc_ij_ik_final: PDA = PDA(
    states = Set(0,1,2,3,4,5,6),
    symbols = Set('a','b','c'),
    alphabets = Set("Z","X"),
    trans = Map(
      (0, Some('a'), "Z") -> Set((0, List("X", "Z"))),
      (0, Some('a'), "X") -> Set((0, List("X", "X"))),
      (0, None, "Z") -> Set((1, List("Z")),(4, List("Z"))),
      (0, None, "X") -> Set((1, List("X")),(4, List("X"))),
      
      (1, Some('b'), "X") -> Set((1,List())),
      (1, None, "Z") -> Set((2,List("Z"))),
      (2, Some('c'), "Z") -> Set((2,List("Z"))),
      (2, Some('c'), "X") -> Set((2,List("X"))),
      (2, None, "Z") -> Set((3,List("Z"))),
      
      (4, Some('b'), "Z") -> Set((4,List("Z"))),
      (4, Some('b'), "X") -> Set((4,List("X"))),
      (4, None, "Z") -> Set((5,List("Z"))),
      (4, None, "X") -> Set((5,List("X"))),
      (5, Some('c'), "X") -> Set((5,List())),
      (5, None, "Z") -> Set((6,List("Z"))),
      
      
    ).withDefaultValue(Set()),
    initState = 0,
    initAlphabet = "Z",
    finalStates = Set(3,6),
    )
  
  // PDA accepting L = { a^i b^j | (i = 2n + 1 and j = 3n + 1) for n >= 0 } by
  // empty stacks
  def pda_a2n1_b3n1_empty: PDA = PDA(
    states = Set(0,1,2,3,4,5,6,7,8,9),
    symbols = Set('a','b'),
    alphabets = Set("Z","X"),
    trans = Map(
      (0, Some('a'), "Z") -> Set((1, List("X", "Z"))),
      (1, Some('a'), "X") -> Set((2, List("X"))),
      (2, Some('a'), "X") -> Set((3, List("X", "X"))),
      (3, None, "X") -> Set((1, List("X"))),
      (3, None, "X") -> Set((4, List("X"))),
      (1, None, "X") -> Set((3, List("X"))),
      (4, Some('b'), "X") -> Set((5, List("X"))),
      (5, Some('b'), "X") -> Set((6, List("X"))),
      (6, Some('b'), "X") -> Set((7, List())),
      (7, None, "X") -> Set((4, List("X"))),
      (4, None, "X") -> Set((7, List("X"))),
      (7, Some('b'), "X") -> Set((8, List())),
      (8, None, "Z") -> Set((9, List())),
    ).withDefaultValue(Set()),
    initState = 0,
    initAlphabet = "Z",
    finalStates = Set(),
    )

  // PDA accpeting L = { a^i b^j c^k | i, j, k >= 0 and j = i + 2k } by final
  // states
  def pda_abc_j_i2k_final: PDA = PDA(
    states = Set(0,1,2,3,4),
    symbols = Set('a','b','c'),
    alphabets = Set("Z","X","Y"),
    trans = Map(
      (0, Some('a'), "Z") -> Set((0, List("X", "Z"))),
      (0, Some('a'), "X") -> Set((0, List("X", "X"))),
      (0, None, "Z") -> Set((1, List("Z"))),
      (0, None, "X") -> Set((1, List("X"))),
	  
      (1, Some('b'), "X") -> Set((1, List())),
      (1, Some('b'), "Z") -> Set((1, List("Y", "Z"))),
      (1, Some('b'), "Y") -> Set((1, List("Y", "Y"))),
      (1, None, "Z") -> Set((2, List("Z"))),
      (1, None, "Y") -> Set((2, List("Y"))),
	  
      (2, Some('c'), "Y") -> Set((3, List())),
      (3, None, "Y") -> Set((2, List())),
      (2, None, "Z") -> Set((4, List("Z"))),  
    ).withDefaultValue(Set()),
    initState = 0,
    initAlphabet = "Z",
    finalStates = Set(4),
    )

  // PDA accpeting L = { w \in { '(', ')', '{', '}', '[', ']' }* | w is
  // well-formed and satisfies the order: '()' <= '{}' <= '[]' } by empty stacks
  def pda_ord_brace_empty: PDA = PDA(
    states = Set(0),
    symbols = Set('(', ')', '{', '}', '[', ']'),
    alphabets = Set("(", "{", "["),
    trans = Map(
      (0, Some('('), "Z") -> Set((0, List("(", "Z"))),
      (0, Some('('), "(") -> Set((0, List("(", "("))),
      (0, Some('('), "{") -> Set((0, List("(", "{"))),
      (0, Some('('), "[") -> Set((0, List("(", "["))),
      (0, Some(')'), "(") -> Set((0, List())),
      (0, Some('{'), "Z") -> Set((0, List("{", "Z"))),
      (0, Some('{'), "{") -> Set((0, List("{", "{"))),
      (0, Some('{'), "[") -> Set((0, List("{", "["))),
      (0, Some('}'), "{") -> Set((0, List())),
      (0, Some('['), "Z") -> Set((0, List("[", "Z"))),
      (0, Some('['), "[") -> Set((0, List("[", "["))),
      (0, Some(']'), "[") -> Set((0, List())),
      (0, None, "Z") -> Set((0, List()))
    ).withDefaultValue(Set()),
    initState = 0,
    initAlphabet = "Z",
    finalStates = Set(),
    )

  // PDA accpeting L = { w \in { '0', '1', '+', '*' }* | w is an arithmetic
  // expression evaluated to an even number } by final states
  def pda_ae_even_final: PDA = PDA(
    states = Set(0,1,2,3,4,5,6,7,8,9),
    symbols = Set('0','1','+','*'),
    alphabets = Set("Z","1","O","+","*"),
    trans = Map(
      (0, Some('1'), "Z") -> Set((0, List("1", "Z"))),
      (0, Some('1'), "+") -> Set((0, List("1", "+"))),
      (0, Some('0'), "Z") -> Set((0, List("0", "Z"))),
      (0, Some('0'), "+") -> Set((0, List("0", "+"))),
      (0, Some('+'), "1") -> Set((0, List("+", "1"))),
      (0, Some('+'), "0") -> Set((0, List("+", "0"))),
      
      (0, Some('*'), "1") -> Set((1, List("*", "1"))),
      (1, None, "*") -> Set((1, List())),
      (1, None, "1") -> Set((1, List())),
      (1, None, "Z") -> Set((2, List("Z"))),
      (1, None, "+") -> Set((2, List("+"))),
      (2, Some('1'), "+") -> Set((3, List("1", "+"))),
      (2, Some('1'), "Z") -> Set((3, List("1", "Z"))),
      (2, Some('0'), "+") -> Set((4, List("0", "+"))),
      (2, Some('0'), "Z") -> Set((4, List("0", "Z"))),
      
      (0, Some('*'), "0") -> Set((5, List("*", "0"))),
      (5, None, "*") -> Set((5, List())),
      (5, None, "0") -> Set((5, List())),
      (5, None, "Z") -> Set((6, List("Z"))),
      (5, None, "+") -> Set((6, List("+"))),
      (6, Some('1'), "+") -> Set((4, List("0", "+"))),
      (6, Some('1'), "Z") -> Set((4, List("0", "Z"))),
      (6, Some('0'), "+") -> Set((4, List("0", "+"))),
      (6, Some('0'), "Z") -> Set((4, List("0", "Z"))),
      
      (3, None, "1") -> Set((0, List("1"))),
      (4, None, "0") -> Set((0, List("0"))),
      
      (0, None, "1") -> Set((7, List("1"))),
      (0, None, "0") -> Set((7, List("0"))),
      
      (7, None, "0") -> Set((7, List())),
      (7, None, "+") -> Set((7, List())),
      (7, None, "1") -> Set((8, List())),
      
      (8, None, "0") -> Set((8, List())),
      (8, None, "+") -> Set((8, List())),
      (8, None, "1") -> Set((7, List())),
    
      (7, None, "Z") -> Set((9, List("Z"))),
    ).withDefaultValue(Set()),
    initState = 0,
    initAlphabet = "Z",
    finalStates = Set(9),
    )

  // PDA accpeting L = { a_1 a_2 ... a_2n \in {a, b}* | n >= 1 and a_i = a_{n+i}
  // for some 1 <= i <= n } by empty stacks
  def pda_eq_pair_empty: PDA = PDA(
    states = Set(0,1,2,3,4,5,6,7),
    symbols = Set('a','b'),
    alphabets = Set("Z","X"),
    trans = Map(
      (0, Some('a'), "Z") -> Set((0, List("X", "Z")), (1, List("Z"))),
      (0, Some('b'), "Z") -> Set((0, List("X", "Z")), (4, List("Z"))),
      (0, Some('a'), "X") -> Set((0, List("X", "X")), (1, List("X"))),
      (0, Some('b'), "X") -> Set((0, List("X", "X")), (4, List("X"))),
      
      (1, Some('a'), "X") -> Set((1, List())),
      (1, Some('b'), "X") -> Set((1, List())),
      (1, None, "Z") -> Set((2, List("Z"))),
      
      (2, Some('a'), "Z") -> Set((2, List("X","Z")),(3, List("Z"))),
      (2, Some('a'), "X") -> Set((2, List("X","X")),(3, List("X"))),
      (2, Some('b'), "Z") -> Set((2, List("X","Z"))),
      (2, Some('b'), "X") -> Set((2, List("X","X"))),
      
      (3, Some('a'), "X") -> Set((3, List())),
      (3, Some('b'), "X") -> Set((3, List())),
      (3, None, "Z") -> Set((7, List())),
      
      (4, Some('a'), "X") -> Set((4, List())),
      (4, Some('b'), "X") -> Set((4, List())),
      (4, None, "Z") -> Set((5, List("Z"))),
      
      (5, Some('a'), "Z") -> Set((5, List("X","Z"))),
      (5, Some('a'), "X") -> Set((5, List("X","X"))),
      (5, Some('b'), "Z") -> Set((5, List("X","Z")),(6, List("Z"))),
      (5, Some('b'), "X") -> Set((5, List("X","X")),(6, List("X"))),
      
      (6, Some('a'), "X") -> Set((6, List())),
      (6, Some('b'), "X") -> Set((6, List())),
      (6, None, "Z") -> Set((7, List())),
      
    ).withDefaultValue(Set()),
    initState = 0,
    initAlphabet = "Z",
    finalStates = Set(),
    )
}
