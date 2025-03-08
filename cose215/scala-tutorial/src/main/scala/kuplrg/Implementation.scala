package kuplrg

object Implementation extends Template {

  def sqsum(x: Int, y: Int): Int = x*x + y*y

  def concat(left: String, right: String): String = left + right

  def subN(n: Int): Int => Int = _ - n

  def twice(f: Int => Int): Int => Int = x => f(f(x))

  def compose(f: Int => Int, g: Int => Int): Int => Int = x => f(g(x))

  def sumOnlyOdd(l: List[Int]): Int = l.filter(_ % 2 == 1).sum

  def foldWith(f: (Int, Int) => Int): List[Int] => Int = l => l.foldLeft(0)(f)

  def toSet(l: List[Int], from: Int): Set[Int] = l.drop(from).toSet

  def getOrZero(map: Map[String, Int], key: String): Int = map.get(key).getOrElse(0)

  def setMinus(s1: Set[Int], s2: Set[Int]): Set[Int] = s1.diff(s2)

  // ---------------------------------------------------------------------------
  // Trees
  // ---------------------------------------------------------------------------
  import Tree.*

  def has(value: Int): Tree => Boolean = x => x match {
	case Leaf(n) if n==value => true
	case Leaf(_) =>false
	case Branch(l,n,r) if n==value => true
	case Branch(l,_,r) => has(value)(l) || has(value)(r)
	}

  def maxDepthOf(value: Int): Tree => Option[Int] = {
  def search_with_d(cur: Tree, d: Int): Option[Int] = cur match {
    case Leaf(n) => if (n == value) Some(d) else None
    case Branch(l, n, r) => 
      val ld = search_with_d(l, d + 1)
      val rd = search_with_d(r, d + 1)
      (ld, rd) match {
        case (None, None) => if (n == value) Some(d) else None
        case (lds, rds) => 
          val depth = List(lds, rds).flatten
          if (n == value) Some((d :: depth).max) else Some(depth.max)
      }
  }
  search_with_d(_, 0)
}

  def mul(t: Tree): Int = t match {
	case Leaf(n) => 1*n
	case Branch(l,n,r) => 1*mul(l)*n*mul(r)
	}

  def countLeaves(t: Tree): Int = t match {
	case Leaf(_) => 1
	case Branch(left, _, right) => countLeaves(left) + countLeaves(right)
	}


  def postOrder(t: Tree): List[Int] = t match {
	case Leaf(n) => n :: Nil
	case Branch(l,n,r) => postOrder(l) ++ postOrder(r) ++ List(n)
	}

  // ---------------------------------------------------------------------------
  // Boolean Expressions
  // ---------------------------------------------------------------------------
  import BE.*

  def countLiterals(expr: BE): Int = expr match {
	case True => 1
	case False => 1
	case And(be1,be2) => countLiterals(be1) + countLiterals(be2)
	case Or(be1,be2) => countLiterals(be1) + countLiterals(be2)
	case Not(be) => countLiterals(be)
	}

  def countNots(expr: BE): Int = expr match {
	case Not(be) => 1 + countNots(be)
	case And(be1,be2) => countNots(be1) + countNots(be2)
	case Or(be1,be2) => countNots(be1) + countNots(be2)
	case _ => 0
	}
	
  def depth(expr: BE): Int = expr match {
	case True => 0
	case False => 0
	case And(be1,be2) => if(1+depth(be1)>1+depth(be2)) 1+depth(be1) else 1+depth(be2)
	case Or(be1,be2) => if(1+depth(be1)>1+depth(be2)) 1+depth(be1) else 1+depth(be2)
	case Not(be) => 1+depth(be)
	}

  def getString(expr: BE): String = expr match {
	case True => "true"
	case False => "false"
	case And(be1,be2) => "(" + getString(be1) + " & " + getString(be2)+")"
	case Or(be1,be2) => "(" + getString(be1) + " | " + getString(be2) + ")"
	case Not(be) => "!"+getString(be)
	}

  def eval(expr: BE): Boolean = expr match {
	case True => true
	case False => false
	case And(be1,be2) => if(eval(be1) && eval(be2)) eval(True) else eval(False)
	case Or(be1,be2) => if(eval(be1) || eval(be2)) eval(True) else eval(False)
	case Not(be) => if(eval(be)) eval(False) else eval(True)
	}
	
}
