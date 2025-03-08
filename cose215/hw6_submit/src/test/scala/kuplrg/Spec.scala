package kuplrg

import Implementation.*

class Spec extends SpecBase {

  // L = { a^{n^2} | n ≥ 0 }
  {
    val lang = Lang(
      "a".toSet,
      w => math.sqrt(w.length).isWhole,
    )
    check(tm_square.mustValid.mustEqual(lang, 100))
  }

  // L = { a^n | n is a fibonacci number }
  {
    val lang = Lang(
      "a".toSet,
      w => {
        val n = w.length
        def isFib(prev: Int, cur: Int): Boolean =
          if (cur < n) isFib(cur, prev + cur)
          else cur == n
        n == 0 || n == 1 || isFib(1, 2)
      },
    )
    check(tm_fib.mustValid.mustEqual(lang, 100))
  }

  // L = { w \in {a, b, c}* | N_a(w) = N_b(w) = N_c(w) }
  {
    val lang = Lang(
      "abc".toSet,
      w =>
        val a = w.count(_ == 'a')
        val b = w.count(_ == 'b')
        val c = w.count(_ == 'c')
        a == b && b == c,
    )
    check(tm_eq_abc.mustValid.mustEqual(lang, 1_000))
  }

  // f(w ∈ {0, 1}*) = w' where w' = w - 1 if w starts with 1, otherwise f(w) is
  // not defined
  {
    val func: Computer = Computer(
      "01".toSet,
      w =>
        if (w.startsWith("1")) Some((BigInt(w, 2) - 1).toString(2))
        else None,
    )
    check(tm_dec.mustValid.mustEqual(func, 10_000))
  }

  // f(x+y) = z where x, y ∈ {0, 1}* start with 1 and z = x + y
  {
    val pattern = "([01]*)\\+([01]*)".r
    val func: Computer = Computer(
      "01+".toSet,
      _ match
        case pattern(x, y) if x.startsWith("1") && y.startsWith("1") =>
          Some((BigInt(x, 2) + BigInt(y, 2)).toString(2))
        case _ => None,
    )
    check(tm_add.mustValid.mustEqual(func, 20_000))
  }

  /* Write your own tests */
}
