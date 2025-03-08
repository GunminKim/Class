package kuplrg

trait Template {
  def complementDFA(dfa: DFA): DFA
  def intersectDFA(ldfa: DFA, rdfa: DFA): DFA
  def reverseENFA(enfa: ENFA): ENFA
  def reverseRE(re: RE): RE
  def homRE(re: RE, h: Hom): RE
  def ihomDFA(dfa: DFA, h: Hom): DFA
}
