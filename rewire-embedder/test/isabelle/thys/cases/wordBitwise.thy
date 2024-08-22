theory wordBitwise
imports
  Main
  ReWire.Atmo
begin

definition main :: "'a" where
"main  =
  undefined"

fun compute :: "8 W \<times> 8 W \<times> 8 W \<Rightarrow> 8 W"  where
   "compute (u, v, w) = ((u && v) xor w) || (((~~ u) && (~~ v)) ~xor w)"

definition start :: "(8 W \<times> 8 W \<times> 8 W, 8 W) Dev" where
"start  =
  iter compute (lit 0, lit 1, lit 2)"


end