theory wordLit
imports
  Main
  ReWire.Atmo
begin

definition main :: "'a" where
"main  =
  undefined"

fun compute :: "8 W \<Rightarrow> 16 W"  where
   "compute i = (resize i) xor (lit 1)"

definition start :: "(8 W, 16 W) Dev" where
"start  =
  iter compute (lit 255)"


end