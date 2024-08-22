theory wordArith
imports
  Main
  ReWire.Atmo
begin

definition main :: "'a" where
"main  =
  undefined"

fun compute :: "8 W \<Rightarrow> 8 W"  where
   "compute w = ((((w + (lit 1)) ** (lit 2)) * (w - (lit 2))) div (lit 3)) mod (w + (lit 1))"

definition start :: "(8 W, 8 W) Dev" where
"start  =
  iter compute (lit 0)"


end