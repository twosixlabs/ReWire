theory wordShift
imports
  Main
  ReWire.Atmo
begin

fun myRotr :: "8 W \<Rightarrow> 8 W \<Rightarrow> 8 W"  where
   "myRotr w n = (case n mod (lit 8) of
     n' \<Rightarrow> ((w << ((lit 8) - n')) || (w >> n')))"

fun myArithRotr :: "8 W \<Rightarrow> 8 W \<Rightarrow> 8 W"  where
   "myArithRotr w n = (case n mod (lit 8) of
     n' \<Rightarrow> ((w << ((lit 8) - n')) || (w >>> n')))"

definition main :: "'a" where
"main  =
  undefined"

fun compute :: "8 W \<Rightarrow> 8 W"  where
   "compute i = myArithRotr (myRotr i (lit 3)) (lit 5)"

definition start :: "(8 W, 8 W) Dev" where
"start  =
  iter compute (lit 52)"


end