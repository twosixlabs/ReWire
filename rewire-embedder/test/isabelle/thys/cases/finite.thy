theory finite
imports
  Main
  ReWire.Atmo
begin

definition main :: "'a" where
"main  =
  undefined"

fun compute :: "128 fin \<Rightarrow> 20 fin"  where
   "compute n = (toFinite' (fromFinite n :: 7 W)) +% (toFinite (lit 6 :: 3 W))"

definition start :: "(128 fin, 20 fin) Dev" where
"start  =
  iter compute (finite 77)"


end