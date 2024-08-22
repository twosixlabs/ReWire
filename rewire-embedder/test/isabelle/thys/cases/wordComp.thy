theory wordComp
imports
  Main
  ReWire.Atmo
begin

definition main :: "'a" where
"main  =
  undefined"

fun compute :: "8 W \<times> 8 W \<Rightarrow> bool"  where
   "compute (v,
             w) = ((((v < w) \<and> ((\<not> (v >= w))))) \<or> ((((v = w)) \<or> (((v > w) \<and> ((\<not> (v <= w))))))))"

definition start :: "(8 W \<times> 8 W, bool) Dev" where
"start  =
  iter compute (lit 0, lit 1)"


end