theory wordLOps
imports
  Main
  ReWire.Atmo
begin

definition main :: "'a" where
"main  =
  undefined"

fun compute :: "8 W \<times> 8 W \<Rightarrow> bool"  where
   "compute (v, w) = ((v \<and>w w) \<or> (((v \<or>w w) \<and> (\<not>w w))))"

fun loop :: "8 W \<times> 8 W \<Rightarrow> (8 W \<times> 8 W,
bool,
unit,
8 W \<times> 8 W) Re"  where
   "loop i = ((return (compute i)) \<bind> signal)"

definition start :: "(8 W \<times> 8 W, bool) Dev" where
"start  =
  iterRe loop (lit 0, lit 1)"


end