theory iterSt
imports
  Main
  ReWire.Atmo
begin

definition main :: "'a" where
"main  =
  undefined"

fun f :: "bool \<Rightarrow> bool \<Rightarrow> bool \<times> bool"  where
   "f i s = (s x\<or> i, s)"

definition loop :: "(bool, bool, bool \<times> unit) StateDev" where
"loop  =
  iterSt f False"

definition start :: "(bool, bool) Dev" where
"start  =
  extrude loop False"


end