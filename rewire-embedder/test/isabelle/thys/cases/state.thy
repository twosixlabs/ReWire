theory state
imports
  Main
  ReWire.Atmo
begin

fun stateAction :: "bool \<Rightarrow> (bool \<times> unit, bool) State"  where
   "stateAction i = (get \<bind> ((\<lambda> s. (put (i x\<or> s)) \<then> (return s))))"

definition main :: "'a" where
"main  =
  undefined"

fun loop :: "bool \<Rightarrow> (bool, bool, bool \<times> unit, bool) Re"  where
   "loop i = ((lift (stateAction i)) \<bind> signal)"

definition start :: "(bool, bool) Dev" where
"start  =
  extrudeDev (iterRe loop True) True"


end