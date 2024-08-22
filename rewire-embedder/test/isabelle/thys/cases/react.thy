theory react
imports
  Main
  ReWire.Atmo
begin

definition main :: "'a" where
"main  =
  undefined"

fun loop :: "bool \<Rightarrow> (bool, bool, unit, bool) Re"  where
   "loop i = ((return ((\<not> i))) \<bind> signal)"

definition device :: "bool \<Rightarrow> (bool, bool) Dev" where
"device  =
  iterRe loop"

definition start :: "(bool, bool) Dev" where
"start  =
  device True"


end