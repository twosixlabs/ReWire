theory iter
imports
  Main
  ReWire.Atmo
begin

definition main :: "'a" where
"main  =
  undefined"

definition f :: "bool \<Rightarrow> bool" where
"f  =
  (\<lambda> x. \<not> x)"

definition start :: "(bool, bool) Dev" where
"start  =
  iter f False"


end