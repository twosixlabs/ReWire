theory stateTower
imports
  Main
  ReWire.Atmo
begin

definition main :: "'a" where
"main  =
  undefined"

definition incr :: "(Bit, Bit, Bit \<times> Bit \<times> unit, unit) Re" where
"incr  =
  (lift get) \<bind>R ((\<lambda> r0. ((lift (lift get)) \<bind> ((\<lambda> r1. (lift (put r1)) \<then> (lift (lift (put (r0 x\<or> r1)))))))))"

fun sig :: "Bit \<Rightarrow> (Bit,
Bit,
Bit \<times> Bit \<times> unit,
Bit) Re"  where
   "sig i = (((case (\<not> i) of
     True \<Rightarrow> (incr)
   | False \<Rightarrow>
  (return ())) \<then> (lift get)) \<bind> ((\<lambda> r0. signal r0)))"

definition start :: "(bool, bool) Dev" where
"start  =
  extrude (extrude (iterRe sig False) False) True"


end