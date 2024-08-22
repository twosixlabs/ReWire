theory vectorIndexProxy
imports
  Main
  ReWire.Atmo
begin

definition main :: "'a" where
"main  =
  undefined"

type_synonym Output = "(8, 8 W) Vec"

fun compute :: "(16, (8, 8 W) Vec) Vec \<Rightarrow> (4, 8 W) Vec \<Rightarrow> Output"  where
   "compute v w = map ((\<lambda> v'. (w !v 3) + (v' !v 3))) (rslice 8 v)"

type_synonym Input = "(16, (8, 8 W) Vec) Vec \<times> (4, 8 W) Vec"

fun loop :: "Input \<Rightarrow> Output"  where
   "loop (v, w) = compute v w"

definition start :: "(Input, Output) Dev" where
"start  =
  iter loop (replicate (replicate (lit 0)), replicate (lit 0))"


end