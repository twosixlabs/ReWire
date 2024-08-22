theory vectorCons
imports
  Main
  ReWire.Atmo
begin

definition main :: "'a" where
"main  =
  undefined"

definition initVec :: "(8, 8 W) Vec" where
"initVec  =
  replicate (lit 0)"

type_synonym Output = "(8, 8 W) Vec \<times> (8, 8 W) Vec"

type_synonym Input = "(8, 8 W) Vec \<times> (8, 8 W) Vec"

fun compute :: "Input \<Rightarrow> Output"  where
   "compute (v, w) = (case head v of
     v0 \<Rightarrow> ((case (tail v :: (7, 8 W) Vec) of
     vs \<Rightarrow> ((case last w of
     w7 \<Rightarrow> ((case (init w :: (7, 8 W) Vec) of
     ws \<Rightarrow> ((snoc vs w7, cons v0 ws)))))))))"

definition start :: "(Input, Output) Dev" where
"start  =
  iter compute (initVec, initVec)"


end