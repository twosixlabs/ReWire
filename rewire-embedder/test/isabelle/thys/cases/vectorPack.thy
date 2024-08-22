theory vectorPack
imports
  Main
  ReWire.Atmo
begin

definition main :: "'a" where
"main  =
  undefined"

definition initVec :: "(8, 8 W) Vec" where
"initVec  =
  map lit (replicate 0 :: (8, int) Vec)"

type_synonym Output = "(8, 8 W) Vec \<times> (8, 8 W) Vec"

type_synonym Input = "(8, 8 W) Vec \<times> (8, 8 W) Vec"

fun compute :: "Input \<Rightarrow> Output"  where
   "compute (v, w) = (case (packlo v w, packhi v w) of
     (lo, hi) \<Rightarrow> ((case (packlo hi lo, packhi hi lo) of
     (lo', hi') \<Rightarrow> ((case (unpacklo lo' hi', unpackhi lo' hi') of
     (v', w') \<Rightarrow> ((v', w')))))))"

definition start :: "(Input, Output) Dev" where
"start  =
  iter compute (initVec, initVec)"


end