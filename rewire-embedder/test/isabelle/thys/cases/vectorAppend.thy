theory vectorAppend
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

type_synonym Output = "(8, 8 W) Vec"

type_synonym Input = "(8, 8 W) Vec \<times> (8, 8 W) Vec"

fun compute :: "Input \<Rightarrow> Output"  where
   "compute (v, w) = (((reverse (take v :: (4, 8 W) Vec)) ++ empty :: (4, 8 W) Vec) ++ (singleton (lit 0)) :: (5,
  8 W) Vec) ++ (drop w :: (3, 8 W) Vec)"

definition start :: "(Input, Output) Dev" where
"start  =
  iter compute (initVec, initVec)"


end