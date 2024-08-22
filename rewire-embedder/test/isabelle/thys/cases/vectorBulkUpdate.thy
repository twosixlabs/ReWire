theory vectorBulkUpdate
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

type_synonym Output = "(8, 8 W) Vec"

type_synonym Input = "(8, 8 W) Vec \<times> (4, 8 W) Vec"

fun compute :: "Input \<Rightarrow> Output"  where
   "compute (v, w) = bulkUpdate v
                                (zip (iterate 4 ((\<lambda> a0. a0 *% 2)) (finite 0))
                                     w)"

fun loop :: "Input \<Rightarrow> (Input, Output, unit, Input) Re"  where
   "loop i = ((return (compute i)) \<bind> signal)"

definition start :: "(Input, Output) Dev" where
"start  =
  iterRe loop (initVec, take initVec)"


end