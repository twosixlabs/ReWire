theory wordSlice
imports
  Main
  ReWire.Atmo
begin

definition main :: "'a" where
"main  =
  undefined"

fun compute :: "16 W \<Rightarrow> 8 W"  where
   "compute w = (case ((msbit w) \<and> (w @! 8)) of
     True \<Rightarrow> (w @@ (7, 0)) | False \<Rightarrow> (w @@ (15, 8)))"

definition start :: "(16 W, 8 W) Dev" where
"start  =
  iter compute (lit 0)"


end