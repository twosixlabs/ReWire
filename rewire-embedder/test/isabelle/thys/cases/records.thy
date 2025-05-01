theory records
imports
  Main
  ReWire.Atmo
begin

definition main :: "'a" where
"main  =
  undefined"

record SRec =
  f1 :: "Bit"
  f2 :: "Bit"
  f3 :: "Bit \<times> Bit"

fun bar :: "SRec \<Rightarrow> SRec"  where
   "bar s = s⦇f1 := False, f2 := True, f3 := (False, True)⦈"

fun foo :: "SRec \<Rightarrow> SRec"  where
   "foo ⦇f1 = a, f2 = b, f3 = c⦈ = ⦇f1 = b, f2 = a, f3 = c⦈"

fun x :: "SRec \<Rightarrow> Bit"  where
   "x d = f1 d"

fun compute :: "Bit \<Rightarrow> Bit"  where
   "compute _ = x (foo (bar ⦇f1 = False, f2 = False, f3 = (False, False)⦈))"

definition start :: "(Bit, Bit) Dev" where
"start  =
  iter compute False"

fun y :: "SRec \<Rightarrow> Bit"  where
   "y d = f2 d"

fun z :: "SRec \<Rightarrow> Bit \<times> Bit"  where
   "z d = f3 d"


end