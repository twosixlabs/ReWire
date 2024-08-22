theory Streams

imports "HOL-Library.Stream" Main

begin

section \<open>Logic of Stream Predicates (SPreds)\<close>

type_synonym 'a SPred = "nat \<Rightarrow> 'a stream \<Rightarrow> bool"

fun stream_pred :: "('a \<Rightarrow> bool) \<Rightarrow> 'a SPred" where
"stream_pred P =  (\<lambda> i s. P (s !! i))"

fun spred_eq :: "'a SPred \<Rightarrow> 'a SPred \<Rightarrow> 'a SPred" (infix "==S" 40) where
"spred_eq SP SQ = (\<lambda> i s. SP i s = SQ i s)"

fun spred_not :: "'a SPred \<Rightarrow> 'a SPred" ("\<not>S") where
"spred_not SP = (\<lambda> i s. (\<not> SP i s))"

fun spred_or :: "'a SPred \<Rightarrow> 'a SPred \<Rightarrow> 'a SPred" (infix "\<or>S" 45) where
"spred_or SP SQ = (\<lambda> i s. SP i s \<or> SQ i s)"

fun spred_and :: "'a SPred \<Rightarrow> 'a SPred \<Rightarrow> 'a SPred" (infix "\<and>S" 46) where
"spred_and SP SQ = (\<lambda> i s. SP i s \<and> SQ i s)"

fun spred_const :: "bool \<Rightarrow> 'a SPred" ("constS") where
"constS b = (\<lambda> i s. b)"

fun STrue :: "'a SPred" where
"STrue _ _ = True"

fun spred_impl :: "'a SPred \<Rightarrow> 'a SPred \<Rightarrow> 'a SPred" (infix "\<Longrightarrow>S" 42) where
"(SP \<Longrightarrow>S SQ) = (\<lambda> i s. SP i s \<longrightarrow> SQ (Suc i) s)"

fun spred_impl_overlap :: "'a SPred \<Rightarrow> 'a SPred \<Rightarrow> 'a SPred" (infix "\<longrightarrow>S" 42) where
"(SP \<longrightarrow>S SQ) = (\<lambda> i s. SP i s \<longrightarrow> SQ i s)"

fun AllS :: "'b \<Rightarrow> ('b \<Rightarrow> 'a SPred) \<Rightarrow> 'a SPred" where
"AllS b f = (\<lambda> i s. f b i s)"


section \<open>System Verilog Assertions (SVA): Sequences and Properties\<close>


subsection \<open>SVA functions\<close>

fun past :: "'a SPred \<Rightarrow> nat \<Rightarrow> 'a SPred" where
"past SP n i = SP (i - n)"

fun future :: "'a SPred \<Rightarrow> nat \<Rightarrow> 'a SPred" where
"future SP n i = SP (i + n)"

fun prev :: "'a SPred \<Rightarrow> 'a SPred" where
"prev SP = past SP 1"

fun nxt :: "'a SPred \<Rightarrow> 'a SPred" where
"nxt SP = future SP 1"

fun stable :: "'a SPred \<Rightarrow> 'a SPred" where
"stable SP = (SP ==S (prev SP))"

fun change :: "'a SPred \<Rightarrow> 'a SPred" where
"change SP = (\<not>S (stable SP))"

fun rose :: "'a SPred \<Rightarrow> 'a SPred" where
"rose SP = (SP \<and>S (\<not>S (prev SP)))"

fun fell :: "'a SPred \<Rightarrow> 'a SPred" where
"fell SP = ((\<not>S SP) \<and>S (prev SP))"

(* countones, onehot, onehot0, and isunknown are all word-specific *)

subsection \<open>SVA Sequences\<close>

fun spred_delay :: "'a SPred \<Rightarrow> nat \<Rightarrow> 'a SPred \<Rightarrow> 'a SPred" (infixr "##[_]" 50) where
"SP ##[n] SQ = (SP \<and>S (future SQ n))"

fun spred_delay_range :: "'a SPred \<Rightarrow> nat \<Rightarrow> nat \<Rightarrow> 'a SPred \<Rightarrow> 'a SPred" where
"spred_delay_range SP m n SQ = undefined"

(*
sequences: 
-- expressions
e
-- variable-length concatentation:
| seq ##N seq | seq ##[*] seq | seq ##[+] seq | seq ##[N:M] seq | seq ##[N:$] seq
-- variable-length repetition:
| seq [*] | seq [+] | seq [*N] | seq [*N:M] | seq [*N:$]
-- other operators:
| seq or seq | seq and seq | e throughout seq | seq intersect seq | seq within seq 
| first_match(seq) | e [ =N] | e [ =N:M] | e [ =N:$] | e [\<rightarrow>N] | e [\<rightarrow>N:M] | e [\<rightarrow>N:$]

*)

subsection \<open>SVA Properties\<close>



(*
Properties:
[Precond] seq
[Precond] not seq
Precond seq Postcond
Precond not seq Postcond

Preconds:
sequence \<mapsto> -- overlapping implication (RHS is evaluated from the same cycle that LHS is true)
sequence |\<Rightarrow> -- non-overlapping implication (RHS is evaluted one clock cycle after LHS is true)

Postconds:
until e
s_until e
until_with e
s_until_with e

 *)


end