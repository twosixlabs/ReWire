theory Atmo

imports
Main
"CVec"
Rewire
Word_Lib.Reversed_Bit_Lists
Word_Lib.More_Word_Operations
Word_Lib.Generic_set_bit
begin

term word_cat
(* Prelude *)
(* bool, datatypes, functions *)

instantiation bool :: zero
begin
definition zero_bool :: bool where
"zero_bool = False"
instance ..
end

fun uncurry :: "('a \<Rightarrow> 'b \<Rightarrow> 'c) \<Rightarrow> 'a \<times> 'b \<Rightarrow> 'c" where
"uncurry f (a,b) = f a b"

fun xorb :: "bool \<Rightarrow> bool \<Rightarrow> bool" (infix "x\<or>" 60) where
"a x\<or> b = ((a \<and> \<not> b) \<or> (\<not> a \<and> b))"
fun nand :: "bool \<Rightarrow> bool \<Rightarrow> bool" (infix "~\<and>" 60) where
"a ~\<and> b = (\<not> (a \<and> b))"
fun nor :: "bool \<Rightarrow> bool \<Rightarrow> bool"  (infix "~\<or>" 60) where
"a ~\<or> b = (\<not> (a \<or> b))"
fun xnor :: "bool \<Rightarrow> bool \<Rightarrow> bool" (infix "~x\<or>" 60) where
"a ~x\<or> b = (\<not> (a x\<or> b))"

(* Monad *)

type_synonym ('i,'o) Dev = "('i,'o,unit) Re_INF"
type_synonym ('i,'o,'s) StateDev = "('i,'o,'s) Re_INF"

notation liftS ("lift")
notation liftR ("lift")
notation state_bind (infix "\<bind>" 50)
notation R_bind (infix "\<bind>" 50)
notation state_bind' (infix "\<then>" 50)
notation R_bind' (infix "\<then>" 50)
notation returnR ("return")
notation returnS ("return")

definition extrudeStateDev :: "('i,'o,'s \<times> 't) StateDev \<Rightarrow> 's \<Rightarrow> ('i,'o,'t) StateDev" where
"extrudeStateDev = extrude"

definition extrudeDev :: "('i,'o,'s \<times> unit) StateDev \<Rightarrow> 's \<Rightarrow> ('i,'o) Dev" where
"extrudeDev = extrude"



(* Word *)

type_synonym Bit = bool
type_synonym 'a W = "'a word"

fun resize :: "('a::len) word \<Rightarrow> ('b::len) word" where
"resize a = UCAST('a \<rightarrow> 'b) a"

notation word_of_int ("lit")
notation word_cat (infix "++w" 60)

text \<open>Word syntax\<close>
context
  includes bit_operations_syntax
begin

abbreviation
  wordNOT  :: "'a::len word \<Rightarrow> 'a word"      ("~~ _" [70] 71)
where
  "~~ x == NOT x"

abbreviation
  wordAND  :: "'a::len word \<Rightarrow> 'a word \<Rightarrow> 'a word" (infixr "&&" 64)
where
  "a && b == a AND b"

abbreviation
  wordOR   :: "'a::len word \<Rightarrow> 'a word \<Rightarrow> 'a word" (infixr "||"  59)
where
  "a || b == a OR b"

abbreviation
  wordXOR  :: "'a::len word \<Rightarrow> 'a word \<Rightarrow> 'a word" (infixr "xor" 59)
where
  "a xor b == a XOR b"

fun wordXNOR :: "'a::len word \<Rightarrow> 'a word \<Rightarrow> 'a word" (infixr "~xor" 59) where
"a ~xor b = NOT (a XOR b)"
                 
abbreviation
  wordExp :: "'a::len word \<Rightarrow> 'a word \<Rightarrow> 'a word" (infixr "**" 68)
where
  "a ** b == a ^ (unat b)"

end


(* Word_Lib.Bits_Shift_Infix_Syntax. *)
no_notation shiftl (infixl "<<" 55)
no_notation shiftr (infixl ">>" 55)
no_notation sshiftr (infixl ">>>" 55)
fun word_shiftl :: "'a::len word \<Rightarrow> 'a word \<Rightarrow> 'a word" (infixl "<<" 55) where
"word_shiftl w n = shiftl w (unat n)"
fun word_shiftr :: "'a::len word \<Rightarrow> 'a word \<Rightarrow> 'a word" (infixl ">>" 55) where
"word_shiftr w n = shiftr w (unat n)"
fun word_sshiftr :: "'a::len word \<Rightarrow> 'a word \<Rightarrow> 'a word" (infixl ">>>" 55) where
"word_sshiftr w n = sshiftr w (unat n)"

notation msb ("msbit")

fun slice_word :: "'a::len word \<Rightarrow> int \<times> int \<Rightarrow> 'b::len word" (infix "@@" 65) where
"w @@ (j,i) = Word.slice (nat i) w"

fun index_word :: "'a::len word \<Rightarrow> int \<Rightarrow> bool" (infix "@!" 65) where
"w @! n = bit w (nat n)"

fun index_word_proxy :: "'a::len word \<Rightarrow> nat \<Rightarrow> bool" (infix "!w" 65) where
"w !w n = bit w n"


fun example1 :: "'a::len word \<Rightarrow> int \<Rightarrow> bool" where
"example1 w n = bit w (nat n)"

fun example2 :: "'a::len word \<Rightarrow> nat \<Rightarrow> nat \<Rightarrow> 'b::len word" where
"example2 w n _ = Word.slice n w"


(* New word operations *)

fun word_foldl :: "('a \<Rightarrow> bool  \<Rightarrow> 'a) \<Rightarrow> 'a \<Rightarrow> 'b::len word \<Rightarrow> 'a" where
"word_foldl f a w = foldl f a (to_bl w)"
fun word_foldr :: "(bool \<Rightarrow> 'a \<Rightarrow> 'a) \<Rightarrow> 'b::len word \<Rightarrow> 'a \<Rightarrow> 'a" where
"word_foldr f w  = foldr f (to_bl w)"

fun rAnd :: "'a::len word \<Rightarrow> bool" where
"rAnd w = word_foldl (\<and>) True w"
fun rOr :: "'a::len word \<Rightarrow> bool" where
"rOr w = word_foldl (\<or>) False w"
fun rXOr :: "'a::len word \<Rightarrow> bool" where
"rXOr w = word_foldl (x\<or>) False w"
fun rNAnd :: "'a::len word \<Rightarrow> bool" where
"rNAnd w = word_foldl nand False w"
fun rNor :: "'a::len word \<Rightarrow> bool" where
"rNor w = word_foldl nor True w"
fun rXNor :: "'a::len word \<Rightarrow> bool" where
"rXNor w = word_foldl xnor True w"

fun lor :: "'a::len word \<Rightarrow> 'a word \<Rightarrow> bool" (infix "\<or>w" 60) where
"lor v w = ((v > 0) \<or> (w > 0))"
fun land :: "'a::len word \<Rightarrow> 'a word \<Rightarrow> bool" (infix "\<and>w" 60) where
"land v w = ((v > 0) \<and> (w > 0))"
fun lnot :: "'a::len word \<Rightarrow> bool" ("\<not>w") where
"\<not>w v = (\<not> (v > 0))"




(* Finites *)

(* We'll embed Finites in words *)
(* '4 fin'='4 word' includes the numbers 0 = 0001, 1 = 0010, 2 = 0100, 3=1000  *)

type_synonym 'a fin = "'a word"

fun finite :: "int \<Rightarrow> 'a::len fin" where
"finite n = lit (2 ^ (nat n))"

fun fromFinite :: "'a::len fin \<Rightarrow> ('b::len) word" where
"fromFinite n = word_of_nat (word_log2 n)"

fun toFinite :: "'a::len word \<Rightarrow> ('b::len) fin" where
"toFinite w = finite (int (unat w))"

fun toFinite' :: "'a::len word \<Rightarrow> ('b::len) fin" where
"toFinite' w = finite (int (unat w) mod LENGTH('b))"

definition minBound :: "'a::len fin" where
"minBound = word_of_nat 1"

definition maxBound :: "'a::len fin" where
"maxBound = finite (int (LENGTH('a) - 1))"

fun val_fin :: "'a::len fin \<Rightarrow> int" where
"val_fin n = word_log2 n"

fun fin_binop :: "(int \<Rightarrow> int \<Rightarrow> int) \<Rightarrow> ('a::len fin \<Rightarrow> 'a fin \<Rightarrow> 'a fin)" where
"fin_binop f m n = lit ((f (val_fin m) (val_fin n)) mod LENGTH('a))"

fun plus_fin :: "('a::len fin \<Rightarrow> 'a fin \<Rightarrow> 'a fin)" (infix "+%" 60) where
"plus_fin m n = (fin_binop (+)) m n"

fun minus_fin :: "('a::len fin \<Rightarrow> 'a fin \<Rightarrow> 'a fin)" (infix "-%" 60) where
"minus_fin m n = (fin_binop (-)) m n"

fun times_fin :: "('a::len fin \<Rightarrow> 'a fin \<Rightarrow> 'a fin)" (infix "*%" 62) where
"times_fin m n = (fin_binop (*)) m n"

fun div_fin :: "('a::len fin \<Rightarrow> 'a fin \<Rightarrow> 'a fin)" (infix "div%" 62) where
"div_fin m n = (fin_binop (div)) m n"

fun even_fin :: "'a::len fin \<Rightarrow> bool" where
"even_fin n = even (val_fin n)"

fun odd_fin :: "'a::len fin \<Rightarrow> bool" where
"odd_fin n = odd (val_fin n)"

notation even_fin ("even")
notation odd_fin ("odd")

fun fin_of_nat :: "nat \<Rightarrow> 'a::len fin" where
"fin_of_nat n = lit (2 ^ n)"


fun update_word_fin :: "'a::len word \<Rightarrow> 'a fin \<Rightarrow> Bit \<Rightarrow> 'a word" ("update") where
"update_word_fin w i b = set_bit w (nat (val_fin i)) b"





(* Vectors *)

notation cvec_of_list ("fromList")

type_synonym ('a,'b) Vec = "('a,'b) cvec"

definition generate_cvec :: "('len::len fin \<Rightarrow> 'a) \<Rightarrow> ('len,'a) Vec" ("generate") where
"generate f = cvec (f o fin_of_nat)"

fun update_cvec_fin ::  "('len::len,'a) cvec \<Rightarrow> 'len fin \<Rightarrow> 'a \<Rightarrow> ('len,'a) cvec" ("update") where
"update_cvec_fin v i = update_cvec v (nat (val_fin i))"

fun lit_numeral_cvec :: "int \<Rightarrow> ('b :: len, Bit) Vec" where
"lit_numeral_cvec n = generate (\<lambda> i. (n div 2 ^ (nat (val_fin i) + 1)) mod 2 = 1)"

definition lastIndex_cvec :: "('b::len,'a) cvec \<Rightarrow> 'b fin" ("lastIndex") where
"lastIndex_cvec _ \<equiv> maxBound"

definition lastIndexNat_cvec :: "('b::len,'a) cvec \<Rightarrow> nat" ("lastIndexNat") where
"lastIndexNat_cvec _ \<equiv> LENGTH('b) - 1"

definition cvec_index_fin :: "('len::len,'a) cvec \<Rightarrow> 'len fin \<Rightarrow> 'a" (infix "!vf" 100) where
"cvec_index_fin v i \<equiv> v !v nat (val_fin i)"

definition foldl_cvec :: "('a \<Rightarrow> 'b \<Rightarrow> 'a) \<Rightarrow> 'a \<Rightarrow> ('len::len0,'b) Vec \<Rightarrow> 'a" where
"foldl_cvec f a v \<equiv> foldl f a (list_of_cvec v)"

definition bulkUpdate :: "('n::len,'a) Vec \<Rightarrow> ('m::len0, 'n fin \<times> 'a) Vec \<Rightarrow> ('n,'a) Vec" where
"bulkUpdate v ps \<equiv> foldl_cvec (\<lambda> v (i,a). update_cvec_fin v i a) v ps"




(* Additional overloading and RWUser operations *)

fun index_word_fin :: "'a::len word \<Rightarrow> 'a fin \<Rightarrow> bool" (infix "!wf" 65) where
"w !wf n = bit w (nat (val_fin n))"

definition lastIndex_word :: "'b::len word \<Rightarrow> 'b fin" ("lastIndex") where
"lastIndex_word _ \<equiv> maxBound"

definition lastIndexNat_word :: "'b::len word \<Rightarrow> nat" ("lastIndexNat")  where
"lastIndexNat_word _ \<equiv> LENGTH('b) - 1"

(*
definition map_cvec_to_word :: "('a \<Rightarrow> bool) \<Rightarrow> ('len::len,'a) cvec \<Rightarrow> 'len word" ("map") where
  "map_cvec f v \<equiv> bits (\<lambda>i. f (v !v i))"
*)

definition map_word_to_cvec :: "(bool \<Rightarrow> 'b) \<Rightarrow>  'len::len word \<Rightarrow> ('len, 'b) cvec" ("map") where
  "map_word_to_cvec f v \<equiv> cvec (\<lambda>i. f (v !w i))"


fun slice_word_fin :: "'a::len fin \<Rightarrow> 'a word \<Rightarrow> 'c::len word" ("slicew") where
"slice_word_fin i v = word_reverse (Word.slice (nat (val_fin i)) (word_reverse v))"

fun take_word :: "'a::len word \<Rightarrow> 'b::len word" ("takew") where
"take_word a = slicew (finite 0) a"

fun slice_word_nat :: "nat \<Rightarrow> 'a::len word \<Rightarrow> 'c::len word" ("slice") where
"slice_word_nat n w = word_reverse (Word.slice n (word_reverse w))"

fun take_word_nat :: "'a::len word \<Rightarrow> 'b::len word" ("take") where
"take_word_nat w = slice 0 w"

(*
{-# INLINE bulkUpdate #-}
bulkUpdate :: KnownNat n => Vec n a -> Vec m (Finite n,a) -> Vec n a
bulkUpdate = rwPrimVecBulkUpdate

rwPrimVecBulkUpdate v a = V.update v (V.map (BF.first fromEnum) a)
 *)

(*
record Globals =
  X :: nat
  Y :: nat
  Z :: nat

value "\<lparr> X = 3, Y = 6, Z = 7 \<rparr>"

definition "rec = \<lparr> X = 3, Y = 6, Z = 7 \<rparr>"
  
fun test3 :: "Globals \<Rightarrow> nat \<Rightarrow> nat" where
"test3 g n = n + Globals.X g "

value "test3 rec 39"
*)

(* notion:  (Slice i w l :: 'a word) \<equiv> slice i w :: l word  *)

(* 

nonterminal wpattern and wpatterns and wcases
syntax
 "_wpattern"     :: "[type, wpattern, wpatterns] \<Rightarrow> wpattern" ("_w.'(_,/ _')")
 "_wpatterns"    :: "[type, wpattern, wpatterns] \<Rightarrow> wpatterns" ("_w.'(_,/ _')")
 ""             :: "wpattern \<Rightarrow> wpatterns" ("_")
 "_var_wpattern" :: "[type, pttrn] \<Rightarrow> wpattern" ("_w.
 "_lit_wpattern" :: "[type, int] \<Rightarrow> wpattern" ("_w._" 100)
 "_wcase_block"  :: "['x word, wcases, 'a] \<Rightarrow> 'a" ("'( wcase _ of// _ else// _ ')")

nonterminal weirds
syntax
  "_weird_block" :: "weirds \<Rightarrow> nat" ("\<lparr>/ '(_')/ \<rparr>")
  "_weirds" :: "bool \<Rightarrow> weirds \<Rightarrow> weirds" ("_,/ _")
  "_weird" :: "bool \<Rightarrow> weirds" ("_")

translations
  "_weird_block (_weirds w ws)" \<rightharpoonup> "(if w then 1 :: nat else 0 :: nat) + (2 :: nat) * _weird_block ws"
  "_weird_block (_weird w)" \<rightharpoonup> "(if w then 1 else 0)"

value "\<lparr> ( False , True, True, False, True ) \<rparr>"

fun silly :: "'a \<Rightarrow> 'b \<Rightarrow> 'c \<Rightarrow> 'a \<times> 'b \<times> 'c" where
"silly a b c = (a , b , c)"

(* 
value "\<lparr> ( silly True False True ) \<rparr>"

abbreviation sillier :: "bool \<times> bool \<times> bool" where
"sillier \<equiv> (False, True , False)"

value "\<lparr> (False, True, False) \<rparr>"
value "\<lparr> sillier \<rparr>"
*)


(* 
datatype 'a wcat = 
    WCat 
  | WWord "'a word"


datatype ('a::len, 'b::len, 'c::len) wcat = WCat "'a word" "'b word" "'c word"
*)

term "(case x of True \<Rightarrow> True | False \<Rightarrow> False)"

nonterminal word_args
syntax
 "_word" :: "type \<Rightarrow> int \<Rightarrow> 'x word" ("_w._" 100)
 "_concat_word" :: "type \<Rightarrow> 'x word \<Rightarrow> word_args \<Rightarrow> 'y word" ("(1_w.'(_,/ _'))")
 "_word_arg"    :: "'x word \<Rightarrow> word_args" ("_")

(*"_word_args" :: "type \<Rightarrow> 'x word \<Rightarrow> word_args \<Rightarrow> word_args" ("_w._,/ _") *)

term case_prod

translations
  "'t w. i" == "(CONST word_of_int) i :: 't word"
  "'t w.( u , v )" \<rightleftharpoons> "((CONST word_cat) u v) :: 't word"



nonterminal wpattern and wpatterns and wcases
syntax
 "_wpattern"     :: "[type, wpattern, wpatterns] \<Rightarrow> wpattern" ("_w.'(_,/ _')")
 "_wpatterns"    :: "[type, wpattern, wpatterns] \<Rightarrow> wpatterns" ("_w.'(_,/ _')")
 ""             :: "wpattern \<Rightarrow> wpatterns" ("_")
 "_var_wpattern" :: "[type, pttrn] \<Rightarrow> wpattern"
 "_lit_wpattern" :: "[type, int] \<Rightarrow> wpattern" ("_w._" 100)
 "_wcase_block"  :: "['x word, wcases, 'a] \<Rightarrow> 'a" ("'( wcase _ of// _ else// _ ')")
 "_wcase"        :: "[wpattern, 'a] \<Rightarrow> wcases" ("[ _ \<Rightarrow> _ ]")
 "_wcases"       :: "[wpattern, 'a, wcases] \<Rightarrow> wcases" ("[ _ \<Rightarrow> _ ]// |/ _")


definition example :: "bool \<times> 'a \<times> 'b \<Rightarrow> 'a + 'b" where
"example = (\<lambda> (True,a,b) \<Rightarrow> Inl a | (False,a,b) \<Rightarrow> Inl b)"

value "example (True,False)"

value "example (False,False)"


fun wcase :: "wpattern \<Rightarrow> 'a \<Rightarrow> 'a \<Rightarrow> 'a" where
  "wcase (_wpattern t wp wps) res alt = alt"
| "wcase (_var_wpattern t p) res alt = alt"
| "wcase (_lit_wpattern t i) res alt = alt"

translations
  "_wcase_block ('t w. v) (_wcase (_lit_wpattern s i) res) default" \<rightharpoonup> "default"
  "_wcase_block ('t w. v) (_wcase (_var_wpattern s p) res) default" \<rightharpoonup> "default"
  "_wcase_block ('t w. v) (_wcases p resp (_wcases q resq cs)) default"  
       \<rightharpoonup> "default"

(*
nonterminal stdo_binds and stdo_bind
syntax
  "_do_block" :: "stdo_binds \<Rightarrow> 'a" ("stdo {//(2  _)//}" [12] 62)
  "_do_bind"  :: "[pttrn, 'a] \<Rightarrow> stdo_bind" ("(2_ \<leftarrow>/ _)" 13)
  "_do_let" :: "[pttrn, 'a] \<Rightarrow> stdo_bind" ("(2let _ =/ _)" [1000, 13] 13)
  "_do_then" :: "'a \<Rightarrow> stdo_bind" ("_" [14] 13)
  "_do_final" :: "'a \<Rightarrow> stdo_binds" ("_")
  "_do_cons" :: "[stdo_bind, stdo_binds] \<Rightarrow> stdo_binds" ("_;//_" [13, 12] 12)
  "_thenM" :: "['a, 'b] \<Rightarrow> 'c" (infixl "\<then>\<^sub>d\<^sub>o" 54)

translations
  "_do_block (_do_cons (_do_then t) (_do_final e))"
    \<rightleftharpoons> "CONST state_bind t (\<lambda>_. e)"
  "_do_block (_do_cons (_do_bind p t) (_do_final e))"
    \<rightleftharpoons> "CONST state_bind t (\<lambda>p. e)"
  "_do_block (_do_cons (_do_let p t) bs)"
    \<rightleftharpoons> "let p = t in _do_block bs"
  "_do_block (_do_cons b (_do_cons c cs))"
    \<rightleftharpoons> "_do_block (_do_cons b (_do_final (_do_block (_do_cons c cs))))"
  "_do_cons (_do_let p t) (_do_final s)"
    \<rightleftharpoons> "_do_final (let p = t in s)"
  "_do_block (_do_final e)" \<rightharpoonup> "e"
  "(m \<then>\<^sub>d\<^sub>o n)" \<rightharpoonup> "(m \<bind>\<^sub>s (\<lambda>_. n))"
*)



(*   "\<lambda>(x, y, zs). b" \<rightleftharpoons> "CONST case_prod (\<lambda>x (y, zs). b)"
  "\<lambda>(x, y). b" \<rightleftharpoons> "CONST case_prod (\<lambda>x y. b)"
  "_abs (CONST Pair x y) t" \<rightharpoonup> "\<lambda>(x, y). t" *)


(*
definition f :: "W2 \<rightarrow> W8 \<rightarrow> W8 \<rightarrow> W8" where
"f a0 a1 a2 = (wcase  18w.( 2w.a0 , 16w.( 8w.a1 , 8w.a2)) of
    18w.\<lparr> 2w.0 , 8w.x1 , 8w.x2 \<rparr> \<Rightarrow> 8w.0 
  | 18w.\<lparr> 2w.1 , 8w.x1 , 8w.x2 \<rparr> \<Rightarrow> 8w.x1
  | 18w.\<lparr> 2w.2 , 8w.x1 , 8w.x2 \<rparr> \<Rightarrow> 8w.x2
  | 18w.\<lparr> 2w.3 , 8w.x1 , 8w.x2 \<rparr> \<Rightarrow> 8w.x1 + 8w.x2)
*)

nonterminal stdo_binds and stdo_bind
syntax
  "_match_block" :: "stdo_binds \<Rightarrow> 'a" ("stdo {//(2  _)//}" [12] 62)
  "_match_bind"  :: "[pttrn, 'a] \<Rightarrow> stdo_bind" ("(2_ \<leadsto>/ _)" 13)
  "_do_then" :: "'a \<Rightarrow> stdo_bind" ("_" [14] 13)
  "_do_final" :: "'a \<Rightarrow> stdo_binds" ("_")
  "_do_cons" :: "[stdo_bind, stdo_binds] \<Rightarrow> stdo_binds" ("_;//_" [13, 12] 12)
  "_thenM" :: "['a, 'b] \<Rightarrow> 'c" (infixl "\<then>\<^sub>d\<^sub>o" 54)

translations
  "_do_block (_do_cons (_do_then t) (_do_final e))"
    \<rightleftharpoons> "CONST state_bind t (\<lambda>_. e)"
  "_do_block (_do_cons (_do_bind p t) (_do_final e))"
    \<rightleftharpoons> "CONST state_bind t (\<lambda>p. e)"
  "_do_block (_do_cons (_do_let p t) bs)"
    \<rightleftharpoons> "let p = t in _do_block bs"
  "_do_block (_do_cons b (_do_cons c cs))"
    \<rightleftharpoons> "_do_block (_do_cons b (_do_final (_do_block (_do_cons c cs))))"
  "_do_cons (_do_let p t) (_do_final s)"
    \<rightleftharpoons> "_do_final (let p = t in s)"
  "_do_block (_do_final e)" \<rightharpoonup> "e"
  "(m \<then>\<^sub>d\<^sub>o n)" \<rightharpoonup> "(m \<bind>\<^sub>s (\<lambda>_. n))"


*)

end