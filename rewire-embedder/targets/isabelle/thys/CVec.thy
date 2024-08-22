theory CVec

imports "Main" "Jordan_Normal_Form.Matrix"
  Word_Lib.Word_Lemmas

begin

typedef (overloaded) ('len :: len0, 'a) cvec = "{v | v :: 'a vec. dim_vec v = LENGTH('len)}"
  morphisms vec_of_cvec Abs_cvec
  apply(auto)
  using dim_vec_first by blast

lemma vec_of_cvec_len [simp]: "dim_vec (vec_of_cvec v) = LENGTH('len)" for v :: "('len::len0, 'b) cvec"
  using vec_of_cvec by auto

term Abs_cvec

setup_lifting type_definition_cvec

lift_definition cvec_of_vec :: "'a vec \<Rightarrow> ('len :: len, 'a) cvec" is
  "\<lambda>v. (if dim_vec v = LENGTH('len) then v else vec LENGTH('len) undef_vec)"
  by (auto)

lift_definition cvec_index :: "('len::len0, 'a) cvec \<Rightarrow> (nat \<Rightarrow> 'a)" (infixl "!v" 100) is vec_index.
lift_definition cvec :: "(nat \<Rightarrow> 'a) \<Rightarrow> ('len::len0, 'a) cvec"
  is "\<lambda>f. vec LENGTH('len) f" by auto

lift_definition update_cvec :: "('len::len0,'a) cvec \<Rightarrow> nat \<Rightarrow> 'a \<Rightarrow> ('len,'a) cvec" is update_vec by auto



fun word_index :: "(('m::len0) , 'a) cvec \<Rightarrow> ('n::len) word \<Rightarrow> 'a" (infixl "!vw" 100) where
"word_index v i  = v !v (unat i)"

fun word_update ::  "('len::len0,'a) cvec \<Rightarrow> ('b::len) word \<Rightarrow> 'a \<Rightarrow> ('len,'a) cvec" where
"word_update v w = update_cvec v (unat w)"

fun list_sz :: "nat \<Rightarrow> ('a::zero) list \<Rightarrow> 'a list" where
  "list_sz 0 _ = []"
| "list_sz (Suc n) [] = 0 # list_sz n []"
| "list_sz (Suc n) (a # as) = a # list_sz n as"


lemma len_list_sz [simp]: "length (list_sz n l) = n"
  apply(induction n)
   apply(auto)
  apply(induction l)
  apply(auto)
  by (smt (verit, best) Suc_inject length_Cons list.distinct(1) list.sel(3) list_sz.elims list_sz.simps(1))

lemma list_sz_id [simp]: "length l = n \<Longrightarrow> list_sz n l = l"
  apply(induction n, auto)
  apply(induction l, auto)
  by (metis length_Cons list.exhaust list.size(3) list_sz.simps(1))

lift_definition cvec_of_list :: "'a list \<Rightarrow> ('len::len0, 'a::zero) cvec" is
  "\<lambda>v. vec_of_list (list_sz LENGTH('len) v)"
  by (auto)


lift_definition list_of_cvec :: "('len::len0, 'a) cvec \<Rightarrow> 'a list" is
  list_of_vec.

lemma list_of_cvec_len [simp]: "length (list_of_cvec v) = LENGTH('len)"
  for v :: "('len::len0, 'a) cvec"
  by (transfer, auto)


instantiation cvec :: (len0, plus) plus
begin
(* yea, use 'b instead of 'a here bc Isabelle *)
lift_definition plus_cvec :: "('a::len0, 'b) cvec \<Rightarrow> ('a, 'b::plus) cvec \<Rightarrow> ('a, 'b) cvec" is "\<lambda>x y. x + y" by auto
instance ..
end

value "(cvec_of_list [1, 2::int] :: (2, int) cvec) + cvec_of_list [1, 2::int]"


instantiation cvec :: (len0, minus) minus
begin
lift_definition minus_cvec :: "('a::len0, 'b) cvec \<Rightarrow> ('a, 'b::minus) cvec \<Rightarrow> ('a, 'b) cvec" is "\<lambda>x y. x - y" by auto
instance ..
end

instantiation cvec :: (len, zero) zero
begin
lift_definition
  zero_cvec :: "('len::len0, 'b::zero) cvec"
  is "0\<^sub>v (LENGTH('len))"
  by auto
instance ..
end

lift_definition
  unit_cvec :: "nat \<Rightarrow> ('len::len0, 'a::zero_neq_one) cvec"
  is "\<lambda>i. unit_vec LENGTH('len) i"
  by auto

lemma index_unit_cvec[simp]:
  fixes uv :: "('len::len0, 'a::zero_neq_one) cvec"
  assumes "uv = unit_cvec i"
  shows "i < LENGTH('len) \<Longrightarrow> j < LENGTH('len) \<Longrightarrow> uv !v j = (if j = i then 1 else 0)"
  and   "i < LENGTH('len) \<Longrightarrow> uv !v i = 1"
  unfolding assms
  by (transfer, auto)+

(*
lemma cvec_vec_of_list [simp, code abstype]:
  "cvec_of_list (list_of_cvec x) = x"
  apply(transfer)
  apply(auto)
  using vec_list by blast

value "(cvec_of_list [1, 2::int] :: (2, int) cvec) + cvec_of_list [1, 2::int]"

*)


definition map_cvec :: "('a \<Rightarrow> 'b) \<Rightarrow> ('len::len0, 'a) cvec \<Rightarrow> ('len, 'b) cvec" ("map") where
  "map_cvec f v \<equiv> cvec (\<lambda>i. f (v !v i))"

definition zip_cvec :: "('len::len0, 'a) cvec \<Rightarrow> ('len, 'b) cvec \<Rightarrow> ('len, 'a \<times> 'b) cvec" ("zip") where
"zip_cvec v w \<equiv> cvec (\<lambda> i. (v !v i, w !v i))"

definition zipWith_cvec :: "('a \<Rightarrow> 'b \<Rightarrow> 'c) \<Rightarrow> ('len::len0,'a) cvec \<Rightarrow> ('len, 'b) cvec \<Rightarrow> ('len ,'c) cvec" ("zipWith") where
"zipWith_cvec f v w \<equiv> cvec (\<lambda> i. f (v !v i) (w !v i))"

definition zipWith3_cvec :: "('a \<Rightarrow> 'b \<Rightarrow> 'c \<Rightarrow> 'd) \<Rightarrow> ('len::len0,'a) cvec \<Rightarrow> ('len, 'b) cvec \<Rightarrow> ('len, 'c) cvec \<Rightarrow> ('len ,'d) cvec" ("zipWith3") where
"zipWith3_cvec f u v w \<equiv> cvec (\<lambda> i. f (u !v i) (v !v i) (w !v i))"

fun iter :: "(nat \<Rightarrow> 'a \<Rightarrow> 'a) \<Rightarrow> nat \<Rightarrow> 'a \<Rightarrow> 'a" where
"iter f 0 a = a" |
"iter f (Suc n) a = f (Suc n) (iter f n a)"

fun iter' :: "('a \<Rightarrow> 'a) \<Rightarrow> nat \<Rightarrow> 'a \<Rightarrow> 'a" where
"iter' f 0 a = a" |
"iter' f (Suc n) a = f (iter' f n a)"

(* iterate n f a  is the list [ a, f 1 a, f 2 (f 1 a), ... iter f (n - 1) a] *)
fun iterate' :: "nat \<Rightarrow> (nat \<Rightarrow> 'a \<Rightarrow> 'a) \<Rightarrow> 'a \<Rightarrow> 'a list \<Rightarrow> 'a list" where
"iterate' 0 f a acc = acc" |
"iterate' (Suc n) f a acc = iterate' n f a (iter f n a # acc)"

fun iterate :: "nat \<Rightarrow> (nat \<Rightarrow> 'a \<Rightarrow> 'a) \<Rightarrow> 'a \<Rightarrow> 'a list" where 
"iterate n f a = iterate' n f a []"

definition iterate_cvec :: "(nat \<Rightarrow> 'a \<Rightarrow> 'a) \<Rightarrow> ('a::zero) \<Rightarrow> ('len::len0,'a) cvec" where
"iterate_cvec f x = cvec_of_list (iterate (LENGTH('len)) f x)"

definition iterate_cvec' :: "nat \<Rightarrow> ('a \<Rightarrow> 'a) \<Rightarrow> 'a \<Rightarrow> ('len::len0,'a) cvec" ("iterate") where
"iterate_cvec' _ f a = cvec (\<lambda> n. iter' f n a)"

fun unpacklo_cvec ::  "('len::len0,'a) cvec \<Rightarrow> ('len,'a) cvec \<Rightarrow> ('len,'a) cvec" ("unpacklo") where
"unpacklo_cvec v w = cvec (\<lambda> i. (if i mod 2 = 0
                          then v !v (i div 2)
                          else w !v ((i - 1) div 2)))"


fun unpackhi_cvec ::  "('len::len0,'a) cvec \<Rightarrow> ('len,'a) cvec \<Rightarrow> ('len,'a) cvec" ("unpackhi") where
"unpackhi_cvec v w = cvec (\<lambda> i. (if i mod 2 = 0
                          then v !v ((LENGTH('len) div 2) + i div 2)
                          else w !v ((LENGTH('len) div 2) + (i - 1) div 2)))"


fun packlo_cvec :: "('len::len0,'a) cvec \<Rightarrow> ('len,'a) cvec \<Rightarrow> ('len,'a) cvec" ("packlo") where
"packlo_cvec v w = cvec (\<lambda> i. (if i < ((LENGTH('len) div 2))
                          then v !v (i * 2)
                          else w !v ((i - (LENGTH('len) div 2)) * 2)))"


fun packhi_cvec ::  "('len::len0,'a) cvec \<Rightarrow> ('len,'a) cvec \<Rightarrow> ('len,'a) cvec" ("packhi") where
"packhi_cvec v w = cvec (\<lambda> i. (if i < (LENGTH('len) div 2)
                          then v !v (i * 2 + 1)
                          else w !v ((i - (LENGTH('len) div 2)) * 2 + 1)))"




instantiation cvec :: (len0, uminus) uminus
begin
definition uminus_cvec :: "('a::len0, 'b :: uminus) cvec \<Rightarrow> ('a, 'b) cvec" where
  "- v \<equiv> cvec (\<lambda> i. - (v !v i))"
instance ..
end

lift_definition smult_cvec :: "'a :: times \<Rightarrow> ('len::len0, 'a) cvec \<Rightarrow> ('len::len0, 'a) cvec" (infixl "\<cdot>\<^sub>c\<^sub>s" 70)
  is "(\<cdot>\<^sub>v)"
  by (auto)

lift_definition scalar_prodc :: "('len::len0, 'a) cvec \<Rightarrow> ('len::len0, 'a) cvec \<Rightarrow> 'a :: semiring_0" (infix "\<bullet>\<^sub>c" 70)
  is "(\<bullet>)".

fun vmult_cvec :: "('len::len0, 'a :: times) cvec \<Rightarrow> ('len, 'a) cvec \<Rightarrow> ('len,'a) cvec" (infixl "\<cdot>\<^sub>c\<^sub>v" 70) where
"v \<cdot>\<^sub>c\<^sub>v w = zipWith_cvec (*) v w"

  

definition monoid_cvec :: "'len itself \<Rightarrow> 'a itself \<Rightarrow> nat \<Rightarrow> (('len::len, 'a :: monoid_add) cvec) monoid" where
  "monoid_cvec len ty n \<equiv> \<lparr>
    carrier = UNIV,
    mult = (+),
    one = 0\<rparr>"

lemma cvec_zero: "0 = cvec (\<lambda> i. 0)"
  apply transfer by auto

lemma index_cvec_zero: "i < LENGTH('a) 
    \<Longrightarrow> (0 :: ('a :: len,'b :: zero) cvec) !v i = 0"
  by (transfer; auto)


lemma cvec_eq_iff: "x = y \<longleftrightarrow> (\<forall>i. i < LENGTH('a) \<longrightarrow> x !v i = y !v i)"
  for x :: "('a::len0, 'b) cvec"
  by (transfer, auto)

lemma cvec_at [simp]: "i < LENGTH('a) \<Longrightarrow> (cvec f :: ('a::len0, 'b) cvec) !v i = f i"
  by(transfer, auto)

lemma cvec_update_index[simp]: "a < LENGTH('a) \<Longrightarrow> (update_cvec (cv :: ('a::len ,'b) cvec) a x) !v a = x"
  apply transfer by simp  

lemma cvec_collapse:
  (* works because of the implicit bounds on the i in cvec (\<lambda>i. ...) :  *)
  "cvec (\<lambda>i. f ((cvec g :: ('a::len0, 'b) cvec) !v i)) = (cvec (\<lambda>i. f (g i)) :: ('a, 'b) cvec)"
  apply(subst cvec_eq_iff)
  by (auto)

lemma cvec_collapse_id:
  "cvec (\<lambda>i. (cvec g :: ('a::len0, 'b) cvec) !v i) = (cvec g :: ('a,'b) cvec)"
  by (rule cvec_collapse)


lemma cvec_add_same_dim: "v1 + v2 = cvec (\<lambda>i. v1 !v i + v2 !v i)"
  by  (simp add: plus_cvec_def cvec_def plus_vec_def cvec_index.rep_eq)

lemma cvec_minus_same_dim: "v1 - v2 = cvec (\<lambda>i. v1 !v i - v2 !v i)"
  by (simp add: minus_cvec_def cvec_def minus_vec_def cvec_index.rep_eq)

definition append_cvec :: "('len1::len0,'a) cvec \<Rightarrow> ('len2::len0,'a) cvec \<Rightarrow> ('len3::len0,'a) cvec" (infixr "++" 65) where
  "v ++ w \<equiv> cvec (\<lambda> i. if i < LENGTH('len1) then v !v i else w !v (i - LENGTH('len1)))"

lift_definition take_cvec :: "('len::len0,'a) cvec \<Rightarrow> ('len1::len0,'a) cvec" ("take") is 
    "\<lambda> w. vec_first w LENGTH('len1)"  using dim_vec_first by blast

lift_definition drop_cvec :: "('len::len0,'a) cvec \<Rightarrow> ('len1::len0,'a) cvec" ("drop") is
    "\<lambda> w. vec_last w LENGTH('len1)" using dim_vec_last by blast

definition empty_cvec :: "(0,'a) cvec" ("empty") where 
"empty \<equiv> cvec (\<lambda> i. undefined) :: (0,'a) cvec" 

definition singleton_cvec :: "'a \<Rightarrow> (1,'a) cvec" ("singleton") where
"singleton a = cvec (\<lambda>_. a)"

definition reverse_cvec :: "('len::len0,'a) cvec \<Rightarrow> ('len,'a) cvec" ("reverse") where
"reverse v = cvec (\<lambda> i. v !v (LENGTH('len) - 1 - i))"

definition replicate_cvec :: "'a \<Rightarrow> ('len::len0,'a) cvec" ("replicate") where
"replicate a = cvec (\<lambda> _. a)"

definition slice_cvec :: "nat => ('b::len0,'a) cvec \<Rightarrow> ('c :: len0,'a) cvec" ("slice") where
"slice n v = cvec (\<lambda> i. v !v (i + n))"

definition rslice_cvec :: "nat \<Rightarrow> ('b::len0,'a) cvec \<Rightarrow> ('c :: len0,'a) cvec" ("rslice") where
"rslice n v = cvec (\<lambda> i. v !v (i + LENGTH('b) - n - LENGTH('c)))"

definition head_cvec :: "('b::len0,'a) cvec \<Rightarrow> 'a" ("head") where
"head_cvec v = cvec_index v 0"

definition tail_cvec :: "('b::len0,'a) cvec \<Rightarrow> ('c::len0,'a) cvec" ("tail") where
"tail_cvec v = cvec (\<lambda> i. v !v (i + 1))"

definition init_cvec :: "('b::len0,'a) cvec \<Rightarrow> ('c::len0,'a) cvec" ("init") where
"init_cvec v = cvec (\<lambda> i. v !v i)"

definition last_cvec :: "('b::len0,'a) cvec \<Rightarrow> 'a" ("last") where
"last_cvec v = v !v (LENGTH('b) - 1)"

definition cons_cvec :: "'a \<Rightarrow> ('b::len0,'a) cvec \<Rightarrow> ('c::len0,'a) cvec" ("cons") where
"cons_cvec a v = singleton a ++ v"

definition snoc_cvec :: "('b::len0,'a) cvec \<Rightarrow> 'a \<Rightarrow> ('c::len0,'a) cvec" ("snoc") where
"snoc_cvec v a = v ++ singleton a"

end