theory Comonads

imports
  Main Monads


begin

(* Stream semantics with hybrid logic *)

datatype 'a CxtStr =
  cxtstr "'a list" 'a "'a stream"  ("_:=_:=|_" [100, 100, 100] 100)

(* extract is a keyword in Isabelle *)
fun "extract" :: "'a CxtStr \<Rightarrow> 'a" where
  "extract (_ := f :=| _) = f"

fun bindsharp :: "('a CxtStr \<Rightarrow> 'b) \<Rightarrow> ('a list \<Rightarrow> 'a \<Rightarrow> 'a stream \<Rightarrow> 'b)" ("_\<sharp>" 100) where
  "(k \<sharp>) az a as = k (az := a :=| as)"

fun cobindL :: "('a list \<Rightarrow> 'a \<Rightarrow> 'a stream \<Rightarrow> 'b) \<Rightarrow> 'a list \<Rightarrow> 'a \<Rightarrow> 'a stream \<Rightarrow> 'b list" where
  "cobindL k [] a as = []"
| "cobindL k (a' # az') a as = (k az' a' (SCons a as)) # (cobindL k az' a' (SCons a as))"

fun bindl :: "('a CxtStr \<Rightarrow> 'b) \<Rightarrow> 'a CxtStr \<Rightarrow> 'b list" ("_=>>\<^sup>l_" [100, 100] 100) where
  "k =>>\<^sup>l (az := a :=| as) = cobindL (k \<sharp>) az a as"

primcorec cobindS :: "('a list \<Rightarrow> 'a \<Rightarrow> 'a stream \<Rightarrow> 'b) \<Rightarrow> 'a list \<Rightarrow> 'a \<Rightarrow> 'a stream \<Rightarrow> 'b stream" where
  "shd (cobindS k az a as) = k (a # az) (shd as) (stl as)"
| "stl (cobindS k az a as) = cobindS k (a # az) (shd as) (stl as)"

fun binds :: "('a CxtStr \<Rightarrow> 'b) \<Rightarrow> 'a CxtStr \<Rightarrow> 'b stream" ("_=>>\<^sup>s_" [100, 100] 100) where
  "k =>>\<^sup>s (az := a :=| as) = cobindS (k \<sharp>) az a as"

fun cobind :: "('a CxtStr \<Rightarrow> 'b) \<Rightarrow> 'a CxtStr \<Rightarrow> 'b CxtStr" ("_=>>_" [100, 100] 100) where
  "k =>> d = (k =>>\<^sup>l d) := (k d) :=| (k =>>\<^sup>s d)"

definition cojoin :: "'a CxtStr \<Rightarrow> ('a CxtStr) CxtStr" where
  "cojoin as = id =>> as"

(* CxtStream operations *)

fun "next" :: "'a CxtStr \<Rightarrow> 'a CxtStr" where
  "next (p := x :=| f) = (x # p) := (shd f) :=| (stl f)"

fun start :: "'a stream \<Rightarrow> 'a CxtStr" where
  "start s = [] := shd s :=| stl s"

fun history :: "'a CxtStr \<Rightarrow> 'a list" where
  "history (hs := _ :=| _) = hs"

fun future :: "'a CxtStr \<Rightarrow> 'a stream" where
  "future (_ := _ :=| f) = f"

fun prev :: "'a CxtStr \<Rightarrow> ('a CxtStr) option" where
  "prev ((p # ps) := x :=| f) = Some (ps := p :=| (SCons x f))"
| "prev ([] := _ :=| _) = None"

primrec "back" :: "nat \<Rightarrow> 'a CxtStr \<Rightarrow> ('a CxtStr) option" where
  "back 0 d = Some d"
| "back (Suc n) d = (case (back n d) of (Some d') \<Rightarrow> prev d' | None \<Rightarrow> None)"

primrec forth :: "nat \<Rightarrow> 'a CxtStr \<Rightarrow> 'a CxtStr" where
  "forth 0 d = d"
| "forth (Suc n) d = forth n (next d)"

(* Extra proving *)

lemma cxt_eq_by_parts:
  "extract x = extract y \<Longrightarrow> history x = history y \<Longrightarrow> future x = future y \<Longrightarrow>
      x = y"
  apply(induction x)
  apply(induction y)
  by (auto)

lemma next_forth [simp]:
  "next (forth n d) = forth (Suc n) d"
  for d :: "'a CxtStr"
  apply(induction n arbitrary: d)
   apply(auto)[1]
proof -
  fix n
  fix d :: "'a CxtStr"
  assume a: "(\<And>d::'a CxtStr. next (forth n d) = forth (Suc n) d)"

  show "next (forth (Suc n) d) = forth (Suc (Suc n)) d"
    apply(insert a)
    by fastforce
qed

corollary  next_forth':
  "next (forth n d) = forth n (next d)"
  apply(subst next_forth)
  by (auto)

corollary forth_next:
  "forth n (next d) = forth (Suc n) d"
  apply(subst next_forth'[symmetric])
  by (simp)

lemma forth_induct:
  "P d \<Longrightarrow> (\<And>r. P r \<Longrightarrow> P (next r)) \<Longrightarrow> P (forth n d)"
  apply(induction n arbitrary: d)
   apply(auto)[1]
  by (simp)

lemma forth_commute:
  "forth n (forth m d) = forth m (forth n d)"
  apply(induction n arbitrary: m)
  apply(auto)
  by (metis next_forth')

lemma forth_combine [simp]:
  "forth n (forth m d) = forth (n + m) d"
  apply(induction n arbitrary: m)
  apply(auto)
  by (metis next_forth')

lemma prev_next [simp]:
  "prev (next d) = Some d"
  apply(induction d)
  by (auto)

lemma extract_next:
  "extract (next (a := b :=| c)) = shd c"
  by (auto)

lemma extract_next_fut:
  "extract (next s) = shd (future s)"
  apply(induction s)
  by (auto)

end