theory MonadSyntax

imports "Monads" Word_Lib.Word_Lemmas

begin

chapter \<open> Monadic Syntax Theory \<close>

(* Inspired by https://isabelle.in.tum.de/dist/library/HOL/HOL-Library/Monad_Syntax.html *)

(* Syntax for 'do' inside State monad *)

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
  "(m \<then>\<^sub>d\<^sub>o n)" \<rightharpoonup> "(m \<bind>S (\<lambda>_. n))"

(* Similar syntax for ReT monad *)

nonterminal retdo_binds and retdo_bind
syntax
  "_rdo_block" :: "retdo_binds \<Rightarrow> 'a" ("retdo {//(2  _)//}" [12] 62)
  "_rdo_bind"  :: "[pttrn, 'a] \<Rightarrow> retdo_bind" ("(2_ \<leftarrow>/ _)" 13)
  "_rdo_let" :: "[pttrn, 'a] \<Rightarrow> retdo_bind" ("(2let _ =/ _)" [1000, 13] 13)
  "_rdo_then" :: "'a \<Rightarrow> retdo_bind" ("_" [14] 13)
  "_rdo_final" :: "'a \<Rightarrow> retdo_binds" ("_")
  "_rdo_cons" :: "[retdo_bind, retdo_binds] \<Rightarrow> retdo_binds" ("_;//_" [13, 12] 12)
  "_rthenM" :: "['a, 'b] \<Rightarrow> 'c" (infixl "\<then>\<^sub>r\<^sub>d\<^sub>o" 54)

translations
  "_rdo_block (_rdo_cons (_rdo_then t) (_rdo_final e))"
    \<rightleftharpoons> "CONST R_bind t (\<lambda>_. e)"
  "_rdo_block (_rdo_cons (_rdo_bind p t) (_rdo_final e))"
    \<rightleftharpoons> "CONST R_bind t (\<lambda>p. e)"
  "_rdo_block (_rdo_cons (_rdo_let p t) bs)"
    \<rightleftharpoons> "let p = t in _rdo_block bs"
  "_rdo_block (_rdo_cons b (_rdo_cons c cs))"
    \<rightleftharpoons> "_rdo_block (_rdo_cons b (_rdo_final (_rdo_block (_rdo_cons c cs))))"
  "_rdo_cons (_rdo_let p t) (_rdo_final s)"
    \<rightleftharpoons> "_rdo_final (let p = t in s)"
  "_rdo_block (_rdo_final e)" \<rightharpoonup> "e"
  "(m \<then>\<^sub>r\<^sub>d\<^sub>o n)" \<rightharpoonup> "(m \<bind>R (\<lambda>_. n))"


syntax
  "_wlit" :: "type \<Rightarrow> args \<Rightarrow> 'a word"    ("\<^sub>w\<langle>_ \<Colon> (_)\<rangle>")

translations
  "\<^sub>w\<langle>'a \<Colon> a, bs\<rangle>" == "(if a then 1 else 0) + 2*\<^sub>w\<langle>'a \<Colon> bs\<rangle>"
  "\<^sub>w\<langle>'a \<Colon> a\<rangle>" == "(if a then 1 else 0) :: 'a"


end
