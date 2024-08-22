theory Monads

imports "HOL-Library.BNF_Corec" "HOL-Library.Code_Lazy" "HOL-Library.Stream"

begin

chapter \<open> Monadic Theory for ReWire Semantics \<close>

datatype ('s,'a) State =
  SM (run : "'s \<Rightarrow> ('a \<times> 's)")

fun returnS :: "'a \<Rightarrow> ('s,'a) State"  where
  "returnS a = SM (\<lambda> s . (a,s))"

definition getS :: "('s,'s) State" where
  [simp]: "getS = SM (\<lambda> s. (s,s))"

definition setS :: "'s \<Rightarrow> ('s, unit) State" where
  [simp]: "setS s = SM (\<lambda> _ . ((),s))" 

(* TODO:  what syntax precedence to setS? *)
fun state_bind :: "('s,'a) State \<Rightarrow> ( 'a \<Rightarrow> ('s,'b) State) \<Rightarrow> ('s,'b) State" (infix  "\<bind>S" 200) where
  "sm \<bind>S f = SM (\<lambda> s . let 
                                a  = fst ((run sm) s);
                                s2 = snd ((run sm) s)
                              in 
                                (run (f a)) s2 )"

definition state_bind' :: "('s,'a) State \<Rightarrow> ('s,'b) State \<Rightarrow> ('s,'b) State" (infix "\<then>S" 201) where
  [simp]: "sm1 \<then>S sm2 = sm1 \<bind>S (\<lambda>_. sm2)"

definition runSt :: "('s,'a) State \<Rightarrow> 's \<Rightarrow> ('a \<times> 's)" where
  [simp]: "runSt sm s = (run sm) s"


(* We need a representation of State for easy translation from StateT/Identity *)

type_synonym 'a Identity = "(unit,'a) State"

definition get :: "('s\<times>'t,'s) State" where
  [simp]: "get = SM (\<lambda> (s,t). (s,(s,t)))"

definition put :: "'s \<Rightarrow> ('s\<times>'t, unit) State" where
  [simp]: "put s = SM (\<lambda> (_,t) . ((),(s,t)))" 

fun liftS :: "('t,'a) State \<Rightarrow> ('s \<times> 't,'a) State" where
"liftS (SM f) = SM (\<lambda> (s,t) . case f t of (a,t') \<Rightarrow> (a,s,t'))"


(* For examples of the following, see USC.thy *)

(* Equivalent to forM_ in haskell, applied specifically to the state monad *)
fun forS' :: "'a list \<Rightarrow> ('a \<Rightarrow> ('s,'b) State) \<Rightarrow> ('s,unit) State" where
  "forS' (a # as) f = f a \<then>S (forS' as f)"
| "forS' _ f = returnS ()"

(* Equivalent to getsS and modifyS from haskell, respectively *)
fun getsS :: "('s \<Rightarrow> 'a) \<Rightarrow> ('s, 'a) State" where
  "getsS f = SM (\<lambda>s. (f s, s))"

fun modifyS :: "('s \<Rightarrow> 's) \<Rightarrow> ('s, unit) State" where
  "modifyS f = getS \<bind>S (setS \<circ> f)"

(* Writer\<^sup>+ monad *)

datatype ('w,'a) Writer = 
    D 'a 
  | w_con_2 'w "('w,'a) Writer" (infix ":>>>" 100) (* TODO: precedence *)

fun writer_bind :: "('w,'a) Writer \<Rightarrow> ('a \<Rightarrow> ('w,'b) Writer) \<Rightarrow> ('w,'b) Writer" (infix "\<ggreater>\<bind>" 200)where
  "(D a) \<ggreater>\<bind> f = f a" |
  "(w :>>> ws) \<ggreater>\<bind> f = w :>>>  ws \<ggreater>\<bind>f"

datatype ('w,'a) Writer_plus = 
    w_pls_con_1 'w 'a ("_:\<triangleright>[_]")
  | w_pls_con_2 'w "('w,'a) Writer_plus" ("_:\<triangleright>_") 

fun writer_plus_bind :: "('w,'a) Writer_plus \<Rightarrow> ('a \<Rightarrow> ('w,'b) Writer_plus) \<Rightarrow> ('w,'b) Writer_plus" (infix "\<star>=" 200) where
   "(w :\<triangleright>[ a ]) \<star>= f = (w :\<triangleright> (f a))"
  |"(w :\<triangleright> ws)   \<star>= f = (w :\<triangleright> (ws \<star>= f))"


(* Reactive Resumption Monad *)

code_lazy_type stream

primcorec alsothing :: "'a \<Rightarrow> 'a stream \<Rightarrow> 'a stream" ("_\<triangleleft>_")where
  "shd ( a \<triangleleft> _ ) = a"
| "stl ( _ \<triangleleft> as) = as"

fun thing :: "('w,'a) Writer_plus \<Rightarrow> ('w \<Rightarrow> 'a \<Rightarrow> 'w stream) \<Rightarrow> 'w stream" where
  "thing (w :\<triangleright>[ a ]) k = k w a "
| "thing (w :\<triangleright> ws) k = (w \<triangleleft> (thing ws k))"

(* DomRe+ *)
datatype ('i,'o,'s,'a) Re =
  Pause (dePause : "'i stream \<Rightarrow> ('i \<times> 's \<times> 'o) \<Rightarrow> (('i \<times> 's \<times> 'o),('a \<times> 'i stream)) Writer_plus")

(* DomReInf *)
type_synonym ('i,'o,'s) Re_INF = "('i \<times> 's \<times> 'o) \<Rightarrow> 'i stream \<Rightarrow> (('i \<times> 's \<times> 'o) stream)"

datatype ('a,'b) CProd ("_\<uplus>_")= 
    inj\<^sub>1 'a 
  | inj\<^sub>2 'b 


(* _<>==00_  regular state monad bind*)

(* _<>==0+ *)
fun bind_ST_R_R :: "('s,'a) State \<Rightarrow> ('a \<Rightarrow> ('i,'o,'s,'b) Re) \<Rightarrow> ('i,'o,'s,'b) Re" (infix "\<bind>STRR" 200)where
  "bind_ST_R_R x f = Pause (\<lambda> is (i,s,oo). let (a , s') = run x s in ((dePause (f a)) is (i,s',oo)))"

fun post :: "(('i \<times> 's \<times> 'o),('a \<times> 'i stream)) Writer_plus \<Rightarrow> ('a => ('s,'b) State) => (('i \<times> 's \<times> 'o),('b \<times> 'i stream)) Writer_plus " where
  "post ((i,s,oo):\<triangleright>[(a,is)]) phi = (let (b,s') = run (phi a) s in ((i,s',oo):\<triangleright>[(b,is)]))"
| "post (w :\<triangleright> ws) phi = (w :\<triangleright> (post ws phi))"

(* _<>==+0 *)
fun bind_R_ST_R :: "('i,'o,'s,'a) Re \<Rightarrow> ('a \<Rightarrow> ('s,'b) State) \<Rightarrow> ('i,'o,'s,'b) Re" (infix "\<bind>RSTR" 200)where
  "bind_R_ST_R x f = Pause (\<lambda> is (i,s,oo). post ((dePause x) is (i,s,oo)) f)"


fun returnR :: "'a \<Rightarrow> ('i,'o,'s,'a) Re" where
  "returnR v = Pause (\<lambda> \<sigma> iso . (iso :\<triangleright>[ (v,\<sigma>) ]))"

fun liftR :: "('s, 'a) State \<Rightarrow> ('i, 'o, 's, 'a) Re" where
  "liftR s = bind_ST_R_R s (returnR)"

fun ppp :: "(('i \<times> 's \<times> 'o),('a \<times> 'i stream)) Writer_plus \<Rightarrow>
                  ('a \<Rightarrow> 'i stream \<Rightarrow> ('i \<times> 's \<times> 'o) \<Rightarrow> (('i \<times> 's \<times> 'o),'b) Writer_plus) \<Rightarrow>
                  (('i \<times> 's \<times> 'o),'b) Writer_plus" (infix "+++" 200) where
   "(w :\<triangleright>[ (a , \<sigma>) ]) +++ f = f a \<sigma> w"
  |"(w :\<triangleright> ws) +++ f = (w :\<triangleright> (ws +++ f))"

(* _<>==++_ *)
fun R_bind :: "('i,'o,'s,'a) Re \<Rightarrow> ('a \<Rightarrow> ('i,'o,'s,'b) Re) \<Rightarrow> ('i,'o,'s,'b) Re" (infix "\<bind>R" 200) where 
  "r \<bind>R f = Pause (\<lambda> \<sigma> iso . (((dePause r) \<sigma> iso) +++ (dePause \<circ> f)))"


(*

loop i = f(i) \<bind>R loop

loop = (\<lambda>i.f(i) \<bind>R loop)

loop = iterRe (\<lambda>i.f(i))

*)

definition R_bind' :: "('i,'o,'s,'a) Re \<Rightarrow> ('i,'o,'s,'b) Re \<Rightarrow> ('i,'o,'s,'b) Re" (infix "\<then>R" 201) where
  [simp]: "m1 \<then>R m2 = m1 \<bind>R (\<lambda>_. m2)"

fun runR :: "('i,'o,'s,'a) Re \<Rightarrow> 'i stream \<Rightarrow> ('i \<times> 's \<times> 'o) \<Rightarrow> (('i \<times> 's \<times> 'o),('a \<times> 'i stream)) Writer_plus" where
  "runR (Pause x) \<sigma> (i,s,oo) = (x \<sigma> (i,s,oo)) "

(* TODO: precedence (NOTE: this is infixR) *)
fun r_kleisli_comp :: "('a \<Rightarrow> ('i,'o,'s,'b) Re) \<Rightarrow> ('b \<Rightarrow> ('i,'o,'s,'c) Re) \<Rightarrow> 'a \<Rightarrow> ('i,'o,'s,'c) Re" (infixr "<>" 100) where
  "f <> g = (\<lambda> a . f a \<bind>R g)"

fun fst\<^sub>3 :: "'a \<times> 'b \<times> 'c \<Rightarrow> 'a" where
  "fst\<^sub>3 (a, b, c) = a"

fun snd\<^sub>3 :: "'a \<times> 'b \<times> 'c \<Rightarrow> 'b" where
  "snd\<^sub>3 (a, b, c) = b"

fun trd\<^sub>3 :: "'a \<times> 'b \<times> 'c \<Rightarrow> 'c" where
  "trd\<^sub>3 (a, b, c) = c"

primcorec extrudeR :: "('i,'o,'s) Re_INF \<Rightarrow> 's \<Rightarrow> ('i,'o,unit) Re_INF" where
  "shd (extrudeR r s iout is) = (fst\<^sub>3 iout, (), trd\<^sub>3 iout)"
| "stl (extrudeR r s iout is) = (let backcomp = shd (stl (r (fst\<^sub>3 iout, s, trd\<^sub>3 iout) is)) 
                                 in extrudeR r (snd\<^sub>3 backcomp) (fst\<^sub>3 backcomp, (), trd\<^sub>3 backcomp) (stl is))"

(*
type_synonym ('i,'o) PuRe = "('i, unit, 'o) Re_INF" *)

(*
lemma "gennats 1 = stl(gennats 0)" 
proof (coinduction rule : stream.coinduct_strong)
  case Eq_stream
  then show ?case apply auto done
  then have  ?thesis  by simp
qed
*)
end
