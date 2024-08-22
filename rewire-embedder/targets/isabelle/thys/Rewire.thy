theory Rewire

imports
   Monads MonadSyntax Comonads

begin

chapter \<open> Rewire theory \<close>

fun signal :: "'o \<Rightarrow> ('i,'o,'s,'i) Re" where
  "signal o' = Pause (\<lambda> \<sigma> (i,s,oo) . let 
                                        i' = shd \<sigma> ; 
                                        is = stl \<sigma>
                                      in
                                         ((i',s,o') :\<triangleright>[ (i',is) ]))"


fun stepR :: "('s,'o) State \<Rightarrow> ('i,'o,'s,'i) Re" where
  "stepR x = Pause (\<lambda> \<sigma> (i,s,oo) .  let 
                                        i'      = shd \<sigma> ; 
                                        is      = stl \<sigma> ;
                                        (o',s') = runSt x s
                                      in
                                         ((i,s,oo) :\<triangleright> ((i',s',o') :\<triangleright>[ (i',is) ]) ))"

fun liftR :: "('s, 'a) State \<Rightarrow> ('i, 'o, 's, 'a) Re" where
  "liftR s = bind_ST_R_R s (returnR)"


fun squish :: "('w,'a) Writer_plus \<Rightarrow> (( 'w \<times> 'w list) \<times> 'a)" where
   "squish (w :\<triangleright>[ a ]) = ((w,[]),a)"
 | "squish (w :\<triangleright> wp)   = (let
                          ((w0,wp0),a) = squish wp  
                         in 
                          ((w0,w # wp0),a))"


primcorec loopWP :: "('i\<times>'s\<times>'o) list \<Rightarrow> ('a \<Rightarrow> 'i stream \<Rightarrow> ('i \<times> 's \<times> 'o) \<Rightarrow> (('i\<times>'s\<times>'o),('a \<times>'i stream)) Writer_plus)
                  \<Rightarrow> 'a \<Rightarrow> 'i stream \<Rightarrow> ('i \<times> 's \<times> 'o) \<Rightarrow> ('i\<times>'s\<times>'o) stream" where
  "shd (loopWP xs f a \<sigma> iso') = (case xs of 
                                 [] \<Rightarrow> (case (squish (f a \<sigma> iso')) of
                                       ((w,[]),(a',\<sigma>')) \<Rightarrow> w
                                      |((w,w0 # ws),(a',\<sigma>')) \<Rightarrow> w0 )
                                | (w # ws) \<Rightarrow> w)" 
| "stl (loopWP xs f a \<sigma> iso') = (case xs of 
                                 [] \<Rightarrow> (case (squish (f a \<sigma> iso')) of
                                       ((w,[]),(a',\<sigma>')) \<Rightarrow> loopWP [] f a' \<sigma>' w
                                      |((w,w0 # ws),(a',\<sigma>')) \<Rightarrow> loopWP ws f a' \<sigma>' w )
                                | (w # ws) \<Rightarrow> loopWP ws f a \<sigma> iso')"

fun loopR :: "('a \<Rightarrow> ('i,'o,'s,'a) Re) \<Rightarrow> 'a \<Rightarrow> 'i stream \<Rightarrow> ('i \<times> 's \<times> 'o) \<Rightarrow> ('i\<times>'s\<times>'o) stream" where
  "loopR f = loopWP [] (dePause \<circ> f)"

primrec wr2ls :: "('w,'a) Writer_plus \<Rightarrow> 'w list \<times> 'w \<times> 'a" where
  "wr2ls ( w :\<triangleright>[ a ])= ([],w,a) "
| "wr2ls ( w :\<triangleright> ws) = (let (wp0 , w0, a) = wr2ls ws in (w # wp0 , w0, a))"

primcorec iterReAcc :: "('a \<Rightarrow> ('i,'o,'s,'a) Re) \<Rightarrow> ('i\<times>'s\<times>'o) list \<Rightarrow> 'a \<Rightarrow> ('i,'o,'s) Re_INF" where
  "shd (iterReAcc f ws a w is) = (case ws of 
                                      [] \<Rightarrow> w
                                      | (w' # ws) \<Rightarrow> w')"
| "stl (iterReAcc f ws a w' is) = (case ws of 
                                      [] \<Rightarrow> let (ws' , w'' , (a', is')) = wr2ls (dePause (f a) is w' ) in iterReAcc f ws' a' w'' is'
                                      | (w # ws) \<Rightarrow> iterReAcc f ws a w' is)"
 
fun iterRe :: "('a \<Rightarrow> ('i,'o,'s,'a) Re) \<Rightarrow> ('a \<Rightarrow> ('i,'o,'s) Re_INF)" where
  "iterRe f a = iterReAcc f [] a"


fun iter :: "('i \<Rightarrow> 'o) \<Rightarrow> 'i \<Rightarrow> ('i,'o,'s) Re_INF" where
"iter f i = iterRe (\<lambda> i. signal (f i)) i"

fun iterSt :: "('i \<Rightarrow> 's \<Rightarrow> ('o \<times> 's)) \<Rightarrow> 'i \<Rightarrow> ('i,'o,('s \<times> 't)) Re_INF" where
"iterSt f i = iterRe (\<lambda> i. liftR (get \<bind>S (\<lambda> s. returnS (f i s))) \<bind>R (\<lambda> (out,s). 
                           (R_bind' (liftR (put s)) (signal out)))) i"

(* important for stream evaluation *)
lemma eval_stream[simp]:"n > 0 \<Longrightarrow> stake n xs = shd xs # stake (n-1) (stl xs)" 
  by (metis Suc_pred' stake.simps(2))

(* silly ? *)
declare Let_def [simp]

primcorec siter :: "'a \<Rightarrow> 'a stream" where
  "shd (siter a) = a"
| "stl (siter a) = siter a"

fun fst\<^sub>3 :: "'a \<times> 'b \<times> 'c \<Rightarrow> 'a" where
  "fst\<^sub>3 (a, b, c) = a"

fun snd\<^sub>3 :: "'a \<times> 'b \<times> 'c \<Rightarrow> 'b" where
  "snd\<^sub>3 (a, b, c) = b"

fun trd\<^sub>3 :: "'a \<times> 'b \<times> 'c \<Rightarrow> 'c" where
  "trd\<^sub>3 (a, b, c) = c"

fun reToCxtStr :: "('i \<times> 's \<times> 'o) \<Rightarrow> 'i stream \<Rightarrow> ('i, 'o, 's) Re_INF \<Rightarrow> ('i \<times> 's \<times> 'o) CxtStr" where
  "reToCxtStr init is re = start (re init is)"

(*
primcorec extrude :: "('i,'o,('s \<times> 't)) Re_INF \<Rightarrow> 's \<Rightarrow> ('i,'o,'t) Re_INF" where
  "shd (extrude r s itout is) = itout"
| "stl (extrude r s itout is) = (let (i',(s',t'),out') = shd (stl (r (fst\<^sub>3 itout, (s,snd\<^sub>3 itout), trd\<^sub>3 itout) is)) 
                                 in extrude r s' (i', t', out') (stl is))"
*)

fun extrude :: "('i,'o,('s \<times> 't)) Re_INF \<Rightarrow> 's \<Rightarrow> ('i,'o,'t) Re_INF" where
"extrude r s (i,t,out) is = smap (\<lambda> (i',(s',t'),out'). (i',t',out')) (r (i,(s,t),out) is)"

end