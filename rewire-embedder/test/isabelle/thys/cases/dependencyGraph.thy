theory dependencyGraph
imports
  Main
  ReWire.Atmo
begin

definition main :: "'a" where
"main  =
  undefined"

type_synonym MyBool = "bool"

datatype MyState = MyState "MyBool" "4 W option" "(4 W) \<uplus> (8 W)"

type_synonym PipelineState = "(2, MyState) Vec"

definition emptyMyState :: "MyState" where
"emptyMyState  =
  MyState False None (inj\<^sub>1 (lit 0))"

definition emptyPipeline :: "PipelineState" where
"emptyPipeline  =
  replicate emptyMyState"

fun incrPipeline :: "MyState \<times> PipelineState \<Rightarrow> PipelineState \<times> MyState"  where
   "incrPipeline (s, ps) = (cons s (init ps :: (1, MyState) Vec), last ps)"

datatype Output = Output "MyBool" "MyState"

fun myStateToOutput :: "MyState \<Rightarrow> Output"  where
   "myStateToOutput (MyState True mw4 (inj\<^sub>1 w4)) = Output True (MyState True mw4 (inj\<^sub>1 w4))"
   | "myStateToOutput (MyState True mw4 (inj\<^sub>2 w8)) = Output False (MyState True mw4 (inj\<^sub>2 w8))"
   | "myStateToOutput (MyState False _ w4w8) = Output False (MyState False None w4w8)"

datatype Input = InputA "4 W" |
               InputB "8 W" |
               InputC "MyBool"

definition initInput :: "Input" where
"initInput  =
  InputC False"

fun inputToMyState :: "Input \<Rightarrow> MyState \<Rightarrow> MyState"  where
   "inputToMyState (InputA w4) (MyState b _ _) = MyState b (Some w4) (inj\<^sub>1 w4)"
   | "inputToMyState (InputB w8) (MyState b w4 _) = MyState b w4 (inj\<^sub>2 w8)"
   | "inputToMyState (InputC True) (MyState _ (Some w4) _) = MyState True None (inj\<^sub>1 w4)"
   | "inputToMyState (InputC True) (MyState _ None w8) = MyState True None w8"
   | "inputToMyState (InputC False) (MyState _ _ w8) = MyState False None w8"

fun loop :: "Input \<Rightarrow> PipelineState \<Rightarrow> Output \<times> PipelineState"  where
   "loop i s = (case incrPipeline (inputToMyState i (head s), s) of
     (s', out) \<Rightarrow> ((myStateToOutput out, s')))"

definition start :: "(Input, Output) Dev" where
"start  =
  extrude (iterSt loop initInput) emptyPipeline"


end