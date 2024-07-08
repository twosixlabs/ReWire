# Crossbar Switch

## What’s a Crossbar Switch?

To perform this exercise, I relied primarily on two sources to explain what a crossbar switch is; they are:

* [Wikipedia](https://en.wikipedia.org/wiki/Crossbar_switch)
* [The Crossbar Switch](http://www.cs.emory.edu/~cheung/Courses/355/Syllabus/90-parallel/CrossBar.html)

Given these explanations, I generated a Haskell implementation of a crossbar switch like function (see CrossbarSwitch.hs below). All the Haskell and ReWire code for this example can be found below.

* [CrossbarSwitch.hs](code/CrossbarSwitch.hs)

What follows is an explanation of this code. First, we consider the Haskell definition of a crossbar switch, written in monadic style. Then, we transform the Haskell definition of the switch into proper ReWire. This is important because it gives you a practical introduction to the differences between Haskell and ReWire..

### Just write it in Haskell first, then add a few bits to get your program into ReWire.

The usual mode of program development is to first write a version of the desired application in Haskell using the concepts described in the Language Reference section. The reasons to do this boil down to the GHC compiler being vastly more mature than the ReWire compiler, and so, for example, error messages are much more informative. Once all the kinks as it were are worked out in Haskell (e.g., getting something that typechecks, etc.), make a number of small tweeks to get your program into the ReWire subset of Haskell. This section of the tutorial introduces the reader to this mode of program development.

#### ReWire Prelude
In the same manner as the Glasgow Haskell Compiler and other Haskell implementations, we are compiling a list of standard definitions into a prelude file, ReWirePrelude.hs. This file is, in effect, a dirty snowball of definitions that we are accumulating with the intent of ultimately making it part of the standard ReWire implementation. For now, to use it, you must explicitly import it.

ReWirePrelude.hs includes definitions for bits (Bit) and words of various sizes (e.g., W8 and W32) as well as functions on those primitive types (e.g., rotateR2). The particular file we use can be found here.

What follows is a crossbar switch function written in Haskell. We will take this as an input specification, by which we mean that it is not terribly important to actually understand what the crossbar function is calculating. Rather, what is interesting is what must change in this specification to transform it into a proper ReWire specification.

```haskell
{-# LANGUAGE DataKinds #-}

import Prelude hiding ((^), (+))
import ReWire

switch :: t -> t -> Bool -> (t, t)
switch x _ True  = (x,x)
switch x y False = (x,y)

type W8 = W 8

data Maybe4 = Maybe4 (Maybe W8) (Maybe W8) (Maybe W8) (Maybe W8)

type Bool16 = (Bool,Bool,Bool,Bool,Bool,Bool,Bool,Bool,Bool,Bool,Bool,Bool,Bool,Bool,Bool,Bool)

crossbar :: Maybe4 -> Bool16 -> Maybe4 
crossbar (Maybe4 x10 x20 x30 x40) (c11,c12,c13,c14,c21,c22,c23,c24,c31,c32,c33,c34,c41,c42,c43,c44)
   = let
          (x41,y31) = switch x40 Nothing c41
          (x42,y32) = switch x41 Nothing c42
          (x43,y33) = switch x42 Nothing c43
          (_,y34) = switch x43 Nothing c44

          (x31,y21) = switch x30 y31 c31
          (x32,y22) = switch x31 y32 c32
          (x33,y23) = switch x32 y33 c33
          (_,y24) = switch x33 y34 c34

          (x21,y11) = switch x20 y21 c21
          (x22,y12) = switch x21 y22 c22
          (x23,y13) = switch x22 y23 c23
          (_,y14) = switch x23 y24 c24

          (x11,y10) = switch x10 y11 c11
          (x12,y20) = switch x11 y12 c12
          (x13,y30) = switch x12 y13 c13
          (_,y40) = switch x13 y14 c14
     in
       Maybe4 y10 y20 y30 y40

data Inp = Inp Maybe4 Bool16 | NoInput
            
data Out = Out Maybe4 | Nix

dev :: Inp -> ReacT Inp Out Identity ()
dev (Inp m4 b16) = signal (Out (crossbar m4 b16)) >>= dev
dev NoInput      = signal Nix >>= dev

start :: ReacT Inp Out Identity ()
start = signal Nix >>= dev
```

Crossbar Switch in ReWire
This section considers the ReWire version of the crossbar switch. The whole code is available here. These two implementations are almost the same, but there are differences. We will go through the code in detail to highlight the differences.

Here we have commented out the module and import declarations, except for the ReWirePrelude. The main ReWire file does not belong in a module. Note that the monad definitions from Control.Monad are built-in to ReWire, and so, they should not be imported.

{- 
module CrossbarSwitch where

import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Resumption.Reactive

type I = Identity
-}

import ReWirePrelude
Note that the switch function has polymorphic type below. ReWire does not allow polymorphically typed expressions, and so, for that reason, we use an INLINE directive. This directive informs the ReWire frontend to inline that function wherever it occurs. This has the effect of eliminating the polymorphic function. An alternative would be to simply rewrite the type declaration of switch so that it had a simple (i.e., variable free) type. Note also that the Maybe4 declaration is written with no free variables; i.e., Maybe4 isn’t polymorphic either.

It is worth emphasizing that each function declaration in ReWire must have an accompanying type declaration.

switch :: t -> t -> Bool -> (t, t)
{-# INLINE switch #-}
switch x y True  = (x,x)
switch x y False = (x,y)

data Maybe4 = Maybe4 (Maybe W8) (Maybe W8) (Maybe W8) (Maybe W8)
The code for crossbar below has several changes. For one, it is no longer declared with a let declaration, but rather uses an equivalent where formulation. Semantically, where and let are equivalent; that is, let <binding-group> in e is equvalent to e where <binding-group>. But, as of this writing, let has not been implemented in the ReWire compiler as yet (it is on a lengthy to-do list of simple extensions). There is another slightly more substantial difference. The clauses in the where declaration have been rearranged in order of dependency. These binding clauses are processed in-order (in the manner of, say, OCaml) rather than as a group (as in Haskell). This is a ReWire bug/feature which also appears on the aforementioned to-do list. Note that we have also dispensed with the type synonym Bool16. Type synonyms are on our to-do list.

crossbar :: Maybe4                                                                            ->
            (Bool,Bool,Bool,Bool,Bool,Bool,Bool,Bool,Bool,Bool,Bool,Bool,Bool,Bool,Bool,Bool) ->
            Maybe4 
crossbar (Maybe4 x10 x20 x30 x40) (c11,c12,c13,c14,c21,c22,c23,c24,c31,c32,c33,c34,c41,c42,c43,c44)
   = Maybe4 y10 y20 y30 y40
        where
          (x41,y31) = switch x40 Nothing c41
          (x31,y21) = switch x30 y31 c31
          (x21,y11) = switch x20 y21 c21
          (x42,y32) = switch x41 Nothing c42
          (x32,y22) = switch x31 y32 c32
          (x22,y12) = switch x21 y22 c22
          (x11,y10) = switch x10 y11 c11
          (x12,y20) = switch x11 y12 c12
          (x43,y33) = switch x42 Nothing c43
          (x33,y23) = switch x32 y33 c33
          (x23,y13) = switch x22 y23 c23
          (x13,y30) = switch x12 y13 c13
          (x44,y34) = switch x43 Nothing c44
          (x34,y24) = switch x33 y34 c34
          (x24,y14) = switch x23 y24 c24
          (x14,y40) = switch x13 y14 c14
Below are the input and output types for the device, Inp and Out. One thing that stands out style-wise when compared to Haskell is that we don’t use a type synonym for the long tuple of Bools. In Haskell, one would typically write something like type Bool16 = (Bool,...,Bool) just for syntactic convenience. As it stands, type synonyms are unimplemented in ReWire. This is, again, on the aforementioned to-do list of simple extensions to ReWire.

data Inp = Inp Maybe4                                                                            
               (Bool,Bool,Bool,Bool,Bool,Bool,Bool,Bool,Bool,Bool,Bool,Bool,Bool,Bool,Bool,Bool)
         | NoInput
            
data Out = Out Maybe4 | Nix
Below is the device declaration. The built-in identity monad in ReWire is written I (rather than Identity that was imported from Control.Monad in the Haskell version). Note that the dev has been replaced by the semantically equivalent \ i -> dev i below. This is because ReWire is a 1st-order language and you cannot pass the function dev to the other function >>=. It is a focus of current research to extend ReWire to higher-order.

devcrossbar :: ReT Inp Out I ()
devcrossbar = signal Nix >>= \ i -> dev i

dev :: Inp -> ReT Inp Out I ()
dev (Inp m4 b16) = signal (Out (crossbar m4 b16)) >>= \ i -> dev i
dev NoInput      = signal Nix >>= \ i -> dev i
Last, but not least, is that every ReWire specification must contain a start declaration. The start symbol must have type ReT Inp Out I ().

start :: ReT Inp Out I ()
start = devcrossbar
Compiling with the ReWire Compiler
Once the ReWire specification is complete, we can compile with the ReWire compiler rwc:

bill$ rwc RWCrossbar.hs -o RWC.vhd
bill$ ls -l RWC.vhd
-rwxr-xr-x  1 bill  staff  61024 Jun 14 14:11 RWC.vhd
Note that, depending how successfully one’s translation into ReWire is, one may receive error messages fro the ReWire compiler. These are improving, although there is admittedly much room for improvement as of this writing.
