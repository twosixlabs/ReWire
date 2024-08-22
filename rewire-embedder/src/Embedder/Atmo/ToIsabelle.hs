{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}

module Embedder.Atmo.ToIsabelle where

import safe Embedder.Isabelle.Syntax as Isa
    ( Theory(..),
      DatatypeConstructor(..), Decl(..),
      Term(..), Typ(..), TypSig(..), TName, Pttrn(..))
import safe Embedder.Atmo.Syntax as A
    ( FreeProgram, Module (..),
      TypeSynonym(..), DataDefn(..), Defn(..),
      Exp(..), Ty(..), Poly(..),
      DataCon(..), Pat (..), PatBind (..), FunBinding (..), getPatVars,
      -- getFunBody,
      )
import Embedder.Orphans ()
import Embedder.Error (AstError, MonadError, failAt)
import Embedder.Atmo.Types (flattenSig, nilTy, pairTy, TypeAnnotated (..))
import Embedder.Atmo.Util(isPrim, flattenLam)
import Embedder.Annotation (noAnn)

import System.FilePath(takeBaseName)
import Data.Char (isDigit)
import Data.List (isPrefixOf)
import qualified Data.Text as T (Text, pack, splitOn, map, filter, null, head, last)
import Data.Graph (Graph, Vertex, Tree (..))
import Embedder.Atmo.DependencyGraph (Declaration (..), sortFreeProgram)
import Embedder.Atmo.FlattenMonadTrans (transMonadT)
import Embedder.Builtins (TyBuiltin(..))
import Data.Text (Text, pack)

-- import Debug.Trace (trace)

---------- Notes --------------

-- Name Handling:
      -- remove Embed, 
      -- remove Fresh
      -- remove Bind, bind, unbind
      -- remove Name, redefine tGlobal and tLocal
      -- remove n2s, s2n


---------- ================================================= ----------

---------- Todos/Questions --------------
-- TODO: Fix Data Definition printing YAY
-- TODO: Fix bindings for datatdefs (etc.) YAY
   -- we have to unbind Binds, but we can still use tLocal on them if we want their 'prefix'
-- TODO: Implement Case translation in ReWire-Isabelle [and modify this translation to suit]
-- TODO: Implement translation of Builtins/Prims (hopefully can be ported over from the Source translation)
-- TODO: Readability concerns?

---------- ================================================= ----------

-- Translating Programs

{- data Theory = Theory
  { thyName :: T.Text
  , imports :: [Text]
  , decls :: [Decl] } -}

rewireUserMods :: [Text]
rewireUserMods = ["ReWire_2.thy","Bits_2.thy","BitWord_2.thy","Finite_2.thy","FiniteComp_2.thy"
                 ,"Monad_2.thy","Prelude_2.thy","Vectors_2.thy","Primitives_2.thy"]

embedModule :: (MonadError AstError m) => FilePath -> A.Module -> m (Maybe Isa.Theory)
embedModule filename mod = do
    let thyname = T.pack $ takeBaseName filename
    if pack filename `elem` rewireUserMods
      then return Nothing
      else do
            (decls,_graph,_tree) <- tModule mod
            return $ Just $ Theory thyname ["Main","ReWire.Atmo"] decls

embedFreeProgram :: (MonadError AstError m) => FilePath -> A.FreeProgram -> m Isa.Theory
embedFreeProgram filename prog = do
    let thyname = T.pack $ takeBaseName filename
    (decls,_graph,_tree) <- tFreeProgram prog
    return $ Theory thyname ["Main","ReWire.Atmo"] decls


{- type FreeProgram = ([DataDefn], [TypeSynonym], [Defn]) -}
tFreeProgram :: (MonadError AstError m) => A.FreeProgram -> m ([Isa.Decl], Graph, [Tree Vertex] )
tFreeProgram (data_defs, type_syns, definitions) =
      let ddfs = filter (not . isReWire . dataName) data_defs
          tsyns = filter (not . isReWire . typeSynName) type_syns
          defs = filter (not . isReWire . defnName) definitions
          fprog = (ddfs,tsyns,defs)
          (decls,graph,tree) = sortFreeProgram fprog
      in do
            decls' <- mapM tDeclaration decls
            return (decls', graph, tree)

tModule :: (MonadError AstError m) => A.Module -> m ([Isa.Decl], Graph, [Tree Vertex] )
tModule (A.Module data_defs type_syns definitions) =
      let ddfs = filter (not . isReWire . dataName) data_defs
          tsyns = filter (not . isReWire . typeSynName) type_syns
          defs = filter (not . isReWire . defnName) definitions
          fprog = (ddfs,tsyns,defs)
          (decls,graph,tree) = sortFreeProgram fprog
      in do
            decls' <- mapM tDeclaration decls
            return (decls', graph, tree)

---------- ================================================= ----------

-- Translating Declarations

{- data Decl =
    Datatype {
      datatypeName :: T.Text,
      datatypeTVars :: [Typ],
      datatypeConstructors :: [DatatypeConstructor] }
  | TypeSynonym T.Text [Text] Typ
  | Definition {
     definitionName :: T.Text,
     definitionType :: Typ,
     definitionVars :: [Term],
     definitionTerm :: Term }
  | Fun { funEquations :: [(Text, Typ, [([Term], Term)])] } -}


tDeclaration :: (MonadError AstError m) => Declaration -> m Isa.Decl
tDeclaration = \ case
     DDecl d -> tDataDefn d
     TDecl d -> tTypeSynonym d
     FDecl d -> tDefn d
     RDecl ds -> tRDefns ds




{- data TypeSynonym = TypeSynonym
      { typeSynAnnote :: Annote
      , typeSynName   :: !(Name TyConId)
      , typeSynType   :: !Poly) } -}
tTypeSynonym :: Monad m => A.TypeSynonym -> m Isa.Decl
tTypeSynonym (A.TypeSynonym _a name poly) = do
     (tvs, t) <- tPoly poly
     return $ Isa.TypeSynonym (tGlobal name) tvs t


{- data Defn = Defn
      { defnAnnote :: Annote
      , defnName   :: !(Name Exp)
      , defnPolyTy :: !Poly)
      , defnAttr   :: !(Maybe DefnAttr)
      , defnBody   :: !(Bind [Name Exp] Exp)) } -}
tDefn :: (MonadError AstError m) => A.Defn -> m Isa.Decl
tDefn =  \ case
   Defn a _name _poly _defnattr [] -> failAt a "should not have empty definitions"
   Defn _a name poly _defnattr [A.FunBinding _ [] rhs] -> do
                  (_tvs,tsig) <- tPolySig poly
                  let e = rhs
                  e' <- tExp e
                  return $ Isa.Definition (tGlobal name) tsig [] e'
   d@(Defn _a name poly _defnattr fbs@(_:_)) -> do
                  fun <- tFun d
                  return $ Isa.Fun [fun]


tFunBinding :: (MonadError AstError m) => A.FunBinding -> m ([Pttrn], Term)
tFunBinding (A.FunBinding _ ps e) = do
      e' <- tExp e
      let ps' = map tPat ps
      return (ps',e')


tFun :: (MonadError AstError m) => A.Defn -> m (T.Text, TypSig, [([Pttrn], Term)])
tFun (Defn _a name poly _defnattr fbs) = do
                  (_tvs,tsig) <- tPolySig poly
                  fbs' <- mapM tFunBinding fbs
                  return (tGlobal name, tsig, fbs')


tRDefns :: (MonadError AstError m) => [A.Defn] -> m Isa.Decl
tRDefns ds = do
            fs <- mapM tFun ds
            return $ Isa.Fun fs

{- data DataDefn = DataDefn
      { dataAnnote :: Annote
      , dataName   :: !(Name TyConId)
      , dataVars   :: ![Name Ty]
      , dataCons   :: ![DataCon] } -}
tDataDefn :: Monad m => A.DataDefn -> m Isa.Decl
tDataDefn (DataDefn _a name _tvs cons@(A.DataCon _ _ p:_)) = do
      constrs <- mapM tDataCon cons
      (tvs,_t) <- tPoly p
      return $ Datatype (tGlobal name) tvs constrs
tDataDefn (DataDefn _ name tvs []) = return $ Datatype (tGlobal name) (map tLocal tvs) []


{- data DataCon = DataCon Annote !(Name DataConId) !Poly) -}
{- data DatatypeConstructor = DatatypeConstructor {
      constructorName :: T.Text,
      constructorType :: Typ,
      constructorArgs :: [Typ] } |
     DatatypeNoConstructor {
      constructorArgs :: [Typ] } -}
tDataCon :: Monad m => A.DataCon -> m Isa.DatatypeConstructor
tDataCon (A.DataCon _a name poly) = do
      (_tvs, TypSig ts _cod) <- tPolySig poly  -- TODO: Might need to use the codomain and tvs to rename args variables
      return $ DatatypeConstructor (tGlobal name) ts


---------- ================================================= ----------

--- Translating Types

{- data Typ = Type { typeId :: TName,
                   typeArgs :: [Typ] }
         | TNum { typeN :: Int }
         | TVar { typeId :: TName } -}


{- data Ty = TyApp Annote !Ty !Ty
        | TyCon Annote !(Name TyConId)
        | TyVar Annote !(Name Ty)
        | TyNat Annote !Natural -}
tType :: A.Ty -> Isa.Typ
tType (flattenTyTuple . transMonadT -> t') = case flattenTy t' of
        (TyBuiltin _a tb,ts) -> Isa.TBuiltin tb (map tType ts)
        (TyCon _a name,ts) -> Isa.Type (tGlobal name) (map tType ts)
        (TyVar _a name, []) -> Isa.TVar (tLocal name)
        (TyVar _a name, ts@(_:_)) -> Isa.Type (tLocal name) (map tType ts)
        (TyNat _a n, []) -> Isa.TNum (fromEnum n)
        (TyNat a _, _:_) -> error $ "ERROR: shouldn't apply type numerals" ++ show a
        (TyApp {},_) -> error "ERROR: shouldn't have an application after flattening"
        (TyTuple {},_) -> error "ERROR: shouldn't have a type tuple after flattening"


tTypeSig :: A.Ty -> Isa.TypSig
tTypeSig t = let (args,cod) = flattenSig t
             in TypSig (map tType args) (tType cod)

{- newtype Poly = Poly (Bind [Name Ty] Ty) -}
tPoly :: Monad m => A.Poly -> m ([TName], Isa.Typ)
tPoly (Poly tvs t) = return (map tLocal tvs, tType t)

tPolySig :: Monad m => A.Poly -> m ([TName], Isa.TypSig)
tPolySig (Poly tvs t) = return (map tLocal tvs, tTypeSig t)


-- | translates (t1,t2,t3,t4) to (t1,(t2,(t3,t4))))
flattenTyTuple :: A.Ty -> A.Ty
flattenTyTuple (TyTuple _ []) = nilTy
flattenTyTuple (TyTuple a ts@(_:_)) =
      foldr1 (pairTy a) ts
flattenTyTuple t = t

flattenTy :: A.Ty -> (A.Ty,[A.Ty])
flattenTy (TyApp _ t ts) = (t', ts' ++ ts)
       where (t',ts') = flattenTy t
flattenTy t = (t,[])


---------- ================================================= ----------

--- Translating Expressions

{- data Term =
        LitString T.Text
      | LitNum Integer
      | LitWord Int Integer
      | LitVec [Term]
      | Free { termName :: VName }
      | Prim { primId :: Prim }
      | Abs { absVar :: Term,
                termId :: Term }  -- lambda abstraction
      | App { funId :: Term,
               argIds :: [Term] }    -- application
      | If { ifId :: Term,
             thenId :: Term,
             elseId :: Term }
      | Case { termId :: Term,
               caseSubst :: [(Term, Term)] }
      | Let { letSubst :: [(Term, Term)],
              inId :: Term }
      | IsaEq { firstTerm :: Term,
                secondTerm :: Term }
      | Tuplex [Term]
      | List   [Term]  -}


-- Expressions
{- data Exp = App     Annote !(Maybe Poly) !(Maybe Ty) !Exp !Exp
         | Lam     Annote !(Maybe Poly) !(Maybe Ty) !(Bind (Name Exp) Exp)
         | Var     Annote !(Maybe Poly) !(Maybe Ty) !(Name Exp)
         | Con     Annote !(Maybe Poly) !(Maybe Ty) !(Name DataConId)
         | Case    Annote !(Maybe Poly) !(Maybe Ty) !Exp !(Bind Pat Exp) !(Maybe Exp)
         | Builtin Annote !(Maybe Poly) !(Maybe Ty) !Builtin
         | LitInt  Annote !(Maybe Poly) !Integer
         | LitStr  Annote !(Maybe Poly) !Text
         | LitVec  Annote !(Maybe Poly) !(Maybe Ty) ![Exp]
         | LitList Annote !(Maybe Poly) !(Maybe Ty) ![Exp] -}
tExp :: Monad m => A.Exp -> m Isa.Term
tExp e | Just pt <- tyAnn e = do
      e' <- tExp (setTyAnn Nothing e)
      (_ts,pt') <- tPoly pt
      return $ Isa.TypAnnTerm e' pt'
tExp (A.Tuple _ _ _ es) = do
      es' <- mapM tExp es
      return -- $ trace "Tuplex" 
             $ Isa.Tuplex es'
tExp (A.App _ _ _ e es) = do
      rator <- tExp e
      rands <- mapM tExp es
      return $ Isa.App rator rands
-- tExp (A.Lam _ _ _ [x] (A.App _ _ _ (A.RWUser _ _ _ BindRInf) [m,_loop])) = do
--       let x' = tLocal x
--       m' <- tExp m -- : M a
--       return $ Isa.App (Isa.Free "iterRe") [Isa.Abs [x'] m']
tExp lam@(A.Lam {}) = do
      (names,e) <- flattenLam lam
      e' <- tExp e
      return $ Isa.Abs (map tLocal names) e'
tExp (A.Var     _ _mp _mt name) = -- trace ("toIsabelle: Var: " <> show name) $ 
                                  return $ Free $ tLocal name
tExp (A.Con     _ _mp _mt name) = -- trace ("toIsabelle: Con: " <> show name) $ 
                                  return $ Free $ tGlobal name
tExp c@(A.Case    {}) = do
      let (e',bs') = flattenCase c
      e'' <- tExp e'
      bs'' <- mapM tCase bs'
      return $ Isa.Case e'' bs''
tExp (A.RWUser _ _mp _mt b) = -- trace ("toIsabelle: Builtin: " <> show b) $ 
                               return $ Isa.Prim b
tExp (A.LitInt  _ _mp     i) = return $ Isa.LitNum (toInteger i)
tExp (A.LitStr  _ _mp     s) = return $ Isa.LitString s
tExp (A.LitVec  _ _mp _mt es) = do
      es' <- mapM tExp es
      return $ Isa.LitVec es'
tExp (A.LitList _ _mp _mt es) = do
      es' <- mapM tExp es
      return $ Isa.List es'
tExp (A.If _ _mp _mt t c a) = do
      t' <- tExp t
      c' <- tExp c
      a' <- tExp a
      return $ Isa.If t' c' a'
tExp (A.Let _ _mp _mt bs e) = do
      e' <- tExp e
      bs' <- mapM tPatBind bs
      return $ Isa.Let bs' e'


-- Patterns and Case

tPat :: Pat -> Pttrn
tPat = \ case
      (PatCon _a _mp mt name ps) -> PttrnCon (tGlobal name) (fmap tType mt) (map tPat ps)
      (PatVar _a _mp mt name) -> PttrnVar (tLocal name) (fmap tType mt)
      (PatWildCard _a _mp mt) -> PttrnWildCard (fmap tType mt)
      (PatTuple _a _mp mt ps) -> PttrnTuple (fmap tType mt) (map tPat ps)

tPatBind :: (Monad m) => PatBind -> m (Isa.Pttrn, Isa.Term)
tPatBind (A.PatBind p e) = do
      let p' = tPat p
      e' <- tExp e
      return (p',e')


-- data Pttrn = PttrnWildCard (Maybe Typ) 
--            | PttrnVar VName (Maybe Typ) 
--            | PttrnCon VName (Maybe Typ) [Pttrn]
--            | PttrnTuple (Maybe Typ) [Pttrn]
--         deriving (Eq, Ord, Show, Typeable, Data)


wildPat :: A.Pat
wildPat = PatWildCard noAnn Nothing Nothing

-- | Case Annote !(Maybe Poly) !(Maybe Ty) !Exp !(Bind Pat Exp) !(Maybe Exp)
flattenCase :: A.Exp -> (A.Exp, [(Pat,Exp)])
flattenCase (A.Case _ _ _ e pbs) = (e,map (\ (A.PatBind p e') -> (p,e')) pbs)
flattenCase _ = error "flattenCase: should prevent this case"
-- flattenCase (A.Case _ _ _ e p e1 me) = flattenCase' e [(p,e1)] me
--       where
--       flattenCase' :: A.Exp -> [(Pat,A.Exp)] -> Maybe A.Exp
--                   -> (A.Exp, [(Pat, A.Exp)])
--       flattenCase' e bs (Just c@(A.Case _ _ _ e' p e1 me)) = if e == e'
--             then flattenCase' e (bs++[(p,e1)]) me
--             else (e,bs ++ [(wildPat, c)])
--       flattenCase' e bs (Just e') = (e,bs ++ [(wildPat, e')])
--       flattenCase' e bs Nothing = (e,bs)
-- flattenCase _ = error "flattenCase: should prevent this case"

tCase :: Monad m => (Pat,Exp) -> m (Pttrn,Term)
tCase (p,e) = do
      e' <- tExp e
      let p' = tPat p
      return (p', e')

-- Builtins


{-
data Builtin = Error | Extern
             | SetRef | GetRef
             | Bind | Return
             | Put | Get
             | Signal | Lift | Extrude | Unfold
             | VecFromList | VecReplicate | VecReverse | VecSlice | VecRSlice
             | VecIndex | VecIndexProxy
             | VecConcat
             | VecMap | VecFoldR | VecFoldL | VecGenerate
             | Finite | FiniteMinBound | FiniteMaxBound | ToFinite | ToFiniteMod | FromFinite
             | NatVal
             | Bits | Resize | BitSlice | BitIndex
             | Add | Sub | Mul | Div | Mod | Pow
             | LAnd | LOr
             | And | Or
             | XOr | XNor
             | LShift | RShift | RShiftArith
             | Eq | Gt | GtEq | Lt | LtEq
             | LNot | Not
             | RAnd | RNAnd | ROr | RNor | RXOr | RXNor
             | MSBit
      deriving (Eq, Generic, Show, Typeable, Data, Bounded, Enum)


data Prim =
  -- ReWire Core Primitive Operations
            Add | Sub
          | Mul | Div | Mod
          | Pow
          | LAnd | LOr
          | And | Or
          | XOr | XNor
          | LShift | RShift
          | RShiftArith
          | Eq | Gt | GtEq | Lt | LtEq
          | Replicate Natural
          | LNot | Not
          | RAnd | RNAnd
          | ROr | RNor | RXOr | RXNor
          | MSBit
          | Resize | Reverse
          | Id
      deriving (Eq, Ord, Generic, Show, Typeable, Data, Read)

-}



---------- ================================================= ----------

--- Translating Names

-- show Name to keep its Fresh extension, for global variables
-- n2s Name to discard its Fresh extension, for local variables

tGlobal :: T.Text -> T.Text
tGlobal = filterName . getBaseName
 -- could modify getBaseName to try to handle namespaces/imports

tLocal :: T.Text -> T.Text
tLocal = filterName . getBaseName

getBaseName :: T.Text -> T.Text
getBaseName s = last (T.splitOn "." s)

filterName :: T.Text -> T.Text
filterName = leadingTrailingEmpty
             . removePunc
             . handleQualified
      where
      handleQualified :: T.Text -> T.Text
      handleQualified = T.map (\ c -> if c =='.' then '_' else c)
      removePunc :: T.Text -> T.Text
      removePunc = T.filter (/= '$')
      leadingTrailingEmpty :: T.Text -> T.Text
      leadingTrailingEmpty s | T.null s = "null"
                             | isDigit (T.head s) = "a" <> s
                             | s == "_" = s
                             | T.last s == '_' = s <> "a"
                             | T.head s == '_' = "a" <> s
                             | otherwise = s


isPrimException :: Show a => a -> Bool
isPrimException a = let s = show a
      in s == "R_" || s == "A_" || s == "PuRe"

isReWire :: Show a => a -> Bool
isReWire a = isPrefixOf "ReWire." (show a) || isPrim a && not (isPrimException a)


---------- ================================================= ----------



{-
{- Testing area -}

mkVar :: T.Text -> Exp
mkVar t = Var noAnn Nothing Nothing (s2n t)

mkDefn :: T.Text -> T.Text -> Exp -> Defn
mkDefn n t e = Defn noAnn (s2n n) (Embed (poly [] (TyCon noAnn (s2n t)))) Nothing (Embed (bind [] e))

{-
      | Var     Annote !(Maybe Poly) !(Maybe Ty) !(Name Exp)

{ defnAnnote :: Annote
      , defnName   :: !(Name Exp)
      , defnPolyTy :: !Poly)
      , defnAttr   :: !(Maybe DefnAttr)
      , defnBody   :: !(Bind [Name Exp] Exp)) } 
-}

-- d > a,b,c, > e > f,g > h
-- h, [f,g], e, [a,b,c], d == 7, [5,6], 4, [0,1,2], 3 

example :: FreeProgram
example = ([],[],[ mkDefn "a" "Bool" (mkApp noAnn (mkVar "b") [mkVar "e"])
                 , mkDefn "b" "Bool" (mkApp noAnn (mkVar "c") [mkVar "e"])
                 , mkDefn "c" "Bool" (mkApp noAnn (mkVar "a") [mkVar "f"])
                 , mkDefn "d" "Bool" (mkApp noAnn (mkVar "a") [mkVar "b"])
                 , mkDefn "e" "Bool" (mkApp noAnn (mkVar "g") [mkVar "h"])
                 , mkDefn "f" "Bool" (mkApp noAnn (mkVar "g") [mkVar "f"])
                 , mkDefn "g" "Bool" (mkApp noAnn (mkVar "f") [mkVar "h"])
                 , mkDefn "h" "Bool" (mkVar "h")])

-}