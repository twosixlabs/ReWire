module CarrySaveAdders where

import Prelude hiding ((^))
-- import ExtensionalSemantics 
-- import BinaryArithmetic
-- import W8

f :: W8 -> W8 -> W8 -> (W8, W8)
f a b c = ( ((a .&. b) .|. (a .&. c) .|. (b .&. c) ) << C , (a ^ b) ^ c )

-- |
-- | Example 1. CSA
-- |
-- | The only thing this does is take its inputs i, computes csa on them, and
-- | output the results every clock cycle.

csa :: (W8, W8, W8) -> Re (W8, W8, W8) () (W8, W8) ()
csa (a, b, c) = signal (f a b c) >>= csa

csa_sem :: (W8, W8, W8) -> DomReInf (W8, W8, W8) () (W8, W8)
csa_sem = re_inf . csa

csa_tst :: Stream ((W8, W8, W8) , () , (W8, W8))
csa_tst = csa_sem (shd is) w0 (stl is) 
  where
    is = (b40 , b25 , b20) :< ((b41 , b25 , b20) :< is)
    w0 = (shd is , () , (b0 , b0))

-- takeStr 3 csa_tst
-- [((40,25,20),(),(0,0)),((41,25,20),(),(48,37)),((40,25,20),(),(50,36))]

-- |
-- | Example 2. Storing CSA
-- |

scsa :: (W8, W8, W8) -> Re (W8, W8, W8) (W8, W8) (W8, W8) ()
scsa abc = save abc >>= \ cs -> signal cs >>= scsa
   where
     save :: (W8 , W8 , W8) -> Re (W8 , W8 , W8) (W8, W8) (W8, W8) (W8, W8)
     save (a , b , c) = lift (set (f a b c) >> get)

scsa_sem :: (W8, W8, W8) -> DomReInf (W8, W8, W8) (W8, W8) (W8, W8)
scsa_sem = re_inf . scsa

scsa_tst :: Stream ((W8, W8, W8), (W8, W8), (W8, W8))
scsa_tst = scsa_sem (shd is) w0 (stl is)
  where
    is = (b40 , b25 , b20) :< ((b41 , b25 , b20) :< is)
    w0 = (shd is , (b0 , b0) , (b0 , b0))

-- takeStr 3 scsa_tst
-- [((40,25,20),(0,0),(0,0)),((41,25,20),(48,37),(48,37)),((40,25,20),(50,36),(50,36))]

-- |
-- | Example 3. Pipelined CSA
-- |

data Ans a = DC | Val a deriving Show

pcsa :: W8 -> Re W8 () (Ans (W8, W8)) ()
pcsa a = signal DC >>= \ b ->
         signal DC >>= \ c ->
         signal (Val (f a b c)) >>= pcsa

pcsa_sem :: W8 -> DomReInf W8 () (Ans (W8, W8))
pcsa_sem = re_inf . pcsa

pcsa_tst = pcsa_sem (shd is) pw0 (stl is)
  where
    is = b40 :< (b25 :< (b20 :< (b41 :< (b25 :< (b20 :< is)))))
    pw0 = (shd is , () , DC)

-- takeStr 4 pcsa_tst
-- [(40,(),DC),(25,(),DC),(20,(),DC),(41,(),Val (48,37))]

-- |
-- | Example 4. Terminating Fragment of pcsa
-- |

frag :: W8 -> Re W8 () (Ans (W8, W8)) W8
frag a = signal DC >>= \ b -> signal DC >>= \ c -> signal (Val (f a b c))

frag_sem :: W8 -> DomRePlus W8 () (Ans (W8, W8)) W8
frag_sem = re_plus . frag

frag_tst = frag_sem (shd is) pw0 (stl is)
  where
    is = b40 :< (b25 :< (b20 :< (b41 :< (b25 :< (b20 :< is)))))
    pw0 = (shd is , () , DC)

-- frag_tst
-- (40,(),DC) :> (25,(),DC) :> (20,(),DC) :> (41,(),Val (48,37)) :+> (41,(),[25 :< ...])
