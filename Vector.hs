{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}

data Nat = Zero | Succ Nat

data Vector (n :: Nat) a where
  VNil :: Vector 'Zero a
  VCons :: a -> Vector n a -> Vector ('Succ n) a

deriving instance Show a => Show (Vector n a)

type family Add n m where
  Add 'Zero n = n
  Add ('Succ n) m = 'Succ (Add n m)

append :: Vector n a -> Vector m a -> Vector (Add n m) a
append VNil xs           = xs
append (VCons a rest) xs = VCons a (append rest xs)

-- Now list length index are embedded within the types!
one :: Vector ('Succ ('Succ ('Succ 'Zero))) Char
one = VCons 'a' (VCons 'b' (VCons 'c' VNil))

two :: Vector ('Succ 'Zero) Char
two = VCons 'd' VNil