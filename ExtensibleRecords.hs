{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module HList where

import           Data.Monoid  ((<>))

import           GHC.TypeLits
import           Data.Proxy

newtype s >> a = Named a

data HRec xs where
  HEmpty :: HRec '[]
  HCons  :: (s >> a) -> HRec xs -> HRec (s >> a ': xs)

instance Show (HRec '[]) where
  show _ = "HEmpty"

instance (Show a, KnownSymbol s, Show (HRec xs))
  => Show (HRec (s >> a ': xs)) where
    show (HCons (Named a) rest) =
      let val = show a
          key = symbolVal (Proxy :: Proxy s)
          more = show rest
      in "(" <> key <> ": " <> val <> ") " <> more

extRecord :: HRec '["foo" >> Char, "bar" >> Int]
extRecord = HCons (Named @"foo" 'a') (HCons (Named @"bar" (3 :: Int)) HEmpty)