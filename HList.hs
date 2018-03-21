{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE FlexibleContexts   #-}

module HList where

import Data.Monoid ((<>))

data HList xs where
  HNil :: HList '[]
  (:*) :: a -> HList as -> HList (a ': as)

infixr 5 :*

instance Show (HList '[]) where
  show HNil = "HNil"

instance (Show (HList as), Show a)
  => Show (HList (a ': as)) where
    show (a :* rest) = show a <> " :* " <> show rest

hOne :: HList '[Int, Char]
hOne = 1 :* 'b' :* HNil