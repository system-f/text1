{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Data.Text1.AsSingle(
  AsSingle(..)
) where

import Control.Category ( Category((.)) )
import Control.Lens ( uncons, prism, prism', Prism )
import Control.Monad ( (>=>) )
import Data.Char ( Char )
import Data.Either ( Either(Left, Right) )
import Data.Functor.Identity ( Identity(..) )
import Data.Maybe ( Maybe(..), maybe )
import qualified Data.List as List(null)
import Data.List.NonEmpty(NonEmpty((:|)))
import Data.Text(Text)
import qualified Data.Text as Text(singleton, null)
import qualified Data.Text.Lazy as LazyText(Text, singleton, null)

class AsSingle s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _Single :: Prism s t a b

instance AsSingle [a] [a] a a where
  _Single =
    prism'
      (:[])
      (\case
        [a] -> Just a
        _   -> Nothing)

instance AsSingle Text Text Char Char where
  _Single =
    prism'
      Text.singleton
      (uncons >=> \(h, t') -> if Text.null t' then Just h else Nothing)

instance AsSingle LazyText.Text LazyText.Text Char Char where
  _Single =
    prism'
      LazyText.singleton
      (uncons >=> \(h, t') -> if LazyText.null t' then Just h else Nothing)

instance AsSingle (Maybe a) (Maybe b) a b where
  _Single =
    prism
      Just
      (maybe (Left Nothing) Right)

instance AsSingle (Identity a) (Identity b) a b where
  _Single =
    prism
        Identity
        (Right . runIdentity)

instance AsSingle (NonEmpty a) (NonEmpty a) a a where
  _Single =
    prism'
      (:|[])
      (\(h :| t) -> if List.null t then Just h else Nothing)
