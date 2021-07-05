{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Data.Text1.AsSingle(
  AsSingle(..)
) where

import Control.Category ( Category(id, (.)) )
import Control.Lens ( uncons, prism', Prism' )
import Control.Monad ( (>=>) )
import Data.Char ( Char )
import Data.Functor.Identity ( Identity(..) )
import Data.Maybe ( Maybe(..) )
import qualified Data.List as List(null)
import Data.List.NonEmpty(NonEmpty((:|)))
import Data.Text(Text)
import qualified Data.Text as Text(singleton, null)
import qualified Data.Text.Lazy as LazyText(Text, singleton, null)

class AsSingle c a | c -> a where
  _Single :: Prism' c a

instance AsSingle [a] a where
  _Single =
    prism'
      (:[])
      (\case
        [a] -> Just a
        _   -> Nothing)

instance AsSingle Text Char where
  _Single =
    prism'
      Text.singleton
      (uncons >=> \(h, t') -> if Text.null t' then Just h else Nothing)

instance AsSingle LazyText.Text Char where
  _Single =
    prism'
      LazyText.singleton
      (uncons >=> \(h, t') -> if LazyText.null t' then Just h else Nothing)

instance AsSingle (Maybe a) a where
  _Single =
    prism'
      Just
      id

instance AsSingle (Identity a) a where
  _Single =
    prism'
      Identity
      (Just . runIdentity)

instance AsSingle (NonEmpty a) a where
  _Single =
    prism'
      (:|[])
      (\(h :| t) -> if List.null t then Just h else Nothing)
