{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Text1.IsText1(
  IsText1(..)
, unpacked1
, _NonEmpty
) where

import Control.Category ( Category(id, (.)) )
import Control.Applicative ( Applicative((<*>)), (<$>) )
import Control.Lens
    ( Traversable(traverse),
      indexing,
      from,
      iso,
      seconding,
      traversed,
      IndexedTraversal',
      Iso,
      Iso' )
import Data.Char ( Char )
import Data.Int ( Int )
import Data.List.NonEmpty ( NonEmpty((:|)) )
import Data.String ( String )
import Data.Text(Text)
import qualified Data.Text.Lazy as LazyText(Text)
import Data.Text.Lazy.Builder(Builder)
import Data.Text.Lens(IsText(packed, builder), text)
import Data.Tuple(uncurry)

class IsText1 t where
  packed1 ::
    Iso'
      (NonEmpty Char)
      t
  builder1 ::
    Iso'
      (Char, Builder)
      t
  _Text1_ ::
    IndexedTraversal' Int t Char
  _Text1_ =
    from packed1 . traversed
  {-# INLINE _Text1_ #-}

unpacked1 ::
  IsText1 t => Iso' t (NonEmpty Char)
unpacked1 =
  from packed1

instance IsText1 (NonEmpty Char) where
  packed1 =
    id
  builder1 =
    seconding (from builder) . from _NonEmpty
  _Text1_ =
    indexing traverse

instance IsText1 (Char, String) where
  packed1 =
    _NonEmpty
  builder1 =
    seconding (from builder)
  _Text1_ =
    indexing (\f (c, cs) -> (,) <$> f c <*> traverse f cs)

instance IsText1 (Char, Text) where
  packed1 =
    _NonEmpty . seconding packed
  builder1 =
    seconding (from builder)
  _Text1_ =
    indexing (\f (h, t) -> (,) <$> f h <*> text f t)

instance IsText1 (Char, LazyText.Text) where
  packed1 =
    _NonEmpty . seconding packed
  builder1 =
    seconding (from builder)
  _Text1_ =
    indexing (\f (h, t) -> (,) <$> f h <*> text f t)

_NonEmpty ::
  Iso
    (NonEmpty a)
    (NonEmpty b)
    (a, [a])
    (b, [b])
_NonEmpty =
  iso
    (\(h :| t) -> (h, t))
    (uncurry (:|))
