{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Data.Text1(
  Text1(Text1)
, singleton
, cons1
, snoc1
, append
, last1
, init1
, uncons1
, unsnoc1
, isSingle
, length
, compareLength
, _text
, _string
, _head1
, _tail1
, IsText1(packed1, text1)
, unpacked1
) where

import Control.Category(Category(id, (.)))
import Control.Lens(IndexedTraversal', Cons(_Cons), Snoc(_Snoc), Iso', Lens', Prism', prism', iso, lens, (^.), (#), from, indexing, traversed)
import qualified Control.Lens as Lens(uncons)
import Control.Monad(Monad(return, (>>)))
import Data.Binary(Binary(put, get))
import Data.Bool -- (Bool)
import Data.Char(Char)
import Data.Data(Data)
import Data.Eq(Eq)
import Data.Functor(Functor(fmap))
import Data.Int(Int)
import Data.List.NonEmpty(NonEmpty((:|)))
import Data.Maybe(Maybe(Just, Nothing))
import Data.Ord(Ord, Ordering)
import Data.Semigroup(Semigroup((<>)))
import Data.String(String)
import Data.Text(Text)
import qualified Data.Text as Text(cons, snoc, append, null, init, last, empty, length, compareLength, uncons, pack, unpack)
import Data.Text.Lens(IsText(packed))
import Data.Traversable(Traversable(traverse))
import Data.Tuple(uncurry)
import Data.Typeable(Typeable)
import Prelude(Show(show), Num((+)))

data Text1 =
  Text1
    Char
    Text
  deriving (Eq, Ord, Data, Typeable)

instance Show Text1 where
  show (Text1 h t) =
    show (Text.cons h t)

instance Semigroup Text1 where
  (<>) = 
    append

instance Binary Text1 where
  put (Text1 h t) =
    put h >> put t
  get =
    do h <- get
       t <- get
       return (Text1 h t)

singleton ::
  Char
  -> Text1
singleton c =
  Text1 c Text.empty

cons1 ::
  Char
  -> Text1
  -> Text1
cons1 c t = 
  Text1 c (_text # t)

snoc1 ::
  Text1
  -> Char
  -> Text1
snoc1 (Text1 h t) c =
  Text1 h (Text.snoc t c)

append ::
  Text1
  -> Text1
  -> Text1
append (Text1 h1 t1) t =
  Text1 h1 (Text.append t1 (_text # t))

last1 ::
  Text1
  -> Char
last1 (Text1 h t) =
  if Text.null t
    then
      h
    else
      Text.last t

init1 ::
  Text1
  -> Text
init1 (Text1 h t) =
  if Text.null t
    then
      Text.empty
    else
      Text.cons h (Text.init t)

uncons1 ::
  Text1
  -> Maybe (Char, Text1)
uncons1 (Text1 h t) =
  fmap (\(h', t') -> (h, Text1 h' t')) (Text.uncons t)

unsnoc1 ::
  Text1
  -> Maybe (Text1, Char)
unsnoc1 (Text1 h t) =
  if Text.null t
    then
      Nothing
    else
      Just (Text1 h (Text.init t), Text.last t)

isSingle ::
  Text1
  -> Bool
isSingle (Text1 _ t) =
  Text.null t

length ::
  Text1
  -> Int
length (Text1 _ t) =
  1 + Text.length t

compareLength ::
  Text1
  -> Int
  -> Ordering
compareLength (Text1 _ t) n =
  Text.compareLength t (n + 1)

_text ::
  Prism'
    Text
    Text1
_text =
  prism'
    (\(Text1 h t) -> Text.cons h t)
    (fmap (uncurry Text1) . Text.uncons)

_string ::
  Prism'
    String
    Text1
_string =
  prism'
    (\(Text1 h t) -> h : Text.unpack t)
    (fmap (\(h, t) -> Text1 h (Text.pack t)) . Lens.uncons)

_head1 ::
  Lens'
    Text1
    Char
_head1 =
  lens
    (\(Text1 h _) -> h)
    (\(Text1 _ t) h -> Text1 h t)

_tail1 ::
  Lens'
    Text1
    Text
_tail1 =
  lens
    (\(Text1 _ t) -> t)
    (\(Text1 h _) t -> Text1 h t)

class IsText1 t where
  packed1 :: 
    Iso'
      (NonEmpty Char)
      t
  text1 ::
    IndexedTraversal' Int t Char 
  text1 =
    unpacked1 . traversed

instance IsText1 Text1 where
  packed1 =
    iso
      (\(h :| t) -> Text1 h (t ^. packed))
      (\(Text1 h t) -> h :| (packed # t))

instance IsText1 (NonEmpty Char) where
  packed1 =
    id
  text1 =
    indexing traverse

unpacked1 ::
  IsText1 t =>
  Iso'
    t
    (NonEmpty Char)
unpacked1 =
  from packed1

instance Cons Text1 Text1 Char Char where
  _Cons =
    prism'
      (uncurry cons1)
      uncons1

instance Snoc Text1 Text1 Char Char where
  _Snoc =
    prism'
      (uncurry snoc1)
      unsnoc1
