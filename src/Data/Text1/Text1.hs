{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Data.Text1.Text1(
  Text1(..)
, AsText1(..)
, HasText1(..)
, cons1
, snoc1
, length1
, compareLength1
, _head1
, _tail1
, _last1
, _init1
, maybeText
, maybeLazyText
, each1
) where

import Control.Applicative(Applicative((<*>), pure), (*>))
import Control.Category(Category(id, (.)))
import Control.Lens
    ( uncons,
      cons,
      from,
      iso,
      seconding,
      prism',
      (#),
      over,
      Index,
      IxValue,
      Ixed(..),
      Cons(..),
      Snoc(..),
      Each(..),
      Reversing(..),
      Plated(..),
      Field1(_1),
      Field2(_2),
      _Just,
      Iso',
      Lens',
      Prism',
      Traversal1' )
import Data.Binary(Binary(put, get))
import Data.Char(Char)
import Data.Data(Data)
import Data.Eq(Eq((==)))
import Data.Functor(Functor(fmap), (<$>))
import Data.Functor.Apply ( Apply((<.>)) )
import Data.Int(Int)
import Data.List.NonEmpty(NonEmpty((:|)))
import Data.Maybe(Maybe(Just, Nothing))
import Data.Ord(Ord, Ordering)
import Data.Semigroup(Semigroup((<>)))
import Data.String(String)
import Data.Text(Text)
import qualified Data.Text as Text(cons, snoc, uncons, unsnoc, null, empty, length, compareLength, append, unpack, pack)
import qualified Data.Text.Lazy as LazyText(Text, toStrict, fromStrict)
import Data.Text.Lens ( IsText(builder), text )
import Data.Text1.AsSingle ( AsSingle(..) )
import Data.Text1.IsText1 ( IsText1(packed1, builder1) )
import Data.Tuple(uncurry)
import Data.Typeable(Typeable)
import GHC.Show(Show(show))
import Prelude((-), (+))

data Text1 =
  Text1
    !Char
    !Text
  deriving (Eq, Ord, Data, Typeable)

class AsText1 a where
  _Text1 ::
    Prism'
      a
      Text1

instance AsText1 Text1 where
  _Text1 =
    id

instance AsText1 (NonEmpty Char) where
  _Text1 =
    packed1

instance AsText1 String where
  _Text1 =
    prism'
      (\(Text1 h t) -> h : Text.unpack t)
      (fmap (\(h, t) -> Text1 h (Text.pack t)) . uncons)

instance AsText1 Text where
  _Text1 =
    maybeText . _Just

instance AsText1 LazyText.Text where
  _Text1 =
    maybeLazyText . _Just

class HasText1 a where
  text1 ::
    Lens' a Text1

instance HasText1 Text1 where
  text1 =
    id

instance IsText1 Text1 where
  packed1 =
    iso
      (\(h:|t) -> Text1 h (Text.pack t))
      (\(Text1 h t) -> h :| Text.unpack t)
  builder1 =
    from (cons1 . seconding builder)

instance AsSingle Text1 Char where
  _Single =
    prism'
      (`Text1` Text.empty)
      (\(Text1 h t) -> if Text.null t then Just h else Nothing)

instance Show Text1 where
  show (Text1 h t) =
    show (Text.cons h t)

instance Semigroup Text1 where
  Text1 h1 t1 <> t =
    Text1 h1 (Text.append t1 (_Text1 # t))

instance Binary Text1 where
  put (Text1 h t) =
    put h *> put t
  get =
    Text1 <$> get <*> get

instance Each Text1 Text1 Char Char where
  each f (Text1 h t) =
    Text1 <$> f h <*> text f t

instance Reversing Text1 where
  reversing (Text1 h t) =
    case uncons (reversing t) of
      Nothing -> Text1 h Text.empty
      Just (h', t') -> Text1 h' (Text.snoc t' h)

instance Cons Text1 Text1 Char Char where
  _Cons =
    prism'
      (\(h, Text1 h' t) -> Text1 h (Text.cons h' t))
      (\(Text1 h t) -> fmap (\r' -> (h, uncurry Text1 r')) (Text.uncons t))

instance Snoc Text1 Text1 Char Char where
  _Snoc =
    prism'
      (\(Text1 h t, l) -> Text1 h (Text.snoc t l))
      (\(Text1 h t) -> fmap (over _1 (Text1 h)) (Text.unsnoc t))

type instance Index Text1 = Int
type instance IxValue Text1 = Char
instance Ixed Text1 where
  ix n f (Text1 h t) =
    if n == 0
      then
        fmap (`Text1` t) (f h)
      else
        fmap (Text1 h) (ix (n - 1) f t)

instance Plated Text1 where
  plate f (Text1 h t) =
    case Text.uncons t of
      Nothing ->
        pure (Text1 h Text.empty)
      Just (h', t') ->
        cons h <$> f (Text1 h' t')

cons1 ::
  Iso'
    Text1
    (Char, Text)
cons1 =
  iso
    (\(Text1 h t) -> (h, t))
    (uncurry Text1)

snoc1 ::
  Iso'
    Text1
    (Text, Char)
snoc1 =
  iso
    (\(Text1 h t) -> case Text.unsnoc t of
      Nothing ->
        (Text.empty, h)
      Just (i, l) ->
        (Text.cons h i, l))
    (\(i, l) -> case Text.uncons i of
      Nothing ->
        Text1 l Text.empty
      Just (h, t) ->
        Text1 h (Text.snoc t l)
    )

length1 ::
  Text1
  -> Int
length1 (Text1 _ t) =
  1 + Text.length t

compareLength1 ::
  Text1
  -> Int
  -> Ordering
compareLength1 (Text1 _ t) n =
  Text.compareLength t (n - 1)

_head1 ::
  Lens'
    Text1
    Char
_head1 =
  cons1 . _1

_tail1 ::
  Lens'
    Text1
    Text
_tail1 =
  cons1 . _2

_last1 ::
  Lens'
    Text1
    Char
_last1 =
  snoc1 . _2

_init1 ::
  Lens'
    Text1
    Text
_init1 =
  snoc1 . _1

maybeText ::
  Iso'
    Text
    (Maybe Text1)
maybeText =
  iso
    (fmap (uncurry Text1) . Text.uncons)
    (\case
        Nothing ->
          Text.empty
        Just (Text1 h t) ->
          Text.cons h t)

maybeLazyText ::
  Iso'
    LazyText.Text
    (Maybe Text1)
maybeLazyText =
  iso LazyText.toStrict LazyText.fromStrict . maybeText

each1 ::
  Traversal1' Text1 Char
each1 f (Text1 h t) =
  case Text.uncons t of
    Nothing ->
      (`Text1` Text.empty) <$> f h
    Just (h', t') ->
      cons <$> f h <.> each1 f (Text1 h' t')
