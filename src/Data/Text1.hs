{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Data.Text1(
  Text1(Text1)
, length
, compareLength
, _text
, _string
, _head1
, _tail1
, _last1
, _init1
, IsText1(packed1, text1)
, unpacked1
, AsSingle(_Single)
) where

import Control.Category(Category(id, (.)))
import Control.Lens(IndexedTraversal', Cons(_Cons), Snoc(_Snoc), Reversing(reversing), uncons, unsnoc, Iso', Lens', Prism', prism', iso, lens, (^.), (#), from, indexing, traversed)
import Control.Monad(Monad(return, (>>=), (>>)))
import Data.Binary(Binary(put, get))
import Data.Char(Char)
import Data.Data(Data)
import Data.Eq(Eq)
import Data.Functor(Functor(fmap))
import Data.Int(Int)
import Data.List as List(null)
import Data.List.NonEmpty(NonEmpty((:|)))
import Data.Maybe(Maybe(Just, Nothing))
import Data.Ord(Ord, Ordering)
import Data.Semigroup(Semigroup((<>)))
import Data.String(String)
import Data.Text(Text)
import qualified Data.Text as Text(cons, snoc, append, null, init, last, empty, length, compareLength, uncons, pack, unpack, singleton)
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
  Text1 h1 t1 <> t = 
    Text1 h1 (Text.append t1 (_text # t))

instance Binary Text1 where
  put (Text1 h t) =
    put h >> put t
  get =
    do h <- get
       t <- get
       return (Text1 h t)

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
    (fmap (\(h, t) -> Text1 h (Text.pack t)) . uncons)

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

_last1 ::
  Lens'
    Text1
    Char
_last1 =
  lens
    (\(Text1 h t) -> case unsnoc t of
                       Nothing -> h
                       Just (_, l) -> l)
    (\(Text1 h t) x -> case unsnoc t of
                         Nothing -> Text1 x t
                         Just (i, _) -> Text1 h i)

_init1 ::
  Lens'
    Text1
    Text
_init1 =
  lens
    (\(Text1 h t) -> case unsnoc t of
                       Nothing -> Text.empty
                       Just (i, _) -> Text.cons h i)
    (\(Text1 h t) x ->
      let r = case unsnoc t of
                Nothing -> h
                Just (_, l) -> l
      in case uncons x of
                Nothing -> Text1 r Text.empty
                Just (h', t') -> Text1 h' (Text.snoc t' r))

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
      (\(c, t) -> Text1 c (_text # t))
      (\(Text1 h t) -> fmap (\(h', t') -> (h, Text1 h' t')) (Text.uncons t))

instance Snoc Text1 Text1 Char Char where
  _Snoc =
    prism'
      (\(Text1 h t, c) -> Text1 h (Text.snoc t c))
      (\(Text1 h t) -> if Text.null t
                         then
                           Nothing
                         else
                           Just (Text1 h (Text.init t), Text.last t))

instance Reversing Text1 where
  reversing (Text1 h t) =
    case uncons (reversing t) of
      Nothing -> Text1 h Text.empty
      Just (h', t') -> Text1 h' (Text.snoc t' h)

----
-- The following should be in a lens-based package
----

class AsSingle c a | c -> a where
  _Single :: Prism' c a

instance AsSingle [a] a where
  _Single =
    prism'
      (\a -> [a])
      (\c -> case c of 
               [a] -> Just a
               _   -> Nothing)

instance AsSingle Text Char where
  _Single =
    prism'
      Text.singleton
      (\t -> uncons t >>= \(h, t') -> if Text.null t' then Just h else Nothing)

instance AsSingle (Maybe a) a where
  _Single =
    prism'
      Just
      id

instance AsSingle (NonEmpty a) a where
  _Single =
    prism'
      (\a -> a :| [])
      (\(h :| t) -> if List.null t then Just h else Nothing)

instance AsSingle Text1 Char where
  _Single =
    prism'
      (\c -> Text1 c Text.empty)
      (\(Text1 h t) -> if Text.null t then Just h else Nothing)