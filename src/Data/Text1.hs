{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Data.Text1(
  Text1(Text1)
, length1
, compareLength1
, _head1
, _tail1
, _last1
, _init1
, AsText1(_Text1)
, IsText1(packed1, tpacked1, unpacked1, tunpacked1, text1)
, isText1
, AsSingle(_Single)
, OneAnd(_OneAnd)
) where

import Control.Applicative(Applicative)
import Control.Category(Category(id, (.)))
import Control.Lens(Iso, IndexedTraversal', Optic', Profunctor, Choice, Reversing(reversing), Cons(_Cons), Snoc(_Snoc), uncons, unsnoc, Iso', Lens', Prism', prism', iso, lens, (^.), (#), (^?), (%~), _1, _2, from, indexing, traversed)
import Control.Monad(Monad(return, (>>=), (>>)))
import Data.Binary(Binary(put, get))
import Data.Char(Char)
import Data.Data(Data)
import Data.Eq(Eq)
import Data.Foldable(toList)
import Data.Functor(Functor(fmap))
import Data.Int(Int)
import Data.List as List(null)
import Data.List.NonEmpty(NonEmpty((:|)))
import Data.Maybe(Maybe(Just, Nothing))
import Data.Ord(Ord, Ordering)
import Data.Semigroup(Semigroup((<>)))
import Data.String(String)
import Data.Text(Text)
import qualified Data.Text as Text(cons, snoc, append, null, empty, length, compareLength, uncons, pack, unpack, singleton)
import Data.Text.Lens(IsText(packed, builder))
import Data.Traversable(Traversable(traverse))
import Data.Tuple(uncurry)
import Data.Typeable(Typeable)
import Prelude(Show(show), Num((+), (-)))


-- $setup
-- >>> import Control.Lens
-- >>> import Data.Char
-- >>> import qualified Data.Text as Text
-- >>> import Prelude

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
    Text1 h1 (Text.append t1 (_Text1 # t))

instance Binary Text1 where
  put (Text1 h t) =
    put h >> put t
  get =
    do h <- get
       t <- get
       return (Text1 h t)

-- |
--
-- >>> fmap length1 ("a" ^? _Text1)
-- Just 1
--
-- >>> fmap length1 ("abc" ^? _Text1)
-- Just 3
--
length1 ::
  Text1
  -> Int
length1 (Text1 _ t) =
  1 + Text.length t

-- |
--
-- >>> fmap (`compareLength1` 1) ("a" ^? _Text1)
-- Just EQ
--
-- >>> fmap (`compareLength1` 3) ("a" ^? _Text1)
-- Just LT
--
-- >>> fmap (`compareLength1` 1) ("abc" ^? _Text1)
-- Just GT
--
-- >>> fmap (`compareLength1` 3) ("abc" ^? _Text1)
-- Just EQ
--
-- >>> fmap (`compareLength1` 5) ("abc" ^? _Text1)
-- Just LT
--
compareLength1 ::
  Text1
  -> Int
  -> Ordering
compareLength1 (Text1 _ t) n =
  Text.compareLength t (n - 1)

-- |
--
-- >>> fmap (^. _head1) ("a" ^? _Text1)
-- Just 'a'
--
-- >>> fmap (^. _head1) ("abc" ^? _Text1)
-- Just 'a'
--
-- >>> fmap (_head1 %~ toUpper) ("abc" ^? _Text1)
-- Just "Abc"
_head1 ::
  Lens'
    Text1
    Char
_head1 =
  lens
    (\(Text1 h _) -> h)
    (\(Text1 _ t) h -> Text1 h t)

-- |
--
-- >>> fmap (^. _tail1) ("a" ^? _Text1)
-- Just ""
--
-- >>> fmap (^. _tail1) ("abc" ^? _Text1)
-- Just "bc"
--
-- >>> fmap (_tail1 %~ Text.toUpper) ("abc" ^? _Text1)
-- Just "aBC"
_tail1 ::
  Lens'
    Text1
    Text
_tail1 =
  lens
    (\(Text1 _ t) -> t)
    (\(Text1 h _) t -> Text1 h t)

-- |
--
-- >>> fmap (^. _last1) ("a" ^? _Text1)
-- Just 'a'
--
-- >>> fmap (^. _last1) ("abc" ^? _Text1)
-- Just 'c'
--
-- >>> fmap (_last1 %~ toUpper) ("abc" ^? _Text1)
-- Just "abC"
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
                         Just (i, _) -> Text1 h (Text.snoc i x))

-- |
--
-- >>> fmap (^. _init1) ("a" ^? _Text1)
-- Just ""
--
-- >>> fmap (^. _init1) ("abc" ^? _Text1)
-- Just "ab"
--
-- >>> fmap (_init1 %~ Text.toUpper) ("a" ^? _Text1)
-- Just "a"
--
-- >>> fmap (_init1 %~ Text.toUpper) ("abc" ^? _Text1)
-- Just "ABc"
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

class AsText1 p f s where
  _Text1 ::
    Optic' p f s Text1

instance AsText1 p f Text1 where
  _Text1 =
    id

instance (Profunctor p, Functor f) => AsText1 p f (NonEmpty Char) where
  _Text1 =
    packed1

instance (Choice p, Applicative f) => AsText1 p f String where
  _Text1 =
    prism'
      (\(Text1 h t) -> h : Text.unpack t)
      (fmap (\(h, t) -> Text1 h (Text.pack t)) . uncons)

instance (Choice p, Applicative f) => AsText1 p f Text where
  _Text1 =
    prism'
      (\(Text1 h t) -> Text.cons h t)
      (fmap (uncurry Text1) . Text.uncons)

class IsText1 t where
  packed1 :: 
    Iso'
      (NonEmpty Char)
      t

  tpacked1 ::
    Iso'
      Text
      (Maybe t)      
  tpacked1 =
    iso
      (fmap (\(h, t') -> (h :| Text.unpack t') ^. packed1) . Text.uncons)
      (\t -> case t of
               Nothing -> Text.empty
               Just t' -> Text.pack (toList (packed1 # t')))

  unpacked1 ::
    Iso'
      t
      (NonEmpty Char)
  unpacked1 =
    from packed1

  tunpacked1 ::
    Iso'
      (Maybe t)
      Text
  tunpacked1 =
    from tpacked1

  text1 ::
    IndexedTraversal' Int t Char 
  text1 =
    unpacked1 . traversed

instance IsText1 Text1 where
  packed1 =
    iso
      (\(h :| t) -> Text1 h (t ^. packed))
      (\(Text1 h t) -> h :| (packed # t))
  
  tpacked1 =
    iso
      (fmap (\(h, t') -> Text1 h t') . Text.uncons)
      (\t -> case t of
               Nothing -> Text.empty
               Just (Text1 h t') -> Text.cons h t')

instance IsText1 (NonEmpty Char) where
  packed1 =
    id
  text1 =
    indexing traverse

instance IsText (Maybe Text1) where
  packed =
    packed . isText1
  builder =
    from isText1 . builder

instance Reversing Text1 where
  reversing (Text1 h t) =
    case uncons (reversing t) of
      Nothing -> Text1 h Text.empty
      Just (h', t') -> Text1 h' (Text.snoc t' h)

isText1 ::
  Iso' Text (Maybe Text1)
isText1 =
  iso 
    (\x ->
      fmap (\(h, t) -> Text1 h t) (Text.uncons x))
    (\x -> case x of
             Nothing ->
               Text.empty
             Just (Text1 h t) ->
               Text.cons h t)
   
instance Cons (Maybe Text1) (Maybe Text1) Char Char where
  _Cons =
    prism'
      (\(h, t) -> (_Cons # (h, isText1 # t)) ^. isText1)
      (\t -> fmap (_2 %~ (^. isText1)) ((isText1 # t) ^? _Cons))

instance Snoc (Maybe Text1) (Maybe Text1) Char Char where 
  _Snoc =
    prism'
      (\(t, s) -> (_Snoc # (isText1 # t, s)) ^. isText1)
      (\t -> fmap (_1 %~ (^. isText1)) ((isText1 # t) ^? _Snoc))

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

class OneAnd s t a b x y | s -> a, s -> x, t -> b, t -> y, s b -> t, x b -> t, t a -> s, y a -> s where
  _OneAnd ::
    Iso s t (a, x) (b, y)

instance OneAnd Text1 Text1 Char Char Text Text where
  _OneAnd =
    iso
      (\(Text1 h t) -> (h, t))
      (uncurry Text1)

instance OneAnd (NonEmpty a) (NonEmpty b) a b [a] [b] where
  _OneAnd =
    iso
      (\(h :| t) -> (h, t))
      (uncurry (:|))
