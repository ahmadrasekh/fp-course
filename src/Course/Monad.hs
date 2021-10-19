{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Course.Monad where

import Course.Applicative
import Course.Core
import Course.ExactlyOne
import Course.Functor
import Course.List
import qualified Course.List as List
import Course.Optional
import Text.ParserCombinators.ReadPrec (lift)
import qualified Prelude as P ((=<<))

-- | All instances of the `Monad` type-class must satisfy one law. This law
-- is not checked by the compiler. This law is given as:
--
-- * The law of associativity
--   `∀f g x. g =<< (f =<< x) ≅ ((g =<<) . f) =<< x`
class Applicative k => Monad k where
  -- Pronounced, bind.
  (=<<) :: (a -> k b) -> k a -> k b

infixr 1 =<<

-- | Binds a function on the ExactlyOne monad.
--
-- >>> (\x -> ExactlyOne(x+1)) =<< ExactlyOne 2
-- ExactlyOne 3
instance Monad ExactlyOne where
  (=<<) :: (a -> ExactlyOne b) -> ExactlyOne a -> ExactlyOne b
  (=<<) f (ExactlyOne x) = f x

-- | Binds a function on a List.
--
-- >>> (\n -> n :. n :. Nil) =<< (1 :. 2 :. 3 :. Nil)
-- [1,1,2,2,3,3]
instance Monad List where
  (=<<) :: (a -> List b) -> List a -> List b
  (=<<) _ Nil = Nil
  (=<<) f (hd :. tl) = f hd ++ (f =<< tl)

-- | Binds a function on an Optional.
--
-- >>> (\n -> Full (n + n)) =<< Full 7
-- Full 14
instance Monad Optional where
  (=<<) :: (a -> Optional b) -> Optional a -> Optional b
  (=<<) _ Empty = Empty
  (=<<) f (Full a) = f a

-- | Binds a function on the reader ((->) t).
--
-- >>> ((*) =<< (+10)) 7
-- 119
instance Monad ((->) t) where
  (=<<) :: (a -> ((->) t b)) -> ((->) t a) -> ((->) t b)
  -- (=<<) :: (a -> t -> b) -> (t -> a) -> (t -> b)
  -- apply (t -> a) to some t, get a,
  -- apply (a -> t -> b) to a and t, to get b,
  -- abstract on t
  (=<<) f g t = f (g t) t

-- | Witness that all things with (=<<) and (<$>) also have (<*>).
--
-- >>> ExactlyOne (+10) <**> ExactlyOne 8
-- ExactlyOne 18
--
-- >>> (+1) :. (*2) :. Nil <**> 1 :. 2 :. 3 :. Nil
-- [2,3,4,2,4,6]
--
-- >>> Full (+8) <**> Full 7
-- Full 15
--
-- >>> Empty <**> Full 7
-- Empty
--
-- >>> Full (+8) <**> Empty
-- Empty
--
-- >>> ((+) <**> (+10)) 3
-- 16
--
-- >>> ((+) <**> (+5)) 3
-- 11
--
-- >>> ((+) <**> (+5)) 1
-- 7
--
-- >>> ((*) <**> (+10)) 3
-- 39
--
-- >>> ((*) <**> (+2)) 3
-- 15
-- (<**>) fg fa = (\x-> (\g-> g x) <$> fg)=<< fa -- proof of Functor, Monad -> Applicative
(<**>) :: Monad k => k (a -> b) -> k a -> k b
-- we want to complete this definition: <**> bf bx = ...
--
-- we have  =<< :: (a' -> k b') -> k a' -> k b'
--
-- substitute (a -> b) for a' and b for b' :
--
-- =<< :: ((a -> b) -> k b) -> k (a -> b) -> k b, then we can bind on k(a -> b)
-- <**> kf ka = ... =<< kf
--
-- but notice that ((a -> b) -> k b) is just partially applied (a -> b) -> ka -> kb
-- which is <$>, partially applied.
-- And we already have ka so `...` becomes \g -> (g <$> ka)
(<**>) kf ka = (<$> ka) =<< kf

infixl 4 <**>

-- | Flattens a combined structure to a single structure.
--
-- >>> join ((1 :. 2 :. 3 :. Nil) :. (1 :. 2 :. Nil) :. Nil)
-- [1,2,3,1,2]
--
-- >>> join (Full Empty)
-- Empty
--
-- >>> join (Full (Full 7))
-- Full 7
--
-- >>> join (+) 7
-- 14
join :: Monad k => k (k a) -> k a
-- again start from bind =<< :: (a' -> k b') -> k a' -> k b'
-- substituting (k a) for a' gives us
-- (k a -> k b') -> k (k a) -> k b'
-- substituting a for b' gives us
-- (k a -> k a) -> k (k a) -> k a
-- so we can get something like
-- join kka = f =<< kka,  where f :: (k a -> k a), which is just the id function
join kka = id =<< kka

-- join = error "todo: Course.Monad#join"

-- | Implement a flipped version of @(=<<)@, however, use only
-- @join@ and @(<$>)@.
-- Pronounced, bind flipped.
--
-- >>> ((+10) >>= (*)) 7
-- 119
(>>=) :: Monad k => k a -> (a -> k b) -> k b
-- <$> :: (a' -> b') -> k a' -> k b', substitute k b for b':
-- <$> :: (a' -> k b) -> k a' -> k k b, substitute a for a'
-- k a -> (a -> k b) ->  k k b
(>>=) ka f = join (f <$> ka)

infixl 1 >>=

-- | Implement composition within the @Monad@ environment.
-- Pronounced, Kleisli composition.
--
-- >>> ((\n -> n :. n :. Nil) <=< (\n -> n+1 :. n+2 :. Nil)) 1
-- [2,2,3,3]
(<=<) :: Monad k => (b -> k c) -> (a -> k b) -> a -> k c
-- recall composition::
-- for some f :: a -> b, and g :: b -> c
-- g . f      :: (b -> c)   -> (a -> b)   -> a -> c
-- from above :: (b -> k c) -> (a -> k b) -> a -> k c
-- (f a) :: k b
-- recall =<< :: (a' -> k b') -> k a' -> k b', substitute b for a', and c for b'
-- recall =<< :: (b -> k c) -> k b -> k c, then we simply have
(<=<) g f a = g =<< f a

infixr 1 <=<

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

instance Monad IO where
  (=<<) =
    (P.=<<)
