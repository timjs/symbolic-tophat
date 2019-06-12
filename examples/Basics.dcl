definition module Basics

import StdBool
import Data.List


// Synonyms ////////////////////////////////////////////////////////////////////

:: Unit :== ()
:: List a :== [a]


// Functions ///////////////////////////////////////////////////////////////////

(<|) infixr 0 // :: (a -> b) a -> b
(<|) f x :== f x

(|>) infixr 0 // :: a (a -> b) -> b
(|>) x f :== f x

// identity :: a -> a
identity x :== x

// const :: a b -> a
const x y :== x

// always :: b -> Bool
always y :== const True y

// flip :: (a -> (b -> c)) b a -> c
flip f x y :== f y x

(|||) infixr 2 //:: (a -> Bool) -> (a -> Bool) -> a -> Bool
(|||) l r x :== l x || r x

(&&&) infixr 3 //:: (a -> Bool) -> (a -> Bool) -> a -> Bool
(&&&) l r x :== l x && r x


// Lists ///////////////////////////////////////////////////////////////////////
