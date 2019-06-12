definition module TopHat


import Basics

import qualified iTasks as I


// Types ///////////////////////////////////////////////////////////////////////

class Storable a | 'I'.iTask a
:: Ref a :== 'I'.SDSLens Unit a a
:: Task a :== 'I'.Task a

:: Message :== String


// Editors /////////////////////////////////////////////////////////////////////

enter :: Message -> Task a | Storable a
update :: Message a -> Task a | Storable a
view :: Message a -> Task a | Storable a


// References //////////////////////////////////////////////////////////////////

// Create //

ref :: Message a -> Ref a | Storable a
withRef :: a ((Ref a) -> Task b) -> Task b | Storable a & Storable b


// Modify //

(<<-) infixr 2
(<<-) :== modify
modify :: (Ref a) (a -> a) -> Task a | Storable a


// Watch //

watch :: Message (Ref a) -> Task a | Storable a
change :: Message (Ref a) -> Task a | Storable a
select :: Message (List a) (Ref (List a)) -> Task (List a) | Storable a


// Steps ///////////////////////////////////////////////////////////////////////

// Internal //

(>>>) infixl 1 :: (Task a) (List ( a -> Bool, a -> Task b )) -> Task b | Storable a & Storable b
(>>=) infixl 1 :: (Task a) (a -> Task b) -> Task b | Storable a & Storable b
(>>|) infixl 1 :: (Task a) (Task b) -> Task b | Storable a & Storable b


// External //

(>?>) infixl 1 :: (Task a) (List ( String, a -> Bool, a -> Task b )) -> Task b | Storable a & Storable b
(>?=) infixl 1 :: (Task a) (a -> Task b) -> Task b | Storable a & Storable b
(>?|) infixl 1 :: (Task a) (Task b) -> Task b | Storable a & Storable b


// Parallels ///////////////////////////////////////////////////////////////////

(<&>) infixr 4 :: (Task a) (Task b) -> Task ( a, b ) | Storable a & Storable b
(<&) infixl 4 :: (Task a) (Task b) -> Task a | Storable a & Storable b
(&>) infixr 4 :: (Task a) (Task b) -> Task b | Storable a & Storable b

(<|>) infixr 3 :: (Task a) (Task a) -> Task a | Storable a
(<?>) infixr 3 :: (Task a) (Task a) -> Task a | Storable a


// Other ///////////////////////////////////////////////////////////////////////

fail :: Task a | Storable a
forever :: (Task a) -> Task a | Storable a


// Startup /////////////////////////////////////////////////////////////////////

run :: (Task a) *World -> *World | Storable a
