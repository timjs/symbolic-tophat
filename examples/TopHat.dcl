definition module TopHat


import Basics

import qualified iTasks as I


// Types ///////////////////////////////////////////////////////////////////////

class Editable a | 'I'.iTask a
:: Ref a :== 'I'.SDSLens Unit a a
:: Task a :== 'I'.Task a

:: User
  = Internal
  | Id 'I'.UserId

:: Message :== String
:: Button :== String


// Editors /////////////////////////////////////////////////////////////////////

enter :: Message -> Task a | Editable a
update :: Message a -> Task a | Editable a
view :: Message a -> Task a | Editable a


// References //////////////////////////////////////////////////////////////////

// Create //

ref :: Message a -> Ref a | Editable a
withRef :: a ((Ref a) -> Task b) -> Task b | Editable a & Editable b


// Modify //

(<<-) infixr 2
(<<-) :== modify
modify :: (Ref a) (a -> a) -> Task Unit | Editable a


// Watch //

watch :: Message (Ref a) -> Task a | Editable a
change :: Message (Ref a) -> Task a | Editable a
select :: Message (List a) (Ref (List a)) -> Task (List a) | Editable a


// Steps ///////////////////////////////////////////////////////////////////////

// Internal //

(>>>) infixl 1 :: (Task a) (List ( a -> Bool, a -> Task b )) -> Task b | Editable a & Editable b
(>>=) infixl 1 :: (Task a) (a -> Task b) -> Task b | Editable a & Editable b
(>>|) infixl 1 :: (Task a) (Task b) -> Task b | Editable a & Editable b


// External //

(>?>) infixl 1 :: (Task a) (List ( Button, a -> Bool, a -> Task b )) -> Task b | Editable a & Editable b
(>?=) infixl 1 :: (Task a) (a -> Task b) -> Task b | Editable a & Editable b
(>?|) infixl 1 :: (Task a) (Task b) -> Task b | Editable a & Editable b


// Parallels ///////////////////////////////////////////////////////////////////

(<&>) infixr 4 :: (Task a) (Task b) -> Task ( a, b ) | Editable a & Editable b
(<&) infixl 4 :: (Task a) (Task b) -> Task a | Editable a & Editable b
(&>) infixr 4 :: (Task a) (Task b) -> Task b | Editable a & Editable b

(<|>) infixr 3 :: (Task a) (Task a) -> Task a | Editable a
(<?>) infixr 3 :: ( Button, Bool, Task a ) ( Button, Bool, Task a ) -> Task a | Editable a


// Other ///////////////////////////////////////////////////////////////////////

done :: a -> Task a | Editable a
fail :: Task a | Editable a
forever :: (Task a) -> Task a | Editable a
(@) infix 5 :: User (Task a) -> Task a | Editable a


// Startup /////////////////////////////////////////////////////////////////////

run :: (Task a) *World -> *World | Editable a
