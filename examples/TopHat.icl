implementation module TopHat

import Basics

import qualified iTasks as I
from iTasks import >>*, -&&-, -||-, -||, ||-, <<@, @:, :: TaskCont

from iTasks import class Identifiable, instance Identifiable SDSLens
from iTasks import class Readable, instance Readable SDSLens
from iTasks import class Writeable, instance Writeable SDSLens
from iTasks import class Modifiable, instance Modifiable SDSLens
from iTasks import class Registrable, instance Registrable SDSLens

from iTasks import class toPrompt, instance toPrompt ( String, String )
from iTasks import class toUserConstraint, instance toUserConstraint UserId, :: UserId
from iTasks import class tune, instance tune ApplyLayout Task, :: ApplyLayout, instance tune NoUserInterface Task, :: NoUserInterface
from iTasks import class Startable, instance Startable (Task a)

from iTasks import class Functor, instance Functor Task
from iTasks import class TApplicative, instance TApplicative Task


// Types ///////////////////////////////////////////////////////////////////////
// Note: We need these repetitions because Clean does not copy them automatically from the .dcl to the .icl...

class Editable a | 'I'.iTask a
:: Ref a :== 'I'.SDSLens Unit a a
:: Task a :== 'I'.Task a

:: User
  = Internal
  | Id 'I'.UserId


// Editors /////////////////////////////////////////////////////////////////////

enter :: Message -> Task a | Editable a
enter label = 'I'.enterInformation ( label, "" ) []

update :: Message a -> Task a | Editable a
update label value = 'I'.updateInformation ( label, "" ) [] value

view :: Message a -> Task a | Editable a
view label value = 'I'.viewInformation ( label, "" ) [] value


// References //////////////////////////////////////////////////////////////////

ref :: Message a -> Ref a | Editable a
ref label value = 'I'.sharedStore label value

withRef :: a ((Ref a) -> Task b) -> Task b | Editable a & Editable b
withRef value cont = 'I'.withShared value cont

modify :: (Ref a) (a -> a) -> Task Unit | Editable a
modify ref fun = void <| 'I'.upd fun ref

watch :: Message (Ref a) -> Task a | Editable a
watch label ref = 'I'.viewSharedInformation ( label, "" ) [] ref

change :: Message (Ref a) -> Task a | Editable a
change label ref = 'I'.updateSharedInformation ( label, "" ) [] ref

select :: Message (List a) (Ref (List a)) -> Task (List a) | Editable a
select label default ref = 'I'.updateMultipleChoiceWithShared ( label, "" ) [] ref default


// Steps ///////////////////////////////////////////////////////////////////////

(>>>) infixl 1 :: (Task a) (List ( a -> Bool, a -> Task b )) -> Task b | Editable a & Editable b
(>>>) task options = task >>* map trans options
where
  trans ( p, t ) = 'I'.OnValue ('I'.ifValue p t)

(>>=) infixl 1 :: (Task a) (a -> Task b) -> Task b | Editable a & Editable b
(>>=) task cont = task >>> [ ( always, cont ) ]

(>>|) infixl 1 :: (Task a) (Task b) -> Task b | Editable a & Editable b
(>>|) task next = task >>= \_ -> next

(>?>) infixl 1 :: (Task a) (List ( Button, a -> Bool, a -> Task b )) -> Task b | Editable a & Editable b
(>?>) task options = task >>* map trans options
where
  trans ( a, p, t ) = 'I'.OnAction ('I'.Action a) ('I'.ifValue p t)

(>?=) infixl 1 :: (Task a) (a -> Task b) -> Task b | Editable a & Editable b
(>?=) task cont = task >?> [ ( "Continue", always, cont ) ]

(>?|) infixl 1 :: (Task a) (Task b) -> Task b | Editable a & Editable b
(>?|) task next = task >?= \_ -> next


// Parallels ///////////////////////////////////////////////////////////////////

(<&>) infixr 4 :: (Task a) (Task b) -> Task ( a, b ) | Editable a & Editable b
(<&>) x y = (-&&-) x y <<@ 'I'.ApplyLayout 'I'.arrangeHorizontal

(<&) infixl 4 :: (Task a) (Task b) -> Task a | Editable a & Editable b
(<&) x y = (-||) x y <<@ 'I'.ApplyLayout 'I'.arrangeHorizontal

(&>) infixr 4 :: (Task a) (Task b) -> Task b | Editable a & Editable b
(&>) x y = (||-) x y <<@ 'I'.ApplyLayout 'I'.arrangeHorizontal

(<|>) infixr 3 :: (Task a) (Task a) -> Task a | Editable a
(<|>) x y = (-||-) x y <<@ 'I'.ApplyLayout 'I'.arrangeHorizontal

(<?>) infixr 3 :: ( Button, Bool, Task a ) ( Button, Bool, Task a ) -> Task a | Editable a
(<?>) ( m1, p1, t1 ) ( m2, p2, t2 ) =
  'I'.return () >?> [ ( m1 , const p1, const t1 ), ( m2, const p2, const t2 ) ]


// Other ///////////////////////////////////////////////////////////////////////

done :: a -> Task a | Editable a
done a = 'I'.return a

fail :: Task a | Editable a
fail = 'I'.transform (\_ -> 'I'.NoValue) ('I'.return ())

forever :: (Task a) -> Task a | Editable a
forever t =
  t >>= \_ -> t

(@) infix 5 :: User (Task a) -> Task a | Editable a
(@) Internal t = t <<@ 'I'.NoUserInterface
(@) (Id i) t = i @: t


// Startup /////////////////////////////////////////////////////////////////////

run :: (Task a) *World -> *World | Editable a
run task world = 'I'.startEngine task world
