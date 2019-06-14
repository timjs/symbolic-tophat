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

class Storable a | 'I'.iTask a
:: Ref a :== 'I'.SDSLens Unit a a
:: Task a :== 'I'.Task a

:: User
  = Internal
  | Id 'I'.UserId


// Editors /////////////////////////////////////////////////////////////////////

enter :: Message -> Task a | Storable a
enter label = 'I'.enterInformation ( label, "" ) []

update :: Message a -> Task a | Storable a
update label value = 'I'.updateInformation ( label, "" ) [] value

view :: Message a -> Task a | Storable a
view label value = 'I'.viewInformation ( label, "" ) [] value


// References //////////////////////////////////////////////////////////////////

ref :: Message a -> Ref a | Storable a
ref label value = 'I'.sharedStore label value

withRef :: a ((Ref a) -> Task b) -> Task b | Storable a & Storable b
withRef value cont = 'I'.withShared value cont

modify :: (Ref a) (a -> a) -> Task Unit | Storable a
modify ref fun = void <| 'I'.upd fun ref

watch :: Message (Ref a) -> Task a | Storable a
watch label ref = 'I'.viewSharedInformation ( label, "" ) [] ref

change :: Message (Ref a) -> Task a | Storable a
change label ref = 'I'.updateSharedInformation ( label, "" ) [] ref

select :: Message (List a) (Ref (List a)) -> Task (List a) | Storable a
select label default ref = 'I'.updateMultipleChoiceWithShared ( label, "" ) [] ref default


// Steps ///////////////////////////////////////////////////////////////////////

(>>>) infixl 1 :: (Task a) (List ( a -> Bool, a -> Task b )) -> Task b | Storable a & Storable b
(>>>) task options = task >>* map trans options
where
  trans ( p, t ) = 'I'.OnValue ('I'.ifValue p t)

(>>=) infixl 1 :: (Task a) (a -> Task b) -> Task b | Storable a & Storable b
(>>=) task cont = task >>> [ ( always, cont ) ]

(>>|) infixl 1 :: (Task a) (Task b) -> Task b | Storable a & Storable b
(>>|) task next = task >>= \_ -> next

(>?>) infixl 1 :: (Task a) (List ( Button, a -> Bool, a -> Task b )) -> Task b | Storable a & Storable b
(>?>) task options = task >>* map trans options
where
  trans ( a, p, t ) = 'I'.OnAction ('I'.Action a) ('I'.ifValue p t)

(>?=) infixl 1 :: (Task a) (a -> Task b) -> Task b | Storable a & Storable b
(>?=) task cont = task >?> [ ( "Continue", always, cont ) ]

(>?|) infixl 1 :: (Task a) (Task b) -> Task b | Storable a & Storable b
(>?|) task next = task >?= \_ -> next


// Parallels ///////////////////////////////////////////////////////////////////

(<&>) infixr 4 :: (Task a) (Task b) -> Task ( a, b ) | Storable a & Storable b
(<&>) x y = (-&&-) x y <<@ 'I'.ApplyLayout 'I'.arrangeHorizontal

(<&) infixl 4 :: (Task a) (Task b) -> Task a | Storable a & Storable b
(<&) x y = (-||) x y <<@ 'I'.ApplyLayout 'I'.arrangeHorizontal

(&>) infixr 4 :: (Task a) (Task b) -> Task b | Storable a & Storable b
(&>) x y = (||-) x y <<@ 'I'.ApplyLayout 'I'.arrangeHorizontal

(<|>) infixr 3 :: (Task a) (Task a) -> Task a | Storable a
(<|>) x y = (-||-) x y <<@ 'I'.ApplyLayout 'I'.arrangeHorizontal

(<?>) infixr 3 :: ( Button, Bool, Task a ) ( Button, Bool, Task a ) -> Task a | Storable a
(<?>) ( m1, p1, t1 ) ( m2, p2, t2 ) =
  view "Please make a choice" () >?> [ ( m1 , const p1, const t1 ), ( m2, const p2, const t2 ) ]


// Other ///////////////////////////////////////////////////////////////////////

done :: a -> Task a | Storable a
done a = view "" a

fail :: Task a | Storable a
fail = 'I'.transform (\_ -> 'I'.NoValue) ('I'.return ())

forever :: (Task a) -> Task a | Storable a
forever t =
  t >>= \_ -> t

(@) infix 5 :: User (Task a) -> Task a | Storable a
(@) Internal t = t <<@ 'I'.NoUserInterface
(@) (Id i) t = i @: t


// Startup /////////////////////////////////////////////////////////////////////

run :: (Task a) *World -> *World | Storable a
run task world = 'I'.startEngine task world
