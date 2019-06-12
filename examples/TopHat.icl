implementation module TopHat

import Basics

import qualified iTasks as I
from iTasks import >>*, -&&-, -||-, -||, ||-, :: TaskCont

from iTasks import class Identifiable, instance Identifiable SDSLens
from iTasks import class Readable, instance Readable SDSLens
from iTasks import class Writeable, instance Writeable SDSLens
from iTasks import class Modifiable, instance Modifiable SDSLens
from iTasks import class Registrable, instance Registrable SDSLens

from iTasks import class toPrompt, instance toPrompt String
from iTasks import class Startable, instance Startable (Task a)

from iTasks import class Functor, instance Functor Task
from iTasks import class TApplicative, instance TApplicative Task


// Synonyms ////////////////////////////////////////////////////////////////////

class Storable a | 'I'.iTask a
:: Ref a :== 'I'.SDSLens Unit a a
:: Task a :== 'I'.Task a


// Editors /////////////////////////////////////////////////////////////////////

enter :: Message -> Task a | Storable a
enter label = 'I'.enterInformation label []

update :: Message a -> Task a | Storable a
update label value = 'I'.updateInformation label [] value

view :: Message a -> Task a | Storable a
view label value = 'I'.viewInformation label [] value


// References //////////////////////////////////////////////////////////////////

ref :: Message a -> Ref a | Storable a
ref label value = 'I'.sharedStore label value

withRef :: a ((Ref a) -> Task b) -> Task b | Storable a & Storable b
withRef value cont = 'I'.withShared value cont

modify :: (Ref a) (a -> a) -> Task a | Storable a
modify ref fun = 'I'.upd fun ref

watch :: Message (Ref a) -> Task a | Storable a
watch label ref = 'I'.viewSharedInformation label [] ref

change :: Message (Ref a) -> Task a | Storable a
change label ref = 'I'.updateSharedInformation label [] ref

select :: Message (List a) (Ref (List a)) -> Task (List a) | Storable a
select label default ref = 'I'.updateMultipleChoiceWithShared label [] ref default


// Steps ///////////////////////////////////////////////////////////////////////

(>>>) infixl 1 :: (Task a) (List ( a -> Bool, a -> Task b )) -> Task b | Storable a & Storable b
(>>>) task options = task >>* map trans options
where
  trans ( p, t ) = 'I'.OnValue ('I'.ifValue p t)

(>>=) infixl 1 :: (Task a) (a -> Task b) -> Task b | Storable a & Storable b
(>>=) task cont = task >>> [ ( always, cont ) ]

(>>|) infixl 1 :: (Task a) (Task b) -> Task b | Storable a & Storable b
(>>|) task next = task >>= \_ -> next

(>?>) infixl 1 :: (Task a) (List ( String, a -> Bool, a -> Task b )) -> Task b | Storable a & Storable b
(>?>) task options = task >>* map trans options
where
  trans ( a, p, t ) = 'I'.OnAction ('I'.Action a) ('I'.ifValue p t)

(>?=) infixl 1 :: (Task a) (a -> Task b) -> Task b | Storable a & Storable b
(>?=) task cont = task >?> [ ( "Continue", always, cont ) ]

(>?|) infixl 1 :: (Task a) (Task b) -> Task b | Storable a & Storable b
(>?|) task next = task >?= \_ -> next


// Parallels ///////////////////////////////////////////////////////////////////

(<&>) infixr 4 :: (Task a) (Task b) -> Task ( a, b ) | Storable a & Storable b
(<&>) x y = (-&&-) x y

(<&) infixl 4 :: (Task a) (Task b) -> Task a | Storable a & Storable b
(<&) x y = (-||) x y

(&>) infixr 4 :: (Task a) (Task b) -> Task b | Storable a & Storable b
(&>) x y = (||-) x y

(<|>) infixr 3 :: (Task a) (Task a) -> Task a | Storable a
(<|>) x y = (-||-) x y

(<?>) infixr 3 :: (Task a) (Task a) -> Task a | Storable a
(<?>) fst snd = 'I'.return () >?> [ ( "Left" , always, const fst ), (  "Right", always, const snd ) ]


// Other ///////////////////////////////////////////////////////////////////////

fail :: Task a | Storable a
fail = 'I'.transform (\_ -> 'I'.NoValue) ('I'.return ())

forever :: (Task a) -> Task a | Storable a
forever t =
  t >>= \_ -> t


// Startup /////////////////////////////////////////////////////////////////////

run :: (Task a) *World -> *World | Storable a
run task world = 'I'.startEngine task world
