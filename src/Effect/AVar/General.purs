module Effect.AVar.General
  ( AVar
  , AVarCallback
  , AVarStatus(..)
  , new
  , empty
  , take
  , tryTake
  , put
  , tryPut
  , read
  , tryRead
  , kill
  , status
  , isEmpty
  , isFilled
  , isKilled
  ) where

import Data.Either (Either(..))
import Data.Function.Uncurried as Fn
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Prelude (Unit)

type AVarCallback e a = (Either e a → Effect Unit)

foreign import data AVar ∷ Type → Type → Type

data AVarStatus e a
  = Killed e
  | Filled a
  | Empty

-- | Creates a new empty AVar.
foreign import empty ∷ ∀ e a. Effect (AVar e a)

-- | Creates a fresh AVar with an initial value.
new ∷ ∀ e a. a → Effect (AVar e a)
new = _newVar

-- | Kills the AVar with an exception. All pending and future actions will
-- | resolve immediately with the provided exception.
kill ∷ ∀ e a. e → AVar e a → Effect Unit
kill err avar = Fn.runFn3 _killVar ffiUtil err avar

-- | Sets the value of the AVar. If the AVar is already filled, it will be
-- | queued until the value is emptied. Multiple puts will resolve in order as
-- | the AVar becomes available. Returns an effect which will remove the
-- | callback from the pending queue.
put ∷ ∀ e a. a → AVar e a → AVarCallback e Unit → Effect (Effect Unit)
put value avar cb = Fn.runFn4 _putVar ffiUtil value avar cb

-- | Attempts to synchronously fill an AVar. If the AVar is already filled,
-- | this will do nothing. Returns true or false depending on if it succeeded.
tryPut ∷ ∀ e a. a → AVar e a → Effect Boolean
tryPut value avar = Fn.runFn3 _tryPutVar ffiUtil value avar

-- | Takes the AVar value, leaving it empty. If the AVar is already empty,
-- | the callback will be queued until the AVar is filled. Multiple takes will
-- | resolve in order as the AVar fills. Returns an effect which will remove
-- | the callback from the pending queue.
take ∷ ∀ e a. AVar e a → AVarCallback e a → Effect (Effect Unit)
take avar cb = Fn.runFn3 _takeVar ffiUtil avar cb

-- | Attempts to synchronously take an AVar value, leaving it empty. If the
-- | AVar is empty, this will return `Nothing`.
tryTake ∷ ∀ e a. AVar e a → Effect (Maybe a)
tryTake avar = Fn.runFn2 _tryTakeVar ffiUtil avar

-- | Reads the AVar value. Unlike `take`, this will not leave the AVar empty.
-- | If the AVar is empty, this will queue until it is filled. Multiple reads
-- | will resolve at the same time, as soon as possible.
read ∷ ∀ e a. AVar e a → AVarCallback e a → Effect (Effect Unit)
read avar cb = Fn.runFn3 _readVar ffiUtil avar cb

-- | Attempts to synchronously read an AVar. If the AVar is empty, this will
-- | return `Nothing`.
tryRead ∷ ∀ e a. AVar e a → Effect (Maybe a)
tryRead avar = Fn.runFn2 _tryReadVar ffiUtil avar

-- | Synchronously checks the status of an AVar.
status ∷ ∀ e a. AVar e a → Effect (AVarStatus e a)
status avar = Fn.runFn2 _status ffiUtil avar

isEmpty ∷ ∀ e a. AVarStatus e a → Boolean
isEmpty = case _ of
  Empty → true
  _ → false

isFilled ∷ ∀ e a. AVarStatus e a → Boolean
isFilled = case _ of
  Filled _ → true
  _ → false

isKilled ∷ ∀ e a. AVarStatus e a → Boolean
isKilled = case _ of
  Killed _ → true
  _ → false

foreign import _newVar ∷ ∀ e a. a → Effect (AVar e a)
foreign import _killVar ∷ ∀ e a. Fn.Fn3 FFIUtil e (AVar e a) (Effect Unit)
foreign import _putVar ∷ ∀ e a. Fn.Fn4 FFIUtil a (AVar e a) (AVarCallback e Unit) (Effect (Effect Unit))
foreign import _tryPutVar ∷ ∀ e a. Fn.Fn3 FFIUtil a (AVar e a) (Effect Boolean)
foreign import _takeVar ∷ ∀ e a. Fn.Fn3 FFIUtil (AVar e a) (AVarCallback e a) (Effect (Effect Unit))
foreign import _tryTakeVar ∷ ∀ e a. Fn.Fn2 FFIUtil (AVar e a) (Effect (Maybe a))
foreign import _readVar ∷ ∀ e a. Fn.Fn3 FFIUtil (AVar e a) (AVarCallback e a) (Effect (Effect Unit))
foreign import _tryReadVar ∷ ∀ e a. Fn.Fn2 FFIUtil (AVar e a) (Effect (Maybe a))
foreign import _status ∷ ∀ e a. Fn.Fn2 FFIUtil (AVar e a) (Effect (AVarStatus e a))

type FFIUtil =
  { left ∷ ∀ a b. a → Either a b
  , right ∷ ∀ a b. b → Either a b
  , nothing ∷ ∀ a. Maybe a
  , just ∷ ∀ a. a → Maybe a
  , killed ∷ ∀ e a. e → AVarStatus e a
  , filled ∷ ∀ e a. a → AVarStatus e a
  , empty ∷ ∀ e a. AVarStatus e a
  }

ffiUtil ∷ FFIUtil
ffiUtil =
  { left: Left
  , right: Right
  , nothing: Nothing
  , just: Just
  , killed: Killed
  , filled: Filled
  , empty: Empty
  }

