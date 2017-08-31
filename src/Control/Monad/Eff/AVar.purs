module Control.Monad.Eff.AVar
  ( AVar
  , AVAR
  , AVarEff
  , AVarCallback
  , AVarStatus(..)
  , makeVar
  , makeEmptyVar
  , takeVar
  , tryTakeVar
  , putVar
  , tryPutVar
  , readVar
  , tryReadVar
  , killVar
  , status
  , isEmptyVar
  , isFilledVar
  , isKilledVar
  , isEmpty
  , isFilled
  , isKilled
  ) where

import Prelude
import Control.Monad.Eff (kind Effect, Eff)
import Control.Monad.Eff.Exception (Error)
import Data.Either (Either(..))
import Data.Function.Uncurried as Fn
import Data.Maybe (Maybe(..))

type AVarEff eff = Eff (avar ∷ AVAR | eff)

type AVarCallback eff a = (Either Error a → AVarEff eff Unit)

foreign import data AVar ∷ Type → Type

foreign import data AVAR ∷ Effect

data AVarStatus a
  = Killed Error
  | Filled a
  | Empty

-- | Creates a fresh AVar.
foreign import makeEmptyVar ∷ ∀ eff a. AVarEff eff (AVar a)

-- | Creates a fresh AVar with an initial value.
foreign import makeVar ∷ ∀ eff a. a → AVarEff eff (AVar a)

-- | Kills the AVar with an exception. All pending and future actions will
-- | resolve immediately with the provided exception.
killVar ∷ ∀ eff a. Error → AVar a → AVarEff eff Unit
killVar err avar = Fn.runFn3 _killVar ffiUtil err avar

-- | Sets the value of the AVar. If the AVar is already filled, it will be
-- | queued until the value is emptied. Multiple puts will resolve in order as
-- | the AVar becomes available. Returns an effect which will remove the
-- | callback from the pending queue.
putVar ∷ ∀ eff a. a → AVar a → AVarCallback eff Unit → AVarEff eff (AVarEff eff Unit)
putVar value avar cb = Fn.runFn4 _putVar ffiUtil value avar cb

-- | Attempts to synchronously fill an AVar. If the AVar is already filled,
-- | this will do nothing. Returns true or false depending on if it succeeded.
tryPutVar ∷ ∀ eff a. a → AVar a → AVarEff eff Boolean
tryPutVar value avar = Fn.runFn3 _tryPutVar ffiUtil value avar

-- | Takes the AVar value, leaving it empty. If the AVar is already empty,
-- | the callback will be queued until the AVar is filled. Multiple takes will
-- | resolve in order as the AVar fills. Returns an effect which will remove
-- | the callback from the pending queue.
takeVar ∷ ∀ eff a. AVar a → AVarCallback eff a → AVarEff eff (AVarEff eff Unit)
takeVar avar cb = Fn.runFn3 _takeVar ffiUtil avar cb

-- | Attempts to synchronously take an AVar value, leaving it empty. If the
-- | AVar is empty, this will return `Nothing`.
tryTakeVar ∷ ∀ eff a. AVar a → AVarEff eff (Maybe a)
tryTakeVar avar = Fn.runFn2 _tryTakeVar ffiUtil avar

-- | Reads the AVar value. Unlike `takeVar`, this will not leave the AVar empty.
-- | If the AVar is empty, this will queue until it is filled. Multiple reads
-- | will resolve at the same time, as soon as possible.
readVar ∷ ∀ eff a. AVar a → AVarCallback eff a → AVarEff eff (AVarEff eff Unit)
readVar avar cb = Fn.runFn3 _readVar ffiUtil avar cb

-- | Attempts to synchronously read an AVar. If the AVar is empty, this will
-- | return `Nothing`.
tryReadVar ∷ ∀ eff a. AVar a → AVarEff eff (Maybe a)
tryReadVar avar = Fn.runFn2 _tryReadVar ffiUtil avar

-- | Synchronously checks the status of an AVar.
status ∷ ∀ eff a. AVar a → AVarEff eff (AVarStatus a)
status avar = Fn.runFn2 _status ffiUtil avar

-- | Synchronously checks whether an AVar currently is empty.
isEmptyVar ∷ ∀ eff a. AVar a → AVarEff eff Boolean
isEmptyVar = map isEmpty <<< status

-- | Synchronously checks whether an AVar currently has a value.
isFilledVar ∷ ∀ eff a. AVar a → AVarEff eff Boolean
isFilledVar = map isFilled <<< status

-- | Synchronously checks whether an AVar has been killed.
isKilledVar ∷ ∀ eff a. AVar a → AVarEff eff Boolean
isKilledVar = map isKilled <<< status

isEmpty ∷ ∀ a. AVarStatus a → Boolean
isEmpty = case _ of
  Empty → true
  _ → false

isFilled ∷ ∀ a. AVarStatus a → Boolean
isFilled = case _ of
  Filled _ → true
  _ → false

isKilled ∷ ∀ a. AVarStatus a → Boolean
isKilled = case _ of
  Killed _ → true
  _ → false

foreign import _killVar ∷ ∀ eff a. Fn.Fn3 FFIUtil Error (AVar a) (AVarEff eff Unit)
foreign import _putVar ∷ ∀ eff a. Fn.Fn4 FFIUtil a (AVar a) (AVarCallback eff Unit) (AVarEff eff (AVarEff eff Unit))
foreign import _tryPutVar ∷ ∀ eff a. Fn.Fn3 FFIUtil a (AVar a) (AVarEff eff Boolean)
foreign import _takeVar ∷ ∀ eff a. Fn.Fn3 FFIUtil (AVar a) (AVarCallback eff a) (AVarEff eff (AVarEff eff Unit))
foreign import _tryTakeVar ∷ ∀ eff a. Fn.Fn2 FFIUtil (AVar a) (AVarEff eff (Maybe a))
foreign import _readVar ∷ ∀ eff a. Fn.Fn3 FFIUtil (AVar a) (AVarCallback eff a) (AVarEff eff (AVarEff eff Unit))
foreign import _tryReadVar ∷ ∀ eff a. Fn.Fn2 FFIUtil (AVar a) (AVarEff eff (Maybe a))
foreign import _status ∷ ∀ eff a. Fn.Fn2 FFIUtil (AVar a) (AVarEff eff (AVarStatus a))

type FFIUtil =
  { left ∷ ∀ a b. a → Either a b
  , right ∷ ∀ a b. b → Either a b
  , nothing ∷ ∀ a. Maybe a
  , just ∷ ∀ a. a → Maybe a
  , killed ∷ ∀ a. Error → AVarStatus a
  , filled ∷ ∀ a. a → AVarStatus a
  , empty ∷ ∀ a. AVarStatus a
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
