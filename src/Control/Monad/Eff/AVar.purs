module Control.Monad.Eff.AVar
  ( AVar
  , AVAR
  , AVarEff
  , AVarCallback
  , makeVar
  , makeEmptyVar
  , isEmptyVar
  , takeVar
  , tryTakeVar
  , putVar
  , tryPutVar
  , readVar
  , tryReadVar
  , killVar
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

-- | Creates a fresh AVar.
foreign import makeEmptyVar ∷ ∀ eff a. AVarEff eff (AVar a)

-- | Creates a fresh AVar with an initial value.
foreign import makeVar ∷ ∀ eff a. a → AVarEff eff (AVar a)

-- | Synchronously checks whether an AVar currently has a value.
foreign import isEmptyVar ∷ ∀ eff a. AVar a → AVarEff eff Boolean

-- | Kills the AVar with an exception. All pending and future actions will
-- | resolve immediately with the provided exception.
killVar ∷ ∀ eff a. AVar a → Error → AVarEff eff Unit
killVar avar err = Fn.runFn4 _killVar Left Right avar err

-- | Sets the value of the AVar. If the AVar is already filled, it will be
-- | queued until the value is emptied. Multiple puts will resolve in order as
-- | the AVar becomes available. Returns an effect which will remove the
-- | callback from the pending queue.
putVar ∷ ∀ eff a. AVar a → a → AVarCallback eff Unit → AVarEff eff (AVarEff eff Unit)
putVar avar value cb = Fn.runFn5 _putVar Left Right avar value cb

-- | Attempts to synchronously fill an AVar. If the AVar is already filled,
-- | this will do nothing. Returns true or false depending on if it succeeded.
tryPutVar ∷ ∀ eff a. AVar a → a → AVarEff eff Boolean
tryPutVar avar value = Fn.runFn4 _tryPutVar Left Right avar value

-- | Takes the AVar value, leaving it empty. If the AVar is already empty,
-- | the callback will be queued until the AVar is filled. Multiple takes will
-- | resolve in order as the AVar fills. Returns an effect which will remove
-- | the callback from the pending queue.
takeVar ∷ ∀ eff a. AVar a → AVarCallback eff a → AVarEff eff (AVarEff eff Unit)
takeVar avar cb = Fn.runFn4 _takeVar Left Right avar cb

-- | Attempts to synchronously take an AVar value, leaving it empty. If the
-- | AVar is empty, this will return `Nothing`.
tryTakeVar ∷ ∀ eff a. AVar a → AVarEff eff (Maybe a)
tryTakeVar avar = Fn.runFn5 _tryTakeVar Left Right Nothing Just avar

-- | Reads the AVar value. Unlike `takeVar`, this will not leave the AVar empty.
-- | If the AVar is empty, this will queue until it is filled. Multiple reads
-- | will resolve at the same time, as soon as possible.
readVar ∷ ∀ eff a. AVar a → AVarCallback eff a → AVarEff eff (AVarEff eff Unit)
readVar avar cb = Fn.runFn4 _readVar Left Right avar cb

-- | Attempts to synchronously read an AVar. If the AVar is empty, this will
-- | return `Nothing`.
tryReadVar ∷ ∀ eff a. AVar a → AVarEff eff (Maybe a)
tryReadVar avar = Fn.runFn3 _tryReadVar Nothing Just avar

foreign import _killVar ∷ ∀ eff a. Fn.Fn4 (Error → Either Error a) (a → Either Error a) (AVar a) Error (AVarEff eff Unit)
foreign import _putVar ∷ ∀ eff a. Fn.Fn5 (Error → Either Error a) (a → Either Error a) (AVar a) a (AVarCallback eff Unit) (AVarEff eff (AVarEff eff Unit))
foreign import _tryPutVar ∷ ∀ eff a. Fn.Fn4 (Error → Either Error a) (a → Either Error a) (AVar a) a (AVarEff eff Boolean)
foreign import _takeVar ∷ ∀ eff a. Fn.Fn4 (Error → Either Error a) (a → Either Error a) (AVar a) (AVarCallback eff a) (AVarEff eff (AVarEff eff Unit))
foreign import _tryTakeVar ∷ ∀ eff a. Fn.Fn5 (Error → Either Error a) (a → Either Error a) (Maybe a) (a → Maybe a) (AVar a) (AVarEff eff (Maybe a))
foreign import _readVar ∷ ∀ eff a. Fn.Fn4 (Error → Either Error a) (a → Either Error a) (AVar a) (AVarCallback eff a) (AVarEff eff (AVarEff eff Unit))
foreign import _tryReadVar ∷ ∀ eff a. Fn.Fn3 (Maybe a) (a → Maybe a) (AVar a) (AVarEff eff (Maybe a))
