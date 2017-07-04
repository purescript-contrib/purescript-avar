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

type AVarCallback eff a = ((Either Error a → AVarEff eff Unit) → AVarEff eff Unit)

foreign import data AVar ∷ Type → Type

foreign import data AVAR ∷ Effect

foreign import makeEmptyVar ∷ ∀ eff a. AVarEff eff (AVar a)

foreign import makeVar ∷ ∀ eff a. a → AVarEff eff (AVar a)

foreign import isEmptyVar ∷ ∀ eff a. AVar a → AVarEff eff Boolean

killVar ∷ ∀ eff a. Error → AVar a → AVarEff eff Unit
killVar err avar = Fn.runFn4 _killVar Left Right err avar

putVar ∷ ∀ eff a. a → AVar a → AVarCallback eff Unit → AVarEff eff Unit
putVar value avar cb = Fn.runFn5 _putVar Left Right value avar cb

tryPutVar ∷ ∀ eff a. a → AVar a → AVarEff eff Boolean
tryPutVar value avar = Fn.runFn4 _tryPutVar Left Right value avar

takeVar ∷ ∀ eff a. AVar a → AVarCallback eff a → AVarEff eff Unit
takeVar avar cb = Fn.runFn4 _takeVar Left Right avar cb

tryTakeVar ∷ ∀ eff a. AVar a → AVarEff eff (Maybe a)
tryTakeVar avar = Fn.runFn5 _tryTakeVar Left Right Nothing Just avar

readVar ∷ ∀ eff a. AVar a → AVarCallback eff a → AVarEff eff Unit
readVar avar cb = Fn.runFn4 _readVar Left Right avar cb

tryReadVar ∷ ∀ eff a. AVar a → AVarEff eff (Maybe a)
tryReadVar avar = Fn.runFn3 _tryReadVar Nothing Just avar

foreign import _killVar ∷ ∀ eff a. Fn.Fn4 (Error → Either Error a) (a → Either Error a) Error (AVar a) (AVarEff eff Unit)
foreign import _putVar ∷ ∀ eff a. Fn.Fn5 (Error → Either Error a) (a → Either Error a) a (AVar a) (AVarCallback eff Unit) (AVarEff eff Unit)
foreign import _tryPutVar ∷ ∀ eff a. Fn.Fn4 (Error → Either Error a) (a → Either Error a) a (AVar a) (AVarEff eff Boolean)
foreign import _takeVar ∷ ∀ eff a. Fn.Fn4 (Error → Either Error a) (a → Either Error a) (AVar a) (AVarCallback eff a) (AVarEff eff Unit)
foreign import _tryTakeVar ∷ ∀ eff a. Fn.Fn5 (Error → Either Error a) (a → Either Error a) (Maybe a) (a → Maybe a) (AVar a) (AVarEff eff (Maybe a))
foreign import _readVar ∷ ∀ eff a. Fn.Fn4 (Error → Either Error a) (a → Either Error a) (AVar a) (AVarCallback eff a) (AVarEff eff Unit)
foreign import _tryReadVar ∷ ∀ eff a. Fn.Fn3 (Maybe a) (a → Maybe a) (AVar a) (AVarEff eff (Maybe a))
