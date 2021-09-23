module Effect.AVar
  ( AVar
  , AVarCallback
  , AVarStatus(..)
  , new
  , empty
  , take
  , tryTake
  , put
  , Operation(..)
  , windowPut
  , tryPut
  , read
  , tryRead
  , kill
  , status
  , isEmpty
  , isFilled
  , isKilled
  ) where

import Prelude
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Function.Uncurried as Fn
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception (Error)

type AVarCallback a = (Either Error a → Effect Unit)

foreign import data AVar ∷ Type → Type

type role AVar representational

data AVarStatus a
  = Killed Error
  | Filled a
  | Empty

data Operation
  = Ignore -- Do nothing with the queue
  | Fail Error -- Propagate an exception to the callback
  | Halt Error -- Kill the internal queue and propagate the exception
  | PushHead -- Push callback onto the head
  | PushTail -- Push callback onto the tail
  | DropHead Error -- Drop the head, and push onto the tail
  | DropTail Error -- Drop the tail, and push onto the head
  | SwapHead Error -- Replace the head
  | SwapTail Error -- Replace the tail

-- | Creates a new empty AVar.
foreign import empty ∷ ∀ a. Effect (AVar a)

-- | Creates a fresh AVar with an initial value.
new ∷ ∀ a. a → Effect (AVar a)
new = _newVar

-- | Kills the AVar with an exception. All pending and future actions will
-- | resolve immediately with the provided exception.
kill ∷ ∀ a. Error → AVar a → Effect Unit
kill err avar = Fn.runFn3 _killVar ffiUtil err avar

-- | Sets the value of the AVar. If the AVar is already filled, it will be
-- | queued until the value is emptied. Multiple puts will resolve in order as
-- | the AVar becomes available. Returns an effect which will remove the
-- | callback from the pending queue.
put ∷ ∀ a. a → AVar a → AVarCallback Unit → Effect (Effect Unit)
put = windowPut (const PushTail)

-- | Puts a value into an AVar using a strategy determined
-- | dynamically on the basis of the number of pending puts
-- | (i.e. not including the current value of the AVar, if non-EMPTY).
windowPut
  :: ∀ a
   . (Int -> Operation)
  -> a
  -> AVar a
  -> AVarCallback Unit
  -> Effect (Effect Unit)
windowPut strategy value avar cb =
  case strategy (_pendingPuts avar) of
    -- Do nothing with the queue
    Ignore -> pure $ pure unit
    -- Propagate an exception to the callback
    Fail e -> do
      cb $ Left e
      pure $ pure unit
    -- Kill the internal queue and propagate the exception
    Halt e -> do
      kill e avar
      cb $ Left e
      pure $ pure unit
    -- Push callback onto the head
    PushHead -> Fn.runFn4 _consVar ffiUtil value avar cb
    -- Push callback onto the tail
    PushTail -> Fn.runFn4 _snocVar ffiUtil value avar cb
    -- Drop the head, and push onto the tail
    DropHead e -> do
      mPut <- Fn.runFn2 _tailPuts ffiUtil avar
      canceller <- Fn.runFn4 _snocVar ffiUtil value avar cb
      canceller <$ do
        traverse_ <@> mPut $ \{cb: cb'} -> cb' (Left e)
    -- Drop the tail, and push onto the head
    DropTail e -> do
      mPut <- Fn.runFn2 _initPuts ffiUtil avar
      canceller <- Fn.runFn4 _consVar ffiUtil value avar cb
      canceller <$ do
        traverse_ <@> mPut $ \{cb: cb'} -> cb' (Left e)
    -- Replace the head
    SwapHead e -> do
      mPut <- Fn.runFn2 _tailPuts ffiUtil avar
      canceller <- Fn.runFn4 _consVar ffiUtil value avar cb
      canceller <$ do
        traverse_ <@> mPut $ \{cb: cb'} -> cb' (Left e)
    -- Replace the tail
    SwapTail e -> do
      mPut <- Fn.runFn2 _initPuts ffiUtil avar
      canceller <- Fn.runFn4 _snocVar ffiUtil value avar cb
      canceller <$ do
        traverse_ <@> mPut $ \{cb: cb'} -> cb' (Left e)

-- | Attempts to synchronously fill an AVar. If the AVar is already filled,
-- | this will do nothing. Returns true or false depending on if it succeeded.
tryPut ∷ ∀ a. a → AVar a → Effect Boolean
tryPut value avar = Fn.runFn3 _tryPutVar ffiUtil value avar

-- | Takes the AVar value, leaving it empty. If the AVar is already empty,
-- | the callback will be queued until the AVar is filled. Multiple takes will
-- | resolve in order as the AVar fills. Returns an effect which will remove
-- | the callback from the pending queue.
take ∷ ∀ a. AVar a → AVarCallback a → Effect (Effect Unit)
take avar cb = Fn.runFn3 _takeVar ffiUtil avar cb

-- | Attempts to synchronously take an AVar value, leaving it empty. If the
-- | AVar is empty, this will return `Nothing`.
tryTake ∷ ∀ a. AVar a → Effect (Maybe a)
tryTake avar = Fn.runFn2 _tryTakeVar ffiUtil avar

-- | Reads the AVar value. Unlike `take`, this will not leave the AVar empty.
-- | If the AVar is empty, this will queue until it is filled. Multiple reads
-- | will resolve at the same time, as soon as possible.
read ∷ ∀ a. AVar a → AVarCallback a → Effect (Effect Unit)
read avar cb = Fn.runFn3 _readVar ffiUtil avar cb

-- | Attempts to synchronously read an AVar. If the AVar is empty, this will
-- | return `Nothing`.
tryRead ∷ ∀ a. AVar a → Effect (Maybe a)
tryRead avar = Fn.runFn2 _tryReadVar ffiUtil avar

-- | Synchronously checks the status of an AVar.
status ∷ ∀ a. AVar a → Effect (AVarStatus a)
status avar = Fn.runFn2 _status ffiUtil avar

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

foreign import _newVar ∷ ∀ a. a → Effect (AVar a)
foreign import _killVar ∷ ∀ a. Fn.Fn3 FFIUtil Error (AVar a) (Effect Unit)
foreign import _snocVar ∷ ∀ a. Fn.Fn4 FFIUtil a (AVar a) (AVarCallback Unit) (Effect (Effect Unit))
foreign import _consVar ∷ ∀ a. Fn.Fn4 FFIUtil a (AVar a) (AVarCallback Unit) (Effect (Effect Unit))
foreign import _initPuts ∷ ∀ a. Fn.Fn2 FFIUtil (AVar a) (Effect (Maybe {cb :: AVarCallback a, value :: a}))
foreign import _tailPuts ∷ ∀ a. Fn.Fn2 FFIUtil (AVar a) (Effect (Maybe {cb :: AVarCallback a, value :: a}))
foreign import _tryPutVar ∷ ∀ a. Fn.Fn3 FFIUtil a (AVar a) (Effect Boolean)
foreign import _takeVar ∷ ∀ a. Fn.Fn3 FFIUtil (AVar a) (AVarCallback a) (Effect (Effect Unit))
foreign import _tryTakeVar ∷ ∀ a. Fn.Fn2 FFIUtil (AVar a) (Effect (Maybe a))
foreign import _readVar ∷ ∀ a. Fn.Fn3 FFIUtil (AVar a) (AVarCallback a) (Effect (Effect Unit))
foreign import _tryReadVar ∷ ∀ a. Fn.Fn2 FFIUtil (AVar a) (Effect (Maybe a))
foreign import _status ∷ ∀ a. Fn.Fn2 FFIUtil (AVar a) (Effect (AVarStatus a))
foreign import _pendingPuts :: ∀ a. AVar a -> Int

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
