module Effect.Aff.AVar.General
  ( module Exports
  , new
  , empty
  , status
  , take
  , tryTake
  , put
  , tryPut
  , read
  , tryRead
  , kill
  ) where

import Data.Maybe (Maybe)
import Effect.AVar.General (AVar, AVarStatus)
import Effect.AVar.General as AVar
import Effect.AVar.General (AVar, AVarStatus(..), AVarCallback) as Exports
import Effect.Aff.General (Aff, makeAff, effectCanceler)
import Effect.Class (liftEffect)
import Prelude (Unit, bind, pure, (<<<))

-- | Creates a fresh AVar with an initial value.
new ∷ ∀ e a. a → Aff e (AVar e a)
new = liftEffect <<< AVar.new

-- | Creates a fresh AVar.
empty ∷ ∀ e a. Aff e (AVar e a)
empty = liftEffect AVar.empty

-- | Synchronously checks the status of an AVar.
status ∷ ∀ e a. AVar e a → Aff e (AVarStatus e a)
status = liftEffect <<< AVar.status

-- | Takes the AVar value, leaving it empty. If the AVar is already empty,
-- | the callback will be queued until the AVar is filled. Multiple takes will
-- | resolve in order as the AVar fills.
take ∷ ∀ e a. AVar e a → Aff e a
take avar = makeAff \k → do
  c ← AVar.take avar k
  pure (effectCanceler c)

-- | Attempts to synchronously take an AVar value, leaving it empty. If the
-- | AVar is empty, this will return `Nothing`.
tryTake ∷ ∀ e a. AVar e a → Aff e (Maybe a)
tryTake = liftEffect <<< AVar.tryTake

-- | Sets the value of the AVar. If the AVar is already filled, it will be
-- | queued until the value is emptied. Multiple puts will resolve in order as
-- | the AVar becomes available.
put ∷ ∀ e a. a → AVar e a → Aff e Unit
put value avar = makeAff \k → do
  c ← AVar.put value avar k
  pure (effectCanceler c)

-- | Attempts to synchronously fill an AVar. If the AVar is already filled,
-- | this will do nothing. Returns true or false depending on if it succeeded.
tryPut ∷ ∀ e a. a → AVar e a → Aff e Boolean
tryPut value = liftEffect <<< AVar.tryPut value

-- | Reads the AVar value. Unlike `take`, this will not leave the AVar empty.
-- | If the AVar is empty, this will queue until it is filled. Multiple reads
-- | will resolve at the same time, as soon as possible.
read ∷ ∀ e a. AVar e a → Aff e a
read avar = makeAff \k → do
  c ← AVar.read avar k
  pure (effectCanceler c)

-- | Attempts to synchronously read an AVar. If the AVar is empty, this will
-- | return `Nothing`.
tryRead ∷ ∀ e a. AVar e a → Aff e (Maybe a)
tryRead = liftEffect <<< AVar.tryRead

-- | Kills the AVar with an exception. All pending and future actions will
-- | resolve immediately with the provided exception.
kill ∷ ∀ e a. e → AVar e a → Aff e Unit
kill error = liftEffect <<< AVar.kill error
