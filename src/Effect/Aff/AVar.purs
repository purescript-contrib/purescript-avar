module Effect.Aff.AVar
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
import Effect.AVar (AVar, AVarStatus)
import Effect.AVar (AVar, AVarStatus) as Exports
import Effect.Aff (Aff)
import Effect.Aff.AVar.General as G
import Effect.Exception (Error)
import Prelude (Unit)

-- | Creates a fresh AVar with an initial value.
new ∷ ∀ a. a → Aff (AVar a)
new = G.new

-- | Creates a fresh AVar.
empty ∷ ∀ a. Aff (AVar a)
empty = G.empty

-- | Synchronously checks the status of an AVar.
status ∷ ∀ a. AVar a → Aff (AVarStatus a)
status = G.status

-- | Takes the AVar value, leaving it empty. If the AVar is already empty,
-- | the callback will be queued until the AVar is filled. Multiple takes will
-- | resolve in order as the AVar fills.
take ∷ ∀ a. AVar a → Aff a
take = G.take

-- | Attempts to synchronously take an AVar value, leaving it empty. If the
-- | AVar is empty, this will return `Nothing`.
tryTake ∷ ∀ a. AVar a → Aff (Maybe a)
tryTake = G.tryTake

-- | Sets the value of the AVar. If the AVar is already filled, it will be
-- | queued until the value is emptied. Multiple puts will resolve in order as
-- | the AVar becomes available.
put ∷ ∀ a. a → AVar a → Aff Unit
put = G.put

-- | Attempts to synchronously fill an AVar. If the AVar is already filled,
-- | this will do nothing. Returns true or false depending on if it succeeded.
tryPut ∷ ∀ a. a → AVar a → Aff Boolean
tryPut = G.tryPut

-- | Reads the AVar value. Unlike `take`, this will not leave the AVar empty.
-- | If the AVar is empty, this will queue until it is filled. Multiple reads
-- | will resolve at the same time, as soon as possible.
read ∷ ∀ a. AVar a → Aff a
read = G.read

-- | Attempts to synchronously read an AVar. If the AVar is empty, this will
-- | return `Nothing`.
tryRead ∷ ∀ a. AVar a → Aff (Maybe a)
tryRead = G.tryRead

-- | Kills the AVar with an exception. All pending and future actions will
-- | resolve immediately with the provided exception.
kill ∷ ∀ a. Error → AVar a → Aff Unit
kill = G.kill
