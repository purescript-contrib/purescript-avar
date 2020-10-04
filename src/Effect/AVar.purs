module Effect.AVar
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

import Prelude
import Data.Either (Either)
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.AVar.General as G
import Effect.Exception (Error)

type AVarCallback a = (Either Error a → Effect Unit)

type AVar a = G.AVar Error a

type AVarStatus a = G.AVarStatus Error a

-- | Creates a new empty AVar.
empty ∷ ∀ a. Effect (AVar a)
empty = G.empty

-- | Creates a fresh AVar with an initial value.
new ∷ ∀ a. a → Effect (AVar a)
new = G.new

-- | Kills the AVar with an exception. All pending and future actions will
-- | resolve immediately with the provided exception.
kill ∷ ∀ a. Error → AVar a → Effect Unit
kill = G.kill

-- | Sets the value of the AVar. If the AVar is already filled, it will be
-- | queued until the value is emptied. Multiple puts will resolve in order as
-- | the AVar becomes available. Returns an effect which will remove the
-- | callback from the pending queue.
put ∷ ∀ a. a → AVar a → AVarCallback Unit → Effect (Effect Unit)
put = G.put

-- | Attempts to synchronously fill an AVar. If the AVar is already filled,
-- | this will do nothing. Returns true or false depending on if it succeeded.
tryPut ∷ ∀ a. a → AVar a → Effect Boolean
tryPut = G.tryPut

-- | Takes the AVar value, leaving it empty. If the AVar is already empty,
-- | the callback will be queued until the AVar is filled. Multiple takes will
-- | resolve in order as the AVar fills. Returns an effect which will remove
-- | the callback from the pending queue.
take ∷ ∀ a. AVar a → AVarCallback a → Effect (Effect Unit)
take = G.take

-- | Attempts to synchronously take an AVar value, leaving it empty. If the
-- | AVar is empty, this will return `Nothing`.
tryTake ∷ ∀ a. AVar a → Effect (Maybe a)
tryTake = G.tryTake

-- | Reads the AVar value. Unlike `take`, this will not leave the AVar empty.
-- | If the AVar is empty, this will queue until it is filled. Multiple reads
-- | will resolve at the same time, as soon as possible.
read ∷ ∀ a. AVar a → AVarCallback a → Effect (Effect Unit)
read = G.read

-- | Attempts to synchronously read an AVar. If the AVar is empty, this will
-- | return `Nothing`.
tryRead ∷ ∀ a. AVar a → Effect (Maybe a)
tryRead = G.tryRead

-- | Synchronously checks the status of an AVar.
status ∷ ∀ a. AVar a → Effect (AVarStatus a)
status = G.status

isEmpty ∷ ∀ a. AVarStatus a → Boolean
isEmpty = G.isEmpty

isFilled ∷ ∀ a. AVarStatus a → Boolean
isFilled = G.isFilled

isKilled ∷ ∀ a. AVarStatus a → Boolean
isKilled = G.isKilled
