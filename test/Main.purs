module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.AVar (AVAR, killVar, makeEmptyVar, makeVar, putVar, readVar, takeVar, tryPutVar, tryReadVar, tryTakeVar)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (error, message)
import Control.Monad.Eff.Ref (REF, modifyRef, newRef, readRef)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Test.Assert (ASSERT, assert')

type TestEff = Eff (avar ∷ AVAR, assert ∷ ASSERT, console ∷ CONSOLE, ref ∷ REF)

test ∷ String → TestEff Boolean → TestEff Unit
test s k = k >>= \r → assert' s r *> log ("[OK] " <> s)

test_tryRead_full ∷ TestEff Unit
test_tryRead_full = test "tryRead/full" do
  var ← makeVar "foo"
  val1 ← tryReadVar var
  val2 ← tryReadVar var
  pure (val1 == Just "foo" && val2 == Just "foo")

test_tryRead_empty ∷ TestEff Unit
test_tryRead_empty = test "tryRead/empty" do
  var ← makeEmptyVar
  val1 ∷ Maybe Unit ← tryReadVar var
  pure (val1 == Nothing)

test_tryPut_full ∷ TestEff Unit
test_tryPut_full = test "tryPut/full" do
  var ← makeVar "foo"
  res ← tryPutVar var "bar"
  pure (not res)

test_tryPut_empty ∷ TestEff Unit
test_tryPut_empty = test "tryPut/empty" do
  var ← makeEmptyVar
  res ← tryPutVar var "foo"
  val ← tryReadVar var
  pure (res && val == Just "foo")

test_tryTake_full ∷ TestEff Unit
test_tryTake_full = test "tryTake/full" do
  var ← makeVar "foo"
  res1 ← tryTakeVar var
  res2 ← tryTakeVar var
  pure (res1 == Just "foo" && res2 == Nothing)

test_tryTake_empty ∷ TestEff Unit
test_tryTake_empty = test "tryTake/empty" do
  var  ← makeEmptyVar
  res1 ← tryTakeVar var
  res2 ← tryPutVar var "foo"
  res3 ← tryTakeVar var
  pure (res1 == Nothing && res2 && res3 == Just "foo")

test_put_take ∷ TestEff Unit
test_put_take = test "put/take" do
  ref ← newRef ""
  var ← makeEmptyVar
  putVar var "foo" $ traverse_ \_ →
    modifyRef ref (_ <> "bar")
  takeVar var $ traverse_ \val →
    modifyRef ref (_ <> val)
  eq "barfoo" <$> readRef ref

test_put_read_take ∷ TestEff Unit
test_put_read_take = test "put/read/take" do
  ref ← newRef ""
  var ← makeEmptyVar
  putVar var "foo" $ traverse_ \_ →
    modifyRef ref (_ <> "bar")
  readVar var $ traverse_ \val →
    modifyRef ref (_ <> val <> "baz")
  takeVar var $ traverse_ \val →
    modifyRef ref (_ <> val)
  eq "foobazfoobar" <$> readRef ref

test_take_put ∷ TestEff Unit
test_take_put = test "take/put" do
  ref ← newRef ""
  var ← makeEmptyVar
  takeVar var $ traverse_ \val →
    modifyRef ref (_ <> val)
  putVar var "foo" $ traverse_ \_ →
    modifyRef ref (_ <> "bar")
  eq "foobar" <$> readRef ref

test_take_read_put ∷ TestEff Unit
test_take_read_put = test "take/read/put" do
  ref ← newRef ""
  var ← makeEmptyVar
  takeVar var $ traverse_ \val →
    modifyRef ref (_ <> val)
  readVar var $ traverse_ \val →
    modifyRef ref (_ <> val <> "baz")
  putVar var "foo" $ traverse_ \_ →
    modifyRef ref (_ <> "bar")
  eq "foobazfoobar" <$> readRef ref

test_read_put_take ∷ TestEff Unit
test_read_put_take = test "read/put/take" do
  ref ← newRef ""
  var ← makeEmptyVar
  readVar var $ traverse_ \val →
    modifyRef ref (_ <> val <> "baz")
  putVar var "foo" $ traverse_ \_ →
    modifyRef ref (_ <> "bar")
  takeVar var $ traverse_ \val → do
    modifyRef ref (_ <> val)
  eq "foobazbarfoo" <$> readRef ref

test_read_take_put ∷ TestEff Unit
test_read_take_put = test "read/take/put" do
  ref ← newRef ""
  var ← makeEmptyVar
  readVar var $ traverse_ \val → do
    modifyRef ref (_ <> val <> "baz")
    takeVar var $ traverse_ \val' →
      modifyRef ref (_ <> val')
  putVar var "foo" $ traverse_ \_ →
    modifyRef ref (_ <> "bar")
  eq "foobazbarfoo" <$> readRef ref

test_kill_full ∷ TestEff Unit
test_kill_full = test "kill/full" do
  ref ← newRef ""
  var ← makeEmptyVar
  putVar var "foo" $ traverse_ \_ →
    modifyRef ref (_ <> "bar")
  killVar var (error "Die.")
  readVar var case _ of
    Left err → modifyRef ref (_ <> message err)
    Right _  → modifyRef ref (_ <> "BAD")
  eq "barDie." <$> readRef ref

test_kill_empty ∷ TestEff Unit
test_kill_empty = test "kill/empty" do
  ref ← newRef ""
  var ← makeEmptyVar
  killVar var (error "Die.")
  readVar var case _ of
    Left err → modifyRef ref (_ <> message err)
    Right _  → modifyRef ref (_ <> "BAD")
  eq "Die." <$> readRef ref

test_kill_pending ∷ TestEff Unit
test_kill_pending = test "kill/pending" do
  ref ← newRef ""
  var ← makeEmptyVar
  let
    cb s = case _ of
      Left err → modifyRef ref (_ <> s <> message err)
      Right _  → modifyRef ref (_ <> "BAD")
  takeVar var (cb "a")
  takeVar var (cb "b")
  readVar var (cb "c")
  readVar var (cb "d")
  killVar var (error "-die.")
  eq "c-die.d-die.a-die.b-die." <$> readRef ref

main ∷ TestEff Unit
main = do
  test_tryRead_full
  test_tryRead_empty
  test_tryPut_full
  test_tryPut_empty
  test_tryTake_full
  test_tryTake_empty
  test_put_take
  test_take_put
  test_take_read_put
  test_read_put_take
  test_read_take_put
  test_kill_full
  test_kill_empty
  test_kill_pending
