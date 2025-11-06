module Backend.DB.Migrations where

import Backend.DB
import Data.Proxy as Proxy
import Database.Beam.AutoMigrate.Types as BT
import Database.Beam.AutoMigrate
import Database.Beam.Postgres
import Database.PostgreSQL.Simple.Beam ()

runMigrations :: Connection -> IO ()
runMigrations conn = do
  tryRunMigrationsWithEditUpdateAndHooks
    preHook
    postHook
    (filterMigrations . whitelistedUnsafeMigrations)
    (fromAnnotatedDbSettings checkedDb (Proxy.Proxy @'[]))
    conn

preHook :: Pg ()
preHook = pure ()

postHook :: () -> Pg ()
postHook = \() -> pure ()

filterMigrations :: [WithPriority Edit] -> [WithPriority Edit]
filterMigrations x = x

-- | NOTE: we dont need to sort at all cuz the function from
-- | beam-automigrate does it for us after the hook
-- | however, out of paranoia we sort anyways, in case they cease to do so
whitelistedUnsafeMigrations :: [WithPriority Edit] -> [WithPriority Edit]
whitelistedUnsafeMigrations edits =
  let (unsafe, safe) = splitEditsOnSafety edits
  in  sortEdits $ safe <> fmap reviewSteps unsafe
  where
    reviewSteps :: WithPriority Edit -> WithPriority Edit
    reviewSteps (WithPriority (edit, priority)) = WithPriority ( fromUnexpectedUnsafe edit, priority )

    fromUnexpectedUnsafe :: Edit -> Edit -- Maybe [Edit]
    fromUnexpectedUnsafe this = this

markSafe :: Edit -> Edit
markSafe e = e { _editCondition = Right Safe }

markSafeWhen :: Edit -> Bool -> Edit
markSafeWhen this_ b = if b then markSafe this_ else markUnsafe this_

markUnsafe :: Edit -> Edit
markUnsafe e = e { _editCondition = Right Unsafe }
