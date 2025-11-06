{-# Language ScopedTypeVariables #-}


module RhyConcurrent where

import Backend.Config
import Backend.Types
--import Backend.Utils.Email

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.Async (waitCatch, withAsync)
import Control.Exception (SomeException, try)
import Control.Monad ((<=<), forever, void, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Either (isLeft)

-- | Perform a supervised operation with delays in between, on a separate thread. Return an IO action for killing the thread.
worker :: (MonadIO m)
       => ConfigEnv
       -> Int -- ^ Delay between operations, in microseconds
       -> IO a -- ^ Operation to perform
       -> m (IO ())
worker = taggedWorker ""

-- | Perform a tagged, supervised operation with delays in between, on a separate thread. Return an IO action for killing the thread.
taggedWorker
  :: MonadIO m
  => String -- ^ Tag for this worker: displayed in error messages
  -> ConfigEnv
  -> Int
  -> IO a
  -> m (IO ())
taggedWorker tag emailConfig' delay x = return . killThread <=< liftIO . forkIO . supervise tag emailConfig' . void . forever $
  x >> threadDelay delay


-- | Runs an action forever, restarting it if it dies.
supervise :: Show a => String -> ConfigEnv -> IO a -> IO ()
supervise tag _emailConfig' a = forever $ withAsync a $ \child -> do
  let msgPrefix = if null tag then "supervise: " else "supervise: " <> tag <> ": "
  print $ "startup " <> tag
  result <- waitCatch child

  when (isLeft result) $ do

    ---emailGalenX emailConfig' ( LT.fromStrict . T.pack . show $ result )
    threadDelay (10 * 60 * microsecondsInSecond)
  printResult :: Either SomeException () <- try $ putStrLn $ msgPrefix <> "child terminated with " <> show result <> "; restarting"
  threadDelay 1000000
  when (isLeft printResult) $ do
    putStrLn $ msgPrefix <> "note: an exception was encountered when printing the previous result"

    -- emailAdminX emailConfig' $
    --   (LT.fromStrict . T.pack . show $ printResult)

-- emailAdminX :: MonadIO m => EmailConfig -> LT.Text -> m ()
-- emailAdminX cfg msg = do
--   let
--       to = Address
--         { addressName = Just ("Fart Face" :: T.Text)
--         , addressEmail = "galen@aceinterviewprep.io"
--         } -- recipients Address
--       mail = simpleMail' to emailSendFrom ((LT.toStrict msg) <> "...") msg
--   void $ sendEmailIfNotLocal cfg mail
--   where
--     emailSend = EmailSendConfig "Ace Interview Prep" "galen@aceinterviewprep.io"
--     emailSendFrom = Address
--       { addressName = Just $ _emailSendConfig_name emailSend
--       , addressEmail = _emailSendConfig_address emailSend
--       }


-- -- | Runs an action forever, restarting it if it dies.
-- supervise' :: Show a => String -> EmailConfig -> IO a -> IO ()
-- supervise' tag emailConfig' a = forever $ withAsync a $ \child -> do
--   let msgPrefix = if null tag then "supervise: " else "supervise: " <> tag <> ": "
--   result <- waitCatch a
--   when (isLeft result) $ do
--     emailGalen emailConfig' ( LT.fromStrict . T.pack . show $ result )
--   printResult :: Either SomeException () <- try $ putStrLn $ msgPrefix <> "child terminated with " <> show result <> "; restarting"
--   threadDelay 1000000
--   when (isLeft printResult) $ do
--     putStrLn $ msgPrefix <> "note: an exception was encountered when printing the previous result"
--     emailGalen emailConfig' $
--       (LT.fromStrict . T.pack . show $ printResult)
