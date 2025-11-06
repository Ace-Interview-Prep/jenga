{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Backend.DB where

-- we need the -fno-warn-orphans so that we can keep certain types in Common but use them in restricted database modules


-- import Snap (Snap, MonadSnap, liftSnap, getCookie, cookieValue)
import Jenga.Backend.DB.Instances ()
import Common.Schema

--import qualified Database.Beam.AutoMigrate as AutoM
import Database.Beam.Schema.Tables
import Database.PostgreSQL.Simple.Beam ()
import Database.Beam.AutoMigrate hiding (zipTables)
import Database.Beam.Postgres

import Control.Lens hiding ((<.))
import Data.Proxy
import Data.Foldable
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as T




db :: DatabaseSettings Postgres Db
db = deAnnotateDatabase checkedDb


checkedDb :: AnnotatedDatabaseSettings Postgres Db
checkedDb = defaultAnnotatedDbSettings (useObeliskNamingConvention defaultDbSettings)
  -- FOR Customization, uncomment:
  -- `withDbModification` dbModification
  --    { _db_task = annotateTableFields tableModification
  --      { _task_taskId = defaultsTo $ val_ 1
  --      }
  --    }

-- -- | Untyped queries may be necessary for migrating from Table version to another
-- -- | so this function exists mainly for the `preHook` function used for runAceMigration
-- exec_ :: String -> Pg ()
-- exec_ = void . execute_ . Query . T.encodeUtf8 . T.pack

-- runDb :: MonadIO m => Pool Connection -> Pg a -> m a
-- runDb pool a = liftIO $ withResource pool $ \conn ->
--   withTransactionSerializable conn $ runBeamPostgres conn a

-- runDbTrace :: MonadIO m => Pool Connection -> Pg a -> m a
-- runDbTrace pool a = liftIO $ withResource pool $ \conn ->
--   withTransactionSerializable conn $ runBeamPostgresDebug putStrLn conn a

-- runDbQuiet :: MonadIO m => Pool Connection -> Pg a -> m a
-- runDbQuiet pool a = liftIO $ withResource pool $ \conn ->
--   withTransactionSerializable conn $ runBeamPostgres conn a

-- -- | Run a serializable transaction and embed it in a larger context
-- runSerializable :: MonadIO m => Pool Connection -> Pg a -> m a
-- runSerializable connPool a = do
--   let logger = putStrLn
--   liftIO $ withResource connPool $ \dbConn -> liftIO $ withTransactionSerializable dbConn $
--     runBeamPostgresDebug logger dbConn a

-- instance FromField (SqlSerial Int64) where
--   fromField f mbs = fmap SqlSerial $ fromField f mbs

-- instance FromField (PrimaryKey Account Identity) where
--   fromField f mbs = AccountId <$> fromField f mbs

-- instance ToField (PrimaryKey Account Identity) where
--   toField (AccountId x) = toField x

-- instance ToField a => ToField (SqlSerial a) where
--   toField (SqlSerial a) = toField a

-- instance HasColumnType Mail where
--   defaultColumnType _ = defaultColumnType $ Proxy @(PgJSON Mail)

-- instance HasSqlValueSyntax PgValueSyntax Mail where
--   sqlValueSyntax mail = sqlValueSyntax $ PgJSON mail

-- instance FromBackendRow Postgres Mail

-- instance FromField Mail where
--   fromField f mbs = (\(PgJSON mail) -> mail) <$> fromField f mbs

-- -- | Assumes that field names are compatible with obelisk's field naming scheme
-- -- and creates significantly shorter names than the default would produce.
useObeliskNamingConvention
  :: forall be db
  . Database be db
  => DatabaseSettings be db
  -> DatabaseSettings be db
useObeliskNamingConvention d =
  runIdentity $ zipTables (Proxy @be) (\_ -> pure . f) d d
  where
    f :: forall e. IsDatabaseEntity be e => DatabaseEntity be db e -> DatabaseEntity be db e
    f (DatabaseEntity descriptor) =
      let
        renameEntity e = (dbEntityName %~ trimFieldName) e
        renameFields = withFieldRenamer (renamingFields $ fold . (NonEmpty.intersperse (T.pack "_")) . fmap trimFieldName)
      in
        DatabaseEntity (renameFields $ renameEntity descriptor)
    trimFieldName :: T.Text -> T.Text
    trimFieldName = T.drop 1 . T.dropWhile (/= '_') . T.drop 1

-- newtype ParserColumn a = ParserColumn { unParserColumn :: a }

-- instance HasSqlValueSyntax PgValueSyntax [(SqlSerial Int64, UTCTime)] where
--   sqlValueSyntax = sqlValueSyntax . PgJSON
-- instance HasSqlValueSyntax PgValueSyntax (Map.Map T.Text [Id Account]) where
--   sqlValueSyntax = sqlValueSyntax . PgJSON
-- instance HasSqlValueSyntax PgValueSyntax (Map.Map T.Text [T.Text]) where
--   sqlValueSyntax = sqlValueSyntax . PgJSON

-- instance FromBackendRow Postgres [(SqlSerial Int64, UTCTime)]
-- instance FromBackendRow Postgres (Map.Map T.Text [Id Account])
-- instance FromBackendRow Postgres (Map.Map T.Text [T.Text])

-- instance FromBackendRow Postgres [AcctID]
-- instance FromField [(SqlSerial Int64, UTCTime)] where
--   fromField f mbs = (\(PgJSON mail) -> mail) <$> fromField f mbs
-- instance FromField [AcctID] where
--   fromField f mbs = (\(PgJSON mail) -> mail) <$> fromField f mbs
-- instance FromField (Map.Map T.Text [Id Account]) where
--   fromField f mbs = (\(PgJSON x) -> x) <$> fromField f mbs
-- instance FromField (Map.Map T.Text [T.Text]) where
--   fromField f mbs = (\(PgJSON x) -> x) <$> fromField f mbs
-- instance FromField LogItem where
--   fromField f mbs = (\(PgJSON item) -> item) <$> fromField f mbs

-- instance HasColumnType (Map.Map T.Text [Id Account]) where
--   defaultColumnType _ = defaultColumnType $ Proxy @(PgJSON (Map.Map T.Text [Int]))
-- instance HasColumnType (Map.Map T.Text [T.Text]) where
--   defaultColumnType _ = defaultColumnType $ Proxy @(PgJSON (Map.Map T.Text [T.Text]))
-- instance HasColumnType Float where
--   defaultColumnType _ = defaultColumnType (Proxy @(PgJSON Float))
-- instance HasColumnType [(SqlSerial Int64, UTCTime)] where
--   defaultColumnType _ = defaultColumnType (Proxy @(PgJSON Float))
-- instance HasColumnType [Char] where
--   defaultColumnType _ = SqlStdType $ varCharType Nothing Nothing
-- instance HasColumnType [SqlSerial Int64] where
--   defaultColumnType _ = SqlStdType intType
-- instance HasColumnType [AcctID] where
--   defaultColumnType _ = defaultColumnType (Proxy @(PgJSON Float))
-- instance HasColumnType LogItem where
--   defaultColumnType _ = defaultColumnType $ Proxy @(PgJSON (LogItem))

-- -- reportLog :: MonadIO m => Bool -> LogItem -> Pg ()
-- -- reportLog isUrgent item = do
-- --   runInsert $ insert (_db_reporting db) $ insertExpressions
-- --     [ LogItemRow
-- --       default_
-- --       (val_ False)
-- --       current_timestamp_
-- --       (val_ isUrgent)
-- --       (val_ item)
-- --     ]


-- fromAcctID :: AcctID -> PrimaryKey Account Identity
-- fromAcctID (AcctID i) = AccountId . SqlSerial . fromIntegral $ i

-- toAcctID :: PrimaryKey Account Identity -> AcctID
-- toAcctID (AccountId (SqlSerial i)) = AcctID $ fromIntegral i

-- getAcctID' :: Account Identity -> AcctID
-- getAcctID' = toAcctID . primaryKey

-- data DBExistsError = DBExistsError deriving Show

-- data EmailMsg = EmailMsg
--   { _emailMsg_name :: T.Text
--   , _emailMsg_id :: SqlSerial Int64
--   , _emailMsg_sender :: Id Account
--   , _emailMsg_contents :: T.Text
--   } deriving Eq

-- toEmailMsg :: (T.Text, SqlSerial Int64, T.Text, Id Account) -> EmailMsg
-- toEmailMsg (msgText, mId, uname, sender) = EmailMsg uname mId sender msgText
