{-# LANGUAGE MultiParamTypeClasses, DeriveAnyClass #-}
module MimirInternal where

import Data.Convertible
import Database.HDBC
import Database.HDBC.PostgreSQL
import qualified Database.HDBC.Sqlite3 as Lite

import Zepto.Types.Export

data MimirConn = Post Connection
               | SLite Lite.Connection
instance IConnection MimirConn where
  disconnect (Post  c) = disconnect c
  disconnect (SLite c) = disconnect c
  commit (Post  c) = commit c
  commit (SLite c) = commit c
  rollback (Post  c) = rollback c
  rollback (SLite c) = rollback c
  runRaw (Post  c) = runRaw c
  runRaw (SLite c) = runRaw c
  run (Post  c) = run c
  run (SLite c) = run c
  prepare (Post  c) = prepare c
  prepare (SLite c) = prepare c
  clone (Post  c) = clone c >>= return . Post
  clone (SLite c) = clone c >>= return . SLite
  hdbcDriverName (Post  c) = hdbcDriverName c
  hdbcDriverName (SLite c) = hdbcDriverName c
  hdbcClientVer (Post  c) = hdbcClientVer c
  hdbcClientVer (SLite c) = hdbcClientVer c
  proxiedClientName (Post  c) = proxiedClientName c
  proxiedClientName (SLite c) = proxiedClientName c
  proxiedClientVer (Post  c) = proxiedClientVer c
  proxiedClientVer (SLite c) = proxiedClientVer c
  dbServerVer (Post  c) = dbServerVer c
  dbServerVer (SLite c) = dbServerVer c
  dbTransactionSupport (Post  c) = dbTransactionSupport c
  dbTransactionSupport (SLite c) = dbTransactionSupport c
  getTables (Post  c) = getTables c
  getTables (SLite c) = getTables c
  describeTable (Post  c) = describeTable c
  describeTable (SLite c) = describeTable c

instance Convertible SqlValue LispVal where
  safeConvert (SqlString s) = return $ fromSimple $ String s
  safeConvert (SqlByteString b) = return $ ByteVector b
  safeConvert (SqlWord32 w) = return $ fromSimple $ Number $ NumS $ fromIntegral w
  safeConvert (SqlWord64 w) = return $ fromSimple $ Number $ NumS $ fromIntegral w
  safeConvert (SqlInt32 i) = return $ fromSimple $ Number $ NumS $ fromIntegral i
  safeConvert (SqlInt64 i) = return $ fromSimple $ Number $ NumS $ fromIntegral i
  safeConvert (SqlInteger i) = return $ fromSimple $ Number $ NumI i
  safeConvert (SqlChar c) = return $ fromSimple $ Character c
  safeConvert (SqlBool b) = return $ fromSimple $ Bool b
  safeConvert (SqlDouble d) = return $ fromSimple $ Number $ NumF d
  safeConvert (SqlRational r) = return $ fromSimple $ Number $ NumR r
  safeConvert (SqlLocalDate d) = return $ fromSimple $ String $ show d
  safeConvert (SqlLocalTimeOfDay d) = return $ fromSimple $ String $ show d
  safeConvert (SqlZonedLocalTimeOfDay d z) =
    return $ fromSimple $ String $ show d ++ ' ' : show z
  safeConvert (SqlLocalTime d) = return $ fromSimple $ String $ show d
  safeConvert (SqlZonedTime d) = return $ fromSimple $ String $ show d
  safeConvert (SqlUTCTime d) = return $ fromSimple $ String $ show d
  safeConvert (SqlDiffTime d) = return $ fromSimple $ String $ show d
  safeConvert (SqlPOSIXTime d) = return $ fromSimple $ String $ show d
  safeConvert (SqlEpochTime d) = return $ fromSimple $ String $ show d
  safeConvert (SqlTimeDiff d) = return $ fromSimple $ String $ show d
  safeConvert SqlNull = return $ fromSimple $ Nil ""

instance Convertible LispVal SqlValue where
  safeConvert (SimpleVal (String s)) = return $ toSql s
  safeConvert (SimpleVal (Number (NumI i))) = return $ toSql i
  safeConvert (SimpleVal (Number (NumF f))) = return $ toSql f
  safeConvert (SimpleVal (Number (NumR r))) = return $ toSql r
  safeConvert (SimpleVal (Nil _)) = return $ SqlNull

namespace :: String -> String
namespace fun = "mimir:" ++ fun

con :: [[Char]] -> [Char]
con [last] = last
con (x:y) = x ++ ('\n' : con y)

convertResultSet :: [SqlValue] -> LispVal
convertResultSet set = List $ map convert set

convertZeptoList :: [LispVal] -> [SqlValue]
convertZeptoList = map convert

exports :: [(String, [LispVal] -> IOThrowsError LispVal, String)]
exports = [(namespace "connect", unaryIOOp connectFun, con connectDoc),
           (namespace "disconnect", unaryIOOp disconnectFun, con disconnectDoc),
           (namespace "get-tables", unaryIOOp getTablesFun, con getTablesDoc),
           (namespace "execute", executeFun, con executeDoc),
           (namespace "commit", unaryIOOp commitFun, con commitDoc),
           (namespace "rollback", unaryIOOp rollbackFun, con rollbackDoc),
           (namespace "connection?", unaryIOOp connFun, con connDoc),
           (namespace "connection-info", unaryIOOp connInfoFun, con connInfoDoc)]


connectDoc :: [String]
connectDoc = ["connect to a database. Takes a hashmap <par>conf</par> as",
              "configuration.",
              "",
              "params:",
              "- args: the configuration object",
              "complexity: O(n) where n is the complexity of the configuration",
              "returns: a connection object (opaque)"]


connectPost info = do
    conn <- liftLisp $ connectPostgreSQL $ parseInfo info
    return $ toOpaque $ Post conn
  where parseInfo conninfo = tail (foldrWithKeyMap build "" conninfo)
        build (String k) (SimpleVal (String v)) acc = acc ++ ' ' : k ++ '=' : v
        build _ _ acc = acc

{-connectMysql c = return $ toOpaque $ connectMySQL $ parseInfo
  where parseInfo = MySQLConnectInfo (f "host" "127.0.0.1")
                                     (f "user" "zepto")
                                     (f "password" "")
                                     (f "dbname" "zepto")
                                     (f "port" 3306)
                                     (f "group" Nothing)
        f k d     = convert $ findWithDefault k d c
        convert (SimpleVal (String x)) = x
        convert (SimpleVal (Number (NumI x))) = x
        convert _ = error "Invalid type in connection info"-}

connectSqlite3 info = do
    conn <- liftLisp $ Lite.connectSqlite3 $ parseInfo info
    return $ toOpaque $ SLite conn
  where parseInfo = unp .findWithDefaultMap (SimpleVal (String "/")) (String "path")
        unp (SimpleVal (String x)) = x

connectFun :: LispVal -> IOThrowsError LispVal
connectFun (HashMap conninfo) =
    case lookupMap (String "driver") conninfo of
      Just value ->
        let info = deleteMap (String "driver") conninfo
        in case value of
          --SimpleVal (String "mysql") -> connectMysql info
          SimpleVal (String "postgresql") -> connectPost info
          SimpleVal (String "sqlite3") -> connectSqlite3 info
          val -> lispErr $ Default $ "Unknown driver/invalid value: " ++ show val
      Nothing -> lispErr $ Default "Missing driver value in connection info"
connectFun x = lispErr $ TypeMismatch "string" x

disconnectDoc :: [String]
disconnectDoc = ["destroys a database connection. Takes a connection <par>conn</par>.",
                  "",
                  "params:",
                  "- args: the connection object",
                  "complexity: O(1)",
                  "returns: nil"]

disconnectFun :: LispVal -> IOThrowsError LispVal
disconnectFun c@(Opaque _) = do
  case ((fromOpaque c) :: Maybe MimirConn) of
    Just conn -> do
      _      <- liftLisp $ disconnect conn
      return $ fromSimple $ Nil ""
    Nothing -> lispErr $ TypeMismatch "opaque connection" c
disconnectFun x = lispErr $ TypeMismatch "opaque" x

getTablesDoc :: [String]
getTablesDoc = ["gets a list of table names (strings). Takes a connection",
                "<par>conn</par>.",
                "",
                "params:",
                "- args: the connection object",
                "complexity: O(n)",
                "returns: a list of strings"]

getTablesFun :: LispVal -> IOThrowsError LispVal
getTablesFun c@(Opaque _) = do
  case ((fromOpaque c) :: Maybe MimirConn) of
    Just conn -> do
      tables <- liftLisp $ getTables conn
      return $ List $ map (fromSimple . String) tables
    Nothing -> lispErr $ TypeMismatch "opaque connection" c
getTablesFun x = lispErr $ TypeMismatch "opaque" x

executeDoc :: [String]
executeDoc = ["execute a statement <par>stmt</par> (string). Takes a connection,",
              "a statement and an optional list of values <par>args/par> that should be",
              "interpolated into the statement.",
              "",
              "Auto-commits.",
              "",
              "params:",
              "- conn: the connection",
              "- stmt: the statement to execute",
              "- args: the values to interpolate (will be interpolated in place of ?, optional)",
              "complexity: O(n)",
              "returns: a list of rows"]

executeFun :: [LispVal] -> IOThrowsError LispVal
executeFun [c@(Opaque _), (SimpleVal (String s))] = do
  case ((fromOpaque c) :: Maybe MimirConn) of
    Just conn -> do
      stmt   <- liftLisp $ prepare conn s
      exec   <- liftLisp $ executeRaw stmt
      _      <- liftLisp $ commit conn
      res    <- liftLisp $ fetchAllRows' stmt
      return $ List $ map convertResultSet res
    Nothing -> lispErr $ TypeMismatch "opaque connection" c
executeFun [c@(Opaque _), (SimpleVal (String s)), List args] = do
  case ((fromOpaque c) :: Maybe MimirConn) of
    Just conn -> do
      stmt   <- liftLisp $ prepare conn s
      exec   <- liftLisp $ execute stmt $ convertZeptoList args
      _      <- liftLisp $ commit conn
      res    <- liftLisp $ fetchAllRows' stmt
      return $ List $ map convertResultSet res
    Nothing -> lispErr $ TypeMismatch "opaque connection" c
executeFun [x, (SimpleVal (String _))] =
  lispErr $ TypeMismatch "opaque" x
executeFun [_, x] =
  lispErr $ TypeMismatch "string" x
executeFun [x, (SimpleVal (String _)), (List _)] =
  lispErr $ TypeMismatch "opaque" x
executeFun [_, x, (List _)] =
  lispErr $ TypeMismatch "string" x
executeFun [_, _, x] =
  lispErr $ TypeMismatch "list" x
executeFun x =
  lispErr $ NumArgs 2 x

commitDoc :: [String]
commitDoc = ["commits all pending transactions to the DB to ensure writes.",
             "Must be called for transactions to persist.",
             "",
             "params:",
             "- args: the connection",
             "complexity: O(1)",
             "returns: nil"]

commitFun :: LispVal -> IOThrowsError LispVal
commitFun c@(Opaque _) =
  case ((fromOpaque c) :: Maybe MimirConn) of
    Just conn -> do
      _      <- liftLisp $ commit conn
      return $ fromSimple $ Nil ""
    Nothing -> lispErr $ TypeMismatch "opaque connection" c
commitFun x = lispErr $ TypeMismatch "opaque" x

rollbackDoc :: [String]
rollbackDoc = ["rollbacks all pending transactions to the DB to ensure failure recovery.",
             "",
             "params:",
             "- args: the connection",
             "complexity: O(1)",
             "returns: nil"]

rollbackFun :: LispVal -> IOThrowsError LispVal
rollbackFun c@(Opaque _) =
  case ((fromOpaque c) :: Maybe MimirConn) of
    Just conn -> do
      _      <- liftLisp $ rollback conn
      return $ fromSimple $ Nil ""
    Nothing -> lispErr $ TypeMismatch "opaque connection" c
rollbackFun x = lispErr $ TypeMismatch "opaque" x

connDoc :: [String]
connDoc = ["checks whether <par>args</par> is a database connection.",
           "",
           "params:",
           "- args: the db connection",
           "complexity: O(1)",
           "returns: a boolean"]

connFun :: LispVal -> IOThrowsError LispVal
connFun c@(Opaque _) =
  case ((fromOpaque c) :: Maybe MimirConn) of
    Just _ -> return $ fromSimple $ Bool $ True
    Nothing -> return $ fromSimple $ Bool $ False
connFun _ = return $ fromSimple $ Bool $ False

connInfoDoc :: [String]
connInfoDoc = ["gets the connection information for <par>args</par> as a hashmap.",
               "",
               "params:",
               "- args: the connection to check",
               "complexity: O(1)",
               "returns: a hashmap with the fields :driver, :client-version, :server-version, and :transaction-support"]

connInfoFun :: LispVal -> IOThrowsError LispVal
connInfoFun c@(Opaque _) =
  case ((fromOpaque c) :: Maybe MimirConn) of
    Just conn -> do
      return $ HashMap $ fromListMap
        [(Atom ":driver", fromSimple $ String $ hdbcDriverName conn),
         (Atom ":client-version", fromSimple $ String $ hdbcClientVer conn),
         (Atom ":transaction-support", fromSimple $ Bool $ dbTransactionSupport conn),
         (Atom ":server-version", fromSimple $ String $ dbServerVer conn)]
    Nothing -> lispErr $ TypeMismatch "opaque connection" c
connInfoFun x = lispErr $ TypeMismatch "opaque" x
