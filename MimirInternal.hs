{-# LANGUAGE MultiParamTypeClasses #-}
module MimirInternal where

import Data.Convertible
import Database.HDBC
import Database.HDBC.PostgreSQL

import Zepto.Types

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

parseInfo :: LispVal -> String
parseInfo (HashMap conninfo) = tail (foldrWithKey build "" conninfo)
    where build (String k) (SimpleVal (String v)) acc = acc ++ ' ' : k ++ '=' : v
          build _ _ acc = acc

connectDoc :: [String]
connectDoc = ["connect to a database. Takes a hashmap <par>conf</par> as",
              "configuration.",
              "",
              "params:",
              "- args: the configuration object",
              "complexity: O(n) where n is the complexity of the configuration",
              "returns: a connection object (opaque)"]

connectFun :: LispVal -> IOThrowsError LispVal
connectFun conninfo@(HashMap _) =
    return $ toOpaque $ connectPostgreSQL $ parseInfo conninfo
connectFun x = lispErr $ TypeMismatch "string" x

disconnectDoc :: [String]
disconnectDoc = ["destroys a database connection. Takes a connection <par>conn</par>.",
                  "",
                  "params:",
                  "- args: the connection object",
                  "complexity: O(1)",
                  "returns: nil"]

disconnectFun :: LispVal -> IOThrowsError LispVal
disconnectFun conn@(Opaque _) = do
  case ((fromOpaque conn) :: Maybe (IO Connection)) of
    Just x -> do
      x <- liftLisp x
      y <- liftLisp $ disconnect x
      return $ fromSimple $ Nil ""
    Nothing -> lispErr $ TypeMismatch "opaque connection" conn
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
getTablesFun conn@(Opaque _) = do
  case ((fromOpaque conn) :: Maybe (IO Connection)) of
    Just x -> do
      x <- liftLisp x
      y <- liftLisp $ getTables x
      return $ List $ map (fromSimple . String) y
    Nothing -> lispErr $ TypeMismatch "opaque connection" conn
getTablesFun x = lispErr $ TypeMismatch "opaque" x

executeDoc :: [String]
executeDoc = ["execute a statement <par>stmt</par> (string). Takes a connection,",
              "a statement and an optional list of values <par>args/par> that should be",
              "interpolated into the statement.",
              "",
              "params:",
              "- conn: the connection",
              "- stmt: the statement to execute",
              "- args: the values to interpolate (will be interpolated in place of ?, optional)",
              "complexity: O(n)",
              "returns: a list of rows"]

executeFun :: [LispVal] -> IOThrowsError LispVal
executeFun [conn@(Opaque _), (SimpleVal (String stmt))] = do
  case ((fromOpaque conn) :: Maybe (IO Connection)) of
    Just x -> do
      x <- liftLisp x
      stmt <- liftLisp $ prepare x stmt
      x <- liftLisp $ executeRaw stmt
      y <- liftLisp $ fetchAllRows' stmt
      return $ List $ map convertResultSet y
    Nothing -> lispErr $ TypeMismatch "opaque connection" conn
executeFun [conn@(Opaque _), (SimpleVal (String stmt)), List args] = do
  case ((fromOpaque conn) :: Maybe (IO Connection)) of
    Just x -> do
      x <- liftLisp x
      stmt <- liftLisp $ prepare x stmt
      x <- liftLisp $ execute stmt $ convertZeptoList args
      y <- liftLisp $ fetchAllRows' stmt
      return $ List $ (fromSimple $ Number $ NumI x) : map convertResultSet y
    Nothing -> lispErr $ TypeMismatch "opaque connection" conn
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
commitFun conn@(Opaque _) =
  case ((fromOpaque conn) :: Maybe (IO Connection)) of
    Just x -> do
      x <- liftLisp x
      x <- liftLisp $ commit x
      return $ fromSimple $ Nil ""
    Nothing -> lispErr $ TypeMismatch "opaque connection" conn
commitFun x = lispErr $ TypeMismatch "opaque" x

rollbackDoc :: [String]
rollbackDoc = ["rollbacks all pending transactions to the DB to ensure failure recovery.",
             "",
             "params:",
             "- args: the connection",
             "complexity: O(1)",
             "returns: nil"]

rollbackFun :: LispVal -> IOThrowsError LispVal
rollbackFun conn@(Opaque _) =
  case ((fromOpaque conn) :: Maybe (IO Connection)) of
    Just x -> do
      x <- liftLisp x
      x <- liftLisp $ rollback x
      return $ fromSimple $ Nil ""
    Nothing -> lispErr $ TypeMismatch "opaque connection" conn
rollbackFun x = lispErr $ TypeMismatch "opaque" x

connDoc :: [String]
connDoc = ["checks whether <par>args</par> is a database connection.",
           "",
           "params:",
           "- args: the db connection",
           "complexity: O(1)",
           "returns: a boolean"]

connFun :: LispVal -> IOThrowsError LispVal
connFun conn@(Opaque _) =
  case ((fromOpaque conn) :: Maybe (IO Connection)) of
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
connInfoFun conn@(Opaque _) =
  case ((fromOpaque conn) :: Maybe (IO Connection)) of
    Just x -> do
      x <- liftLisp x
      return $ HashMap $ fromList
        [(Atom ":driver", fromSimple $ String $ hdbcDriverName x),
         (Atom ":client-version", fromSimple $ String $ hdbcClientVer x),
         (Atom ":transaction-support", fromSimple $ Bool $ dbTransactionSupport x),
         (Atom ":server-version", fromSimple $ String $ dbServerVer x)]
    Nothing -> lispErr $ TypeMismatch "opaque connection" conn
connInfoFun x = lispErr $ TypeMismatch "opaque" x
