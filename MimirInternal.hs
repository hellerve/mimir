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

convertResultSet :: [SqlValue] -> LispVal
convertResultSet set = List $ map convert set

convertZeptoList :: [LispVal] -> [SqlValue]
convertZeptoList = map convert

exports :: [(String, [LispVal] -> IOThrowsError LispVal, String)]
exports = [(namespace "connect", unaryIOOp connectFun, "returns a DB connection"),
           (namespace "disconnect", unaryIOOp disconnectFun, "destroys a DB connection"),
           (namespace "get-tables", unaryIOOp getTablesFun, "get a list of tables"),
           (namespace "execute", executeFun, "execute a query"),
           (namespace "commit", unaryIOOp commitFun, "commit a transaction")]

parseInfo :: LispVal -> String
parseInfo (HashMap conninfo) = tail (foldrWithKey build "" conninfo)
    where build (String k) (SimpleVal (String v)) acc = acc ++ ' ' : k ++ '=' : v
          build _ _ acc = acc

connectFun :: LispVal -> IOThrowsError LispVal
connectFun conninfo@(HashMap _) =
    return $ toOpaque $ connectPostgreSQL $ parseInfo conninfo
connectFun x = lispErr $ TypeMismatch "string" x

disconnectFun :: LispVal -> IOThrowsError LispVal
disconnectFun conn@(Opaque _) = do
  x <- ((fromOpaque conn) :: IOThrowsError (IO Connection))
  x <- liftLisp x
  y <- liftLisp $ disconnect x
  return $ fromSimple $ Nil ""
disconnectFun x = lispErr $ TypeMismatch "opaque" x

getTablesFun :: LispVal -> IOThrowsError LispVal
getTablesFun conn@(Opaque _) = do
  x <- ((fromOpaque conn) :: IOThrowsError (IO Connection))
  x <- liftLisp x
  y <- liftLisp $ getTables x
  return $ List $ map (fromSimple . String) y
getTablesFun x = lispErr $ TypeMismatch "opaque" x

executeFun :: [LispVal] -> IOThrowsError LispVal
executeFun [conn@(Opaque _), (SimpleVal (String stmt))] = do
  x <- ((fromOpaque conn) :: IOThrowsError (IO Connection))
  x <- liftLisp x
  stmt <- liftLisp $ prepare x stmt
  _ <- liftLisp $ executeRaw stmt
  y <- liftLisp $ fetchAllRows stmt
  return $ List $ map convertResultSet y
executeFun [conn@(Opaque _), (SimpleVal (String stmt)), List args] = do
  x <- ((fromOpaque conn) :: IOThrowsError (IO Connection))
  x <- liftLisp x
  stmt <- liftLisp $ prepare x stmt
  _ <- liftLisp $ execute stmt $ convertZeptoList args
  y <- liftLisp $ fetchAllRows stmt
  return $ List $ map convertResultSet y
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

commitFun :: LispVal -> IOThrowsError LispVal
commitFun conn@(Opaque _) = do
  x <- ((fromOpaque conn) :: IOThrowsError (IO Connection))
  x <- liftLisp x
  _ <- liftLisp $ commit x
  return $ fromSimple $ Nil ""
commitFun x = lispErr $ TypeMismatch "opaque" x
