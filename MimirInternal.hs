module MimirInternal where

import Database.HDBC (getTables)
import Database.HDBC.PostgreSQL

import Zepto.Types

namespace :: String -> String
namespace fun = "mimir:" ++ fun

exports :: [(String, [LispVal] -> IOThrowsError LispVal, String)]
exports = [(namespace "connect", unaryIOOp connectFun, "returns a DB connection"),
           (namespace "get-tables", unaryIOOp getTablesFun, "get a list of tables")]

parseInfo :: LispVal -> String
parseInfo (HashMap conninfo) = tail (foldrWithKey build "" conninfo)
    where build (String k) (SimpleVal (String v)) acc = acc ++ ' ' : k ++ '=' : v
          build _ _ acc = acc

connectFun :: LispVal -> IOThrowsError LispVal
connectFun conninfo@(HashMap _) =
    return $ toOpaque $ connectPostgreSQL $ parseInfo conninfo
connectFun x = lispErr $ TypeMismatch "string" x

getTablesFun :: LispVal -> IOThrowsError LispVal
getTablesFun conn@(Opaque _) = do
  x <- ((fromOpaque conn) :: IOThrowsError (IO Connection))
  x <- liftLisp x
  y <- liftLisp $ getTables x
  return $ List $ map (fromSimple . String) y
getTablesFun x = lispErr $ TypeMismatch "opaque" x
