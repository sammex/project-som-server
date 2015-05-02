module Main where
import Codec.Binary.UTF8.String
import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.IO.Class
import Database.HDBC
import Database.HDBC.ODBC
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.UTF8 as BSU
import Data.Convertible.Base
import Data.Maybe
import Data.URLEncoded
import Data.List
import Debug.Trace
import System.Exit
import System.IO

main :: IO ()
main = do {
	hSetEncoding stdin utf8;
	hSetEncoding stdout utf8;
	conn <- connectODBC "DSN=PurposeMySQL";
	putStr "Content-Type: text/html; charset=utf8\n\n";
	s <- getLine;
	let {p = either (const [("name","")]) pairs (importString s :: Either String URLEncoded)};
	let {f = find ((==) "name" . fst) p};
	when (isNothing f) $ putStr "P" >> exitFailure;
	l <- getFirstMatchingBooks 20 (snd $ fromJust f) conn;
	let {inter = BSC.unlines l};
	if (isUTF8Encoded . BSC.unpack) inter then putStr $ decodeString $ BSC.unpack inter else putStr $ BSC.unpack inter;
	}

getFirstMatchingBooks :: IConnection conn => Int -> String -> conn -> IO [ByteString]
getFirstMatchingBooks i prefix conn
	| i <= 0 = return []
	| otherwise = do {
		query <- prepare conn $ "SELECT name FROM books WHERE name LIKE ? ORDER BY name ASC LIMIT " ++ show i ++ ";";
		execute query [SqlByteString $ BS.pack $ encode $ "%" ++ prefix ++ "%"];
		fetched <- fetchAllRows query;
		return $ map (\[e] -> (convert e :: ByteString)) fetched;
	}
