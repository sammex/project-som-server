import Codec.Binary.UTF8.String
import Control.Exception
import Database.HDBC
import Database.HDBC.ODBC
import qualified Data.ByteString.Char8 as BSC
import Data.Convertible.Base
import System.Environment

main :: IO ()
main = do {
		conn <- connectODBC "DSN=PurposeMySQL";
		allQuery <- prepare conn "SELECT id, name FROM books;";
		execute allQuery [];
		ll <- fetchAllRows allQuery;
		sequence_ $ map (fixUtf16 conn) ll;
	}

fixUtf16 :: IConnection conn => conn -> [SqlValue] -> IO ()
fixUtf16 conn l = do {
		let {ei = convert (l !! 0) :: Int};
		let {es = convert (l !! 1) :: String};
		if '\NUL' `elem` es then do {
			rq <- prepare conn "UPDATE books SET name=? WHERE id=?;";
			let {r = SqlByteString $ BSC.pack $ utf8Encode $ stripOfNULs es};
			stat <- try $ execute rq [r, toSql $ ei] :: IO (Either SqlError Integer);
			case stat of {
				Left _ -> putStrLn $ "Error on: " ++ es;
				Right _ -> putStrLn $ "Converted: " ++ (show r);
			};
			commit conn;
		} else putStrLn $ "Already OK: " ++ es;
	}

stripOfNULs :: String -> String
stripOfNULs ('\NUL':cs) = stripOfNULs cs
stripOfNULs (c:cs) = c : stripOfNULs cs
stripOfNULs [] = []
