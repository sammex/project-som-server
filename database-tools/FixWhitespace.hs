import Database.HDBC
import Database.HDBC.ODBC
import Data.Convertible.Base
import qualified Data.ByteString.Char8 as BSC

-- | This function may need a bit of explanation. Basically speaking, it is all about states.
-- The function starts in a cut-state, which means it will ignore any whitespaces
-- until any other character appears. Then, it will switch to normal text state, where it
-- just continues to use all characters. It switches to whitespace mode when a whitespace
-- comes up. In whitespace mode, no characters are written to the resulting string, theyr are
-- only written when the program switches back to normal text mode. Therefore, if a string
-- does not end with a character, trailing whitespaces are removed.
trimWhitespace :: String -> String
trimWhitespace = cutOffFirstWS -- cutting-state
	where
		cutOffFirstWS (' ':cs) = cutOffFirstWS cs
		cutOffFirstWS cs = removeDupliWS False cs -- switch to text state (False = text, True = whitespace)
		removeDupliWS _ [] = [] -- on an empty list, do nothing (neither text nor whitespace)
		removeDupliWS _ (' ':cs) = removeDupliWS True cs -- switch to whitespace-mode, turning the character into a boolean information
		removeDupliWS b cs = if b
			then ' ':(removeDupliWS False cs) -- switch to text-mode, turning the boolean information to a whitespace character again
			else (head cs):(removeDupliWS False $ tail cs) -- go from text-mode to text-mode

-- | Actually, it splits to author and name!
splitToNameAndAuthor :: String -> (String, String)
splitToNameAndAuthor = snd . foldr (
		\chr (b, (auth, name)) -> if chr == '('
			then (False, (auth, name))
			else if b
				then (True, (chr : auth, name))
				else (False, (auth, chr : name))
	) (True, ([], [])) . init

joinNameAndAuthor :: (String, String) -> String
joinNameAndAuthor (a, n) = n ++ " (" ++ a ++ ")"

main :: IO ()
main = do {
	conn <- connectODBC "DSN=PurposeMySQL";
	allQuery <- prepare conn "SELECT id, name FROM books;";
	execute allQuery [];
	ll <- fetchAllRows allQuery;
	sequence_ $ map (sqlTrimWhitespace conn) ll;
	}

sqlTrimWhitespace :: IConnection conn => conn -> [SqlValue] -> IO ()
sqlTrimWhitespace conn vals = do {
	let {ei = convert (vals !! 0) :: Int};
	let {en = convert (vals !! 1) :: String};
	let {rs = joinNameAndAuthor $ mapBoth trimWhitespace $ splitToNameAndAuthor en};
	if rs == en
		then putStrLn $ "Already OK: " ++ en
		else do {
			putStrLn $ "Modifying: " ++ (show en) ++ " -> " ++ (show rs);
			repqry <- prepare conn "UPDATE books SET name=? WHERE id=?;";
			execute repqry [SqlByteString $ BSC.pack rs, toSql ei];
			commit conn;
		}
	}

mapBoth :: (a -> b) -> (a, a) -> (b, b)
mapBoth f (x,y) = (f x, f y)
