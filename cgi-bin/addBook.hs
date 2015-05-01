module Main where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Error
import Database.HDBC.ODBC
import Database.HDBC
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BSUTF
import Data.Maybe
import Data.List
import Data.URLEncoded
import Debug.Trace
import Network.CGI.Protocol (maybeRead)
import Network.URI
import System.Exit
import System.IO

data BookDescriptorKey = Name | Author | C Int deriving Show

instance Eq BookDescriptorKey where
	Name == Name = True
	Author == Author = True
	C x == C y = x == y
	_ == _ = False

instance Ord BookDescriptorKey where
	compare Name Author = LT
	compare Name (C x) = LT
	compare Author Name = GT
	compare Author (C x) = LT
	compare (C x) Name = GT
	compare (C x) Author = GT
	compare (C x) (C y) = compare x y
	compare _ _ = EQ

bdkFromString :: String -> Maybe BookDescriptorKey
bdkFromString "name" = Just Name
bdkFromString "autor" = Just Author
bdkFromString "author" = Just Author
bdkFromString ('c':xs) = C <$> maybeRead xs
bdkFromString _ = Nothing

splitVars :: String -> [(String, String)]
splitVars s = either (const []) id $ pairs <$> (importString s :: Either String URLEncoded)

sortPurposeMySQL :: [(String, String)] -> [(BookDescriptorKey, String)]
sortPurposeMySQL = sortBy (\(a, _) (b, _) -> compare a b) . mapMaybe (\(k, v) -> (\a -> (a, v)) <$> bdkFromString k)

prepareSQLValues :: [(BookDescriptorKey, String)] -> [SqlValue]
prepareSQLValues l = mapMaybe (\(k, v) -> case k of
	{
	Name -> Just $ SqlByteString $ BSUTF.fromString $ (v ++ maybe " (?)" (\n -> " (" ++ n ++ ")") (snd <$> find (\(a, b) -> a == Author) l));
	Author -> Nothing;
	C x -> Just $ toSql $ fromMaybe 0 (maybeRead v :: Maybe Double)
	}) l

main :: IO ()
main = do
	{
	hSetEncoding stdin utf8;
	putStr "Content-Type: text/plain; charset=utf8\n\n";
	vars <- prepareSQLValues <$> sortPurposeMySQL <$> splitVars <$> getLine;
	when (length vars < 13) $ putStr "P" >> exitFailure;
	conn <- connectODBC "DSN=PurposeMySQL";
	ifQuery <- prepare conn "SELECT count FROM books WHERE name=?;";
	execute ifQuery (take 1 vars);
	res <- fetchRow ifQuery;
	if isJust res then case fromJust res of
		{
		[SqlInt32 i] -> insertUsingCount (fromIntegral i) conn vars;
		_ -> putStr "E"
		}
	else insertUsingCount 0 conn vars;
	}

insertUsingCount :: IConnection conn => Int -> conn -> [SqlValue] -> IO ()
insertUsingCount n conn vars
	| n < 0 = putStr "P"
	| n == 0 = do
		{
		columnQuery <- prepare conn "INSERT INTO books (name, count, spann, action, lineart, emotion, realit, einfuehl, horror, gutboes, tief, zeit, nachv, humor, ende) VALUES (?, 1, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?);";
		r <- execute columnQuery vars;
		commit conn;
		if r > 0 then putStr "S" else putStr "E";
		}
	| n > 0 = do
		{
		existingQ <- prepare conn "SELECT spann, action, lineart, emotion, realit, einfuehl, horror, gutboes, tief, zeit, nachv, humor, ende FROM books WHERE name=?;";
		execute existingQ (take 1 vars);
		existingUgly <- fetchRow existingQ;
		if isNothing existingUgly then putStr "E" else do
			{
			let {existingSqlV = fromJust existingUgly};
			let {mvals = zipWith (\(SqlDouble a) (SqlDouble b) ->
				SqlDouble $ (a + fromIntegral n * b) / (fromIntegral n + 1)) (drop 1 vars) existingSqlV};
			let {mvars = take 1 vars ++ [SqlInt32 $ fromIntegral n + 1] ++ mvals};
			columnQuery <- prepare conn "REPLACE INTO books (name, count, spann, action, lineart, emotion, realit, einfuehl, horror, gutboes, tief, zeit, nachv, humor, ende) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?);";
			r <- execute columnQuery mvars;
			commit conn;
			if r > 0 then putStr "R" else putStr "E";
			};
		}
