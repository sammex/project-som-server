import Control.Monad
import Database.HDBC
import Database.HDBC.ODBC
import System.Environment
import System.Exit

main :: IO ()
main = do {
	args <- getArgs;
	when (args == []) $ putStrLn "No arguments given -> no columns deleted" >> exitSuccess;
	conn <- connectODBC "DSN=PurposeMySQL";
	delqry <- prepare conn "DELETE FROM books WHERE id=?;";
	mapM_ (
		\arg -> execute delqry [toSql (read arg :: Int)]
	) args;
	commit conn;
	}
