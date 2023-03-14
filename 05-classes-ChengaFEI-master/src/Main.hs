import GHC.IO.Encoding
import qualified Language.Nano.Eval as Nano
import Language.Nano.Repl
import qualified Language.Nano.Types as Nano
import Text.Printf

main :: IO ()
main = do
  setLocaleEncoding utf8
  error "TBD: main"

--------------------------------------------------------------------------------

-- | Some useful functions

--------------------------------------------------------------------------------
-- putStr   :: String -> IO ()
-- hFlush   ::
-- putStrLn :: String -> IO ()
-- getLine  :: IO String
