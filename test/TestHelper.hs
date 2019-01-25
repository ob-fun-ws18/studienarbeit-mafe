module TestHelper (
    withHandle
) where

import System.IO.Temp
import GHC.IO.Handle

withHandle :: (Handle -> IO ()) -> IO ()
withHandle func = do withSystemTempFile "tmp.test" (execFunc func)

execFunc :: (Handle -> IO ()) -> (FilePath -> Handle -> IO ())
execFunc func = (\_ h -> func h)
