module Utils where


import Control.Monad.IO.Class
import GHC.Stack
import System.IO


forceEitherStr :: HasCallStack => Either String r -> r
forceEitherStr = either error id

putStrIO :: MonadIO m => String -> m ()
putStrIO = liftIO . (\s -> putStr s >> hFlush stdout)

putStrLnIO :: MonadIO m => String -> m ()
putStrLnIO = liftIO . putStrLn
