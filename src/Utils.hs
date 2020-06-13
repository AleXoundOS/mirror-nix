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

mapBoth :: (a -> c) -> (b -> d) -> Either a b -> Either c d
mapBoth f _ (Left x)  = Left (f x)
mapBoth _ f (Right x) = Right (f x)
