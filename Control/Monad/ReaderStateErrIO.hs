module Control.Monad.ReaderStateErrIO
    ( module Control.Monad.ReaderStateErrIO
    , module Control.Monad.Trans.Except
    , module Control.Monad.Reader
    , module Control.Monad.State.Strict
    , module Control.Monad
    )
where

import Control.Exception        ( IOException, try )

import Control.Monad.Trans.Except
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad

import System.Process           ( rawSystem, readProcessWithExitCode )
import System.Exit
import System.IO                ( hPutStrLn
                                , stderr
                                )

-- ----------------------------------------

type Action r s = ExceptT Msg (ReaderT r (StateT s IO))

runAction :: Action r s a -> r -> s -> IO (Either Msg a, s)
runAction action env0 state0
    = runStateT (runReaderT (runExceptT action) env0) state0

evalAction :: Action r s a -> r -> s -> IO (Maybe a)
evalAction action env0 state0
    = do (res, _state1) <- runAction action env0 state0

         -- issue error or return value
         either (\ x -> hPutStrLn stderr (unMsg x) >> return Nothing)
                (return . Just)
                res

-- ----------------------------------------

class Config c where
    traceOn :: c -> Bool
    traceOn = const True

    verboseOn :: c -> Bool
    verboseOn = const True

    warningOn :: c -> Bool
    warningOn = const True

    errorOn :: c -> Bool
    errorOn = const True

    getLogOp :: c -> (String -> IO ())
    getLogOp = const $ hPutStrLn stderr

-- ----------------------------------------

newtype Msg = Msg String
    deriving (Show)

unMsg :: Msg -> String
unMsg (Msg s) = s

abort :: Config r => String -> Action r s a
abort msg = do
  err msg
  throwE . Msg $ msg

-- ----------------------------------------

logg :: Config r => (r -> Bool) -> String -> String -> Action r s ()
logg enabled level msg = do
  asks enabled
    `guards`
    do cmd <- asks getLogOp
       io $ cmd fmt
  where
    fmt  = take ind (level ++ ":" ++ inds) ++ msg
    ind  = 10
    inds = replicate ind ' '

-- convenience functions for logging

trc :: Config r => String -> Action r s ()
trc = logg traceOn "trace"

verbose :: Config r => String -> Action r s ()
verbose = logg verboseOn "verbose"

warn :: Config r => String -> Action r s ()
warn = logg warningOn "warning"

err :: Config r => String -> Action r s ()
err = logg errorOn "error"

-- ----------------------------------------

-- perform an IO actions and catch all
-- IOExceptions and throw an error with
-- the IOException message

io :: Config r => IO a -> Action r s a
io x
    = do r <- liftIO $ try x
         either (abort . showExc) return $ r
    where
      showExc :: IOException -> String
      showExc = show

always :: Action r s () -> Action r s ()
always x
    = x `orElse` return ()

orElse :: Action r s a -> Action r s a -> Action r s a
orElse x1 x2
    = x1 `catchE` try2
    where
      try2 (Msg _s)
          = do -- warn $ "error ignored: " ++ s
               x2

finally :: Action r s a -> Action r s () -> Action r s a
finally x fin
    = ( do res <- x
           fin
           return res
      ) `catchE` (\ e -> fin >> throwE e)

guards :: Monad m => m Bool -> m () -> m ()
guards g x
    = do b <- g
         when b x

-- ----------------------------------------

-- execute external program

exec :: Config r => String -> [String] -> Action r s ()
exec cmd args
    = do rc <- io $ rawSystem cmd args
         if rc == ExitSuccess
            then return ()
            else abort $
                 unwords
                 [ "error in executing external program: "
                 , cmd
                 , show args
                 ]

-- convenience function for simple commands

execStr :: Config r => String -> Action r s ()
execStr cmd0
    = exec (concat . take 1 $ cmd) (drop 1 cmd)
    where
      cmd = words cmd0

execProcess :: Config r => String -> [String] -> String -> Action r s String
execProcess cmd args input
  = do (rc, stdOut, stdErr) <- io $ readProcessWithExitCode cmd args input
       if rc == ExitSuccess
         then do mapM_ warn $ lines stdErr
                 return stdOut
         else do mapM_ err  $ lines stdErr
                 abort $
                   unwords
                   [ "error in executing external program: "
                   , cmd
                   , show args
                   , show input
                   ]

-- ----------------------------------------
