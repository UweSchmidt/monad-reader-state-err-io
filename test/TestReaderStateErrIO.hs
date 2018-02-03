module Main
where

import Control.Monad.ReaderStateErrIO

import System.Environment
import System.Exit
import System.IO

main :: IO ()
main
    = do args <- getArgs
         res <- evalAction (test args) defaultEnv 0
         maybe
           exitFailure
           return
           res

data Env = Env { flagTrace   :: Bool
               , flagVerbose :: Bool
               }
         deriving Show

instance Config Env where
    traceOn   = flagTrace
    verboseOn = flagVerbose

defaultEnv :: Env
defaultEnv = Env { flagTrace   = True
                 , flagVerbose = True
                 }
type St = Int

initSt :: St
initSt = 0

type Test = Action Env St

test :: [String] -> Test ()
test args
    = do trc "script started"

         trc "io test"
         io $ putStrLn . show $ args

         -- state actions
         s0 <- get
         trc $ "initial state = " ++ show s0

         put (s0 + 1)
         s1 <- get
         trc $ "next state = " ++ show s1

         modify (+ 41)
         s2 <- get
         trc $ "next state = " ++ show s2

         -- environment actions
         e <- ask
         verbose $ "env = " ++ show e

         tf <- asks flagTrace
         verbose $ "traceOn = " ++ show tf

         local (\ env -> env {flagTrace = False}) $ do
           trc "this log message is suppressed"
           modify (* 2)
           s3 <- get
           verbose $ "state is now = " ++ show s3

         -- action combinators
         trc "always test"
         always $ do io $ putStrLn "hello"
                     trc $ "hello"
                     _ <- abort "hello aborted"
                     io $ putStrLn "good bye"
                     trc $ "good bye"

         trc "orElse test"
         ( warn "abort will be called" >> abort "abort called" ) `orElse` trc "2. try"

         trc "catch io error test"
         ( io $ do h <- openFile "xxx" ReadMode
                   hClose h ) `orElse` (err "open file xxx has failed")

         trc "exec test"
         always $ exec "ls" ["-l", "."]

         trc "exec test which fails"
         always $ exec "xxx" ["-l", "."]

         trc "script finished"
         return ()

-- ----------------------------------------
