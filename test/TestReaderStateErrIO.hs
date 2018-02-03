module Main
where

import Control.Monad.ReaderStateErrIO

import System.Environment
import System.Exit
import System.IO
import System.Random

main :: IO ()
main
    = do args <- getArgs
         res  <- evalAction (test args) defaultEnv initSt
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

-- state wi a counter and a random generator
-- this may be configured in initSt
data St = St { cnt :: Int
             , rnd :: StdGen
             }
            deriving Show

initSt :: St
initSt = St { cnt = 0
            , rnd = mkStdGen 42
            }

type Test = Action Env St

incrCnt :: Test ()
incrCnt = do
  trc "increment state cnt"
  modify (\s -> s {cnt = cnt s + 1})

dice :: Test Int
dice = do
  St c rg <- get
  let (d, rg') = randomR (1, 6) rg
  put (St c rg')
  trc $ "new dice = " ++ show d
  return d

-- use the system random generator
sysDice :: Test Int
sysDice = io $ randomRIO (1, 6)


test :: [String] -> Test ()
test args
    = do trc "script started"

         trc "io test"
         io (putStrLn $ "the command line args = " ++ show args)

         -- state actions
         s0 <- get
         trc $ "initial state = " ++ show s0

         incrCnt
         c1 <- gets cnt
         trc $ "next count = " ++ show c1

         dices <- sequence $ replicate 10 dice
         trc $ "next 10 dices = " ++ show dices

         sd <- sysDice
         verbose $ "a dice generated with sys gen = " ++ show sd

         -- environment actions
         e <- ask
         verbose $ "env = " ++ show e

         tf <- asks flagTrace
         verbose $ "traceOn = " ++ show tf

         local (\ env -> env {flagTrace = False}) $ do
           trc "this log message is suppressed"
           d <- dice
           verbose $ "next dice = " ++ show d

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
