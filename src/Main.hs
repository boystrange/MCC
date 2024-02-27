{-
┌───────────────────────────────────────────────────────────────────╖
│ This file is part of MC².                                         ║
│                                                                   ║
│ MC² is free software: you can redistribute it and/or modify it    ║
│ under the terms of the GNU General Public License as published by ║
│ the Free Software Foundation, either version 3 of the License, or ║
│ (at your option) any later version.                               ║
│                                                                   ║
│ MC² is distributed in the hope that it will be useful, but        ║
│ WITHOUT ANY WARRANTY; without even the implied warranty of        ║
│ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU ║
│ General Public License for more details.                          ║
│                                                                   ║
│ You should have received a copy of the GNU General Public License ║
│ along with MC². If not, see <http://www.gnu.org/licenses/>.       ║
│                                                                   ║
│ Copyright 2018 Luca Padovani                                      ║
╘═══════════════════════════════════════════════════════════════════╝
-}
{-# LANGUAGE CPP #-}

module Main (main) where

import Language
import Render
import Exceptions
import qualified Inference
import qualified Constraints
import qualified Validation
#if defined(USE_Z3)
import qualified Presburger.Z3.Solver as Solver
#else
import qualified Presburger.Lash.Solver as Solver
#endif
import Parser
import System.Console.GetOpt
import System.IO (stdout, stderr, hFlush, hPutStrLn)
import System.Exit (exitWith, ExitCode(ExitSuccess, ExitFailure))
import System.Environment (getProgName, getArgs)
import Control.Monad (forM_, unless, when)
import Control.Exception
import qualified Data.Version
import Data.List (replicate, intersperse)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Time (getCurrentTime, diffUTCTime)
import System.FilePath.Posix (takeFileName)

putTitle :: String -> IO ()
putTitle s = printTitle ("\n" ++ s ++ "\n")

version :: Data.Version.Version
version = Data.Version.makeVersion [1, 1]

main :: IO ()
main = do
  progName <- getProgName
  (args, file) <- getArgs >>= parse progName
  source <- if file == "-" then getContents else readFile file
  let result = parseProcess file source
  case result of
    Left msg -> printWarning msg
    Right (idefs, ps) -> catch (check file args idefs ps) (handler args)
  where
    check :: FilePath -> [Flag] -> [S_Interface] -> [S_ProcessDefinition] -> IO ()
    check file args idefs ps = do
      let full     = Full `elem` args
      let verbose  = Verbose `elem` args
      let deadlock = not $ Main.Deadlock `elem` args
      let logging  = Logging `elem` args
      when logging
        (do putStr $ takeFileName file ++ " ... "
            hFlush stdout)
      when (not deadlock)
        (printWarning $ "WARNING: disabling deadlock analysis may also invalidate mailbox conformance analysis")
      start <- getCurrentTime
      let (ienv, qs, cset) = Inference.generate deadlock idefs ps
      when verbose
        (do putTitle "ANNOTATED INTERFACES"
            forM_ (M.toList ienv) (uncurry printInterface)
            putTitle "ANNOTATED PROCESSES"
            forM_ (M.toList qs) (uncurry (printProcessDefinition full))
            putTitle "CONSTRAINTS"
            forM_ cset (putStrLn . show))
      when verbose (putTitle "SOLVING CONSTRAINTS")
      Solver.initialize
      solution <- Constraints.resolve Solver.solver verbose cset
      when verbose
        (do putTitle "SOLUTION"
            forM_ (M.toList solution) (\(α, π) -> putStrLn $ showPatternVariable α ++ " = " ++ show π))
      (ienv', qs') <- Validation.validate solution ienv qs
      when verbose
        (do putTitle "TYPED INTERFACES"
            forM_ (M.toList ienv') (uncurry printInterface)
            putTitle "TYPED PROCESSES"
            forM_ (M.toList qs') (uncurry (printProcessDefinition full)))
      stop <- getCurrentTime
      when logging (printOK (show (diffUTCTime stop start)))

    handler :: [Flag] -> MyException -> IO ()
    handler args _ | Logging `elem` args = printWarning "FAILED"
    handler _ e = printWarning (show e)

data Flag = Deadlock -- -d --deadlock
          | Verbose  -- -v --verbose
          | Version  -- -V --version
          | Logging  --    --log
          | Help     --    --help
          | Full     --    --full
            deriving (Eq, Ord)

flags :: [OptDescr Flag]
flags =
   [ Option "d" ["deadlock"] (NoArg Main.Deadlock) "Disable deadlock analysis"
   , Option []  ["full"]     (NoArg Full)        "Print full type information"
   , Option []  ["log"]      (NoArg Logging)     "Log type checking time"
   , Option "v" ["verbose"]  (NoArg Verbose)     "Print type checking activities"
   , Option "V" ["version"]  (NoArg Version)     "Print version information"
   , Option "h" ["help"]     (NoArg Help)        "Print this help message" ]

versionInfo :: String -> String
versionInfo progName =
  "MC² " ++ Data.Version.showVersion version ++ " Copyright © 2018 Luca Padovani\n"
  ++ "License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>.\n"
  ++ "This is free software: you are free to change and redistribute it.\n"
  ++ "There is NO WARRANTY, to the extent permitted by law."

parse progName argv =
  case getOpt Permute flags argv of
    (args, files, []) -> do
      when (Version `elem` args)
        (do hPutStrLn stderr (versionInfo progName)
            exitWith ExitSuccess)
      when (null files || length files > 1 || Help `elem` args)
        (do hPutStrLn stderr (usageInfo header flags)
            exitWith ExitSuccess)
      return (args, head files)
    (_, _, errs) -> do
      hPutStrLn stderr (concat errs ++ usageInfo header flags)
      exitWith (ExitFailure 1)
  where
    header = "Usage: " ++ progName ++ " [options] [FILE]"
