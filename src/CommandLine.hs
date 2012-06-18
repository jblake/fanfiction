-- Copyright © 2012 Julian Blake Kongslie <jblake@omgwallhack.org>
-- Licensed under the MIT license.

{-# LANGUAGE GADTs #-}

module CommandLine
where

import Control.Monad
import Control.Applicative
import Data.Attoparsec.Combinator
import Data.Maybe
import System.Environment

newtype CommandLineParser a = CLP { openCLP :: [String] -> [(a, [String])] }

instance Functor CommandLineParser where
  fmap f (CLP p) = CLP $ \strs -> [ (f x, strs') | (x, strs') <- p strs ]

instance Applicative CommandLineParser where
  pure x = CLP $ \strs -> [(x, strs)]
  (CLP fs) <*> (CLP xs) = CLP $ \strs -> [ (f x, strs'') | (f, strs') <- fs strs, (x, strs'') <- xs strs' ]

instance Alternative CommandLineParser where
  empty = CLP $ \_ -> []
  (CLP l) <|> (CLP r) = CLP $ \strs -> l strs ++ r strs

instance Monad CommandLineParser where
  return = pure
  c >>= f = joinCLP $ fmap f c
    where
      joinCLP :: CommandLineParser (CommandLineParser a) -> CommandLineParser a
      joinCLP (CLP clps) = CLP $ \strs -> [ (x, strs'') | (CLP clp, strs') <- clps strs, (x, strs'') <- clp strs' ]

instance MonadPlus CommandLineParser where
  mzero = empty
  mplus = (<|>)

parseCommandLine :: CommandLineParser a -> [String] -> Maybe a
parseCommandLine p = fmap fst . listToMaybe . openCLP (p >>= \x -> endOfArgs >> return x)

getArg :: CommandLineParser String
getArg = CLP $ \strs -> case strs of
  arg:args -> [(arg, args)]
  []       -> []

readArg :: (Read a) => CommandLineParser a
readArg = fmap read $ getArg

getAllArgs :: CommandLineParser [String]
getAllArgs = CLP $ \strs -> [(strs, [])]

readAllArgs :: (Read a) => CommandLineParser [a]
readAllArgs = fmap (fmap read) $ getAllArgs

endOfArgs :: CommandLineParser ()
endOfArgs = CLP $ \strs -> case strs of
  [] -> [((),[])]
  _  -> []

whenArg :: (String -> Bool) -> CommandLineParser String
whenArg cond = do
  x <- getArg
  guard $ cond x
  return x

expect :: String -> CommandLineParser String
expect = whenArg . (==)

data Command a where
  Cmd :: String -> String -> a -> Command a
  (:@) :: Command (String -> a) -> String -> Command a
  (:@/) :: Command ([String] -> a) -> String -> Command a
  (:@@) :: (Read a) => Command (a -> b) -> String -> Command b
  (:@@/) :: (Read a) => Command ([a] -> b) -> String -> Command b

parseCommand :: Command a -> CommandLineParser a
parseCommand (Cmd name help f) = expect name *> pure f
parseCommand (cmd :@ help)     = parseCommand cmd <*> getArg
parseCommand (cmd :@/ help)    = parseCommand cmd <*> getAllArgs
parseCommand (cmd :@@ help)    = parseCommand cmd <*> readArg
parseCommand (cmd :@@/ help)   = parseCommand cmd <*> readAllArgs

parseCommands :: [Command a] -> IO (Maybe a)
parseCommands commands = do
  args <- getArgs
  return $ parseCommandLine (choice $ map parseCommand commands) args

printHelp :: [Command a] -> IO ()
printHelp commands = do
  progName <- getProgName
  putStrLn $ "Usage: " ++ progName ++ " subcommand subcommand-args"
  forM_ commands $ \cmd -> do
    printInvocation cmd
    putStrLn ""
    printDescription cmd
  where

    printInvocation :: Command a -> IO ()
    printInvocation (Cmd name help f) = putStr $ "  " ++ name
    printInvocation (cmd :@ help)     = do
      printInvocation cmd
      putStr $ " " ++ help
    printInvocation (cmd :@/ help)    = do
      printInvocation cmd
      putStr $ " [" ++ help ++ " ..]"
    printInvocation (cmd :@@ help)    = do
      printInvocation cmd
      putStr $ " " ++ help
    printInvocation (cmd :@@/ help)   = do
      printInvocation cmd
      putStr $ " [" ++ help ++ " ..]"

    printDescription :: Command a -> IO ()
    printDescription (Cmd name help f) = putStrLn $ "    " ++ help
    printDescription (cmd :@ help)     = printDescription cmd
    printDescription (cmd :@/ help)    = printDescription cmd
    printDescription (cmd :@@ help)    = printDescription cmd
    printDescription (cmd :@@/ help)   = printDescription cmd
