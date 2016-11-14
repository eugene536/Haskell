{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Prelude                hiding (putStrLn, putStr, lines, getLine, concat)
import           Control.Exception      (Exception, SomeException, catch)
import           Control.Exception.Base (throwIO)
import           System.Environment     (getArgs)
import           System.IO              (Handle, stdout, IOMode (ReadMode, WriteMode), withFile)
import           System.IO.Error        (isDoesNotExistError)
import           Data.Maybe             (fromJust)
import           Control.Applicative    (Alternative (..), (*>), (<*), liftA2)
import           Data.Typeable          (Typeable)
import           Data.IORef             (IORef, newIORef, readIORef, writeIORef)
import           Data.Map.Strict        (Map, (!))
import qualified Data.Map.Strict        as Map
import           Data.Text              (Text, pack, unpack, lines, strip, splitOn, split, concat)
import qualified Data.Text              as T
import           Data.Text.IO           (putStrLn, getLine, putStr, hGetContents, hPutStrLn)
--import           Data.List.Split        (splitOn)

import           Parser                 (Parser (..), second)
import           UsefulParsers          (ident, char, optional, spaces)

data InvalidArgument = InvalidArgument Text
    deriving (Show, Typeable)

instance Exception InvalidArgument where

data AbortLoop = AbortLoop
    deriving (Show, Typeable, Exception)

propertyParser :: Text -> IO (Text, Text)
propertyParser s = do
    let striped = strip s
    let splited = split (\x -> (x == ' ') || (x == '=')) striped
    case length splited of
        1 -> return (head splited, mempty)
        2 -> return (head splited, last splited)
        otherwise -> throwIO $ InvalidArgument $ concat ["problem: ", s]

type Config = Map Text (IO (IORef Text))

readConfig :: Handle -> IO Config
readConfig hFile = do
    content <- hGetContents hFile 
    let params' = lines content
    let params  = filter (not . T.null) params'
    let parsedParams = map propertyParser params
    putStrLn ""
    putStrLn $ "parsed content: "
    mapM (>>= putStrLn . pack . show) parsedParams
    putStrLn ""

    let ioArrayOfParams = sequence parsedParams
    arrayOfParams <- ioArrayOfParams

    return $ Map.fromList $ map (second newIORef) arrayOfParams

readHandler :: Monoid a => IOError -> IO a
readHandler e = 
    if isDoesNotExistError e then
        return mempty
    else
        throwIO e

printHeader :: IO ()
printHeader = do
    putStr "Interactive options:\n\
      \* B <value> : modify previous value\n\
      \* W         : finish session and write to file\n\
      \* A         : abort session (discard all changes)\n\
      \* S         : show current values\n"
    return ()

writeConfig :: Config -> Handle -> IO ()
writeConfig config hFile = do
    let lstConfig = Map.toList config :: [(Text, IO (IORef Text))]
    mapM_ (\(str, ioIoRef) -> do ioRef <- ioIoRef 
                                 ref   <- readIORef ioRef
                                 hPutStrLn hFile (concat [str, " ", ref])) lstConfig
    
mainLoop :: Config -> IO Config
mainLoop config = do
    putStrLn "Input property and value:"
    propWIthValue <- getLine
    (lhs, rhs) <- propertyParser propWIthValue
    case lhs of
        "W" -> do
            putStrLn ""
            putStrLn "Next properties are written:"
            writeConfig config stdout
            return config
        "B" -> do
            let prev_value_io_ref = config Map.! rhs
            prev_value_ref <- prev_value_io_ref
            prev_value <- readIORef prev_value_ref

            putStrLn (concat ["Input new value for `", rhs, "` property (previous: `", prev_value, "`)"])
            newProp <- getLine
            let nConfig = Map.insert rhs (newIORef newProp) config
            mainLoop nConfig
        "A" -> do
            throwIO AbortLoop
        "S" -> do
            writeConfig config stdout
            mainLoop config
        otherwise -> do
            let nConfig = Map.insert lhs (newIORef rhs) config
            mainLoop nConfig


startInteractiveMode :: Config -> Handle -> IO ()
startInteractiveMode config hFile = do
    nConfig <- (mainLoop config) `catch` \(e :: SomeException) -> do
                writeConfig config hFile
                throwIO e
    writeConfig nConfig hFile
    return ()

main :: IO ()
main = do
    args <- getArgs
    let inFile = args !! 1
    printHeader
    mp <- withFile inFile ReadMode readConfig `catch` readHandler
    withFile inFile WriteMode (startInteractiveMode mp)
    return ()


