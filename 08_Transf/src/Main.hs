{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Applicative        (Alternative (..), liftA2, (*>), (<*))
import           Control.Arrow              (second)
import           Control.Exception          (Exception, SomeException, catch)
import           Control.Exception.Base     (throwIO)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Reader (ReaderT (..), ask, local)
import           Data.Char                  (isSpace)
import           Data.IORef                 (IORef, newIORef, readIORef, writeIORef)
import           Data.Map.Strict            (Map, (!))
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (fromJust)
import           Data.Monoid                ((<>))
import           Data.Text                  (Text, dropWhile, lines, pack, split, splitOn,
                                             strip, takeWhile, unpack)
import qualified Data.Text                  as T
import           Data.Text.IO               (getLine, hGetContents, hPutStrLn, putStr,
                                             putStrLn)
import           Data.Typeable              (Typeable)
import           Prelude                    hiding (dropWhile, getLine, lines, putStr,
                                             putStrLn, takeWhile)
import           System.Environment         (getArgs)
import           System.IO                  (Handle, IOMode (ReadMode, WriteMode), stdout,
                                             withFile)
import           System.IO.Error            (isDoesNotExistError)

data AbortLoop = AbortLoop
    deriving (Show, Typeable, Exception)

propertyParser :: ReaderT Text IO (Text, Text)
propertyParser = do
    s <- ask
    let striped = strip s
    let first_id = takeWhile (\x -> not $ isSpace x || x == '=') striped
    let without_first = dropWhile (\x -> not $ isSpace x || x == '=') striped
    let second_id = strip (dropWhile (=='=') (strip without_first))
    return (first_id, first_id)

type Config = Map Text (IO (IORef Text))

readConfig :: ReaderT Handle IO Config
readConfig = do
    hFile <- ask
    content <- lift $ hGetContents hFile
    let params' = lines content
    let params  = filter (not . T.null) params'
    let parsedParams = map (runReaderT propertyParser) params
    lift $ putStrLn ""
    lift $ putStrLn "parsed content: "
    lift $ mapM_ (>>= putStrLn . pack . show) parsedParams
    lift $ putStrLn ""

    let ioArrayOfParams = sequence parsedParams
    arrayOfParams <- lift ioArrayOfParams

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

writeConfig :: Handle -> ReaderT Config IO ()
writeConfig hFile = do
    config <- ask
    let lstConfig = Map.toList config :: [(Text, IO (IORef Text))]
    lift $
        mapM_ (\(str, ioIoRef) -> do ioRef <- ioIoRef
                                     ref   <- readIORef ioRef
                                     hPutStrLn hFile (str <> " " <> ref)) lstConfig

mainLoop :: ReaderT Config IO Config
mainLoop = do
    config <- ask
    lift $ putStrLn "Input property and value:"
    propWIthValue <- lift getLine
    (lhs, rhs) <- lift $ runReaderT propertyParser propWIthValue
    case lhs of
        "W" -> do
            lift $ putStrLn ""
            lift $ putStrLn "Next properties are written:"
            ask
        "B" -> do
            let prev_value_io_ref = config Map.! rhs
            prev_value_ref <- lift prev_value_io_ref
            prev_value <- lift $ readIORef prev_value_ref

            lift $ putStrLn ("Input new value for `" <> rhs <> "` property (previous: `" <> prev_value <> "`)")
            newProp <- lift getLine
            let nConfig = Map.insert rhs (newIORef newProp) config
            local (const nConfig) mainLoop
        "A" ->
            lift $ throwIO AbortLoop
        "S" -> do
            writeConfig stdout
            mainLoop
        otherwise -> do
            let nConfig = Map.insert lhs (newIORef rhs) config
            local (const nConfig) mainLoop


startInteractiveMode :: Handle -> ReaderT Config IO ()
startInteractiveMode hFile = do
    config <- ask
    nConfig <- lift $ runReaderT mainLoop config `catch` \(e :: SomeException) -> do
                          runReaderT (writeConfig hFile) config
                          throwIO e
    local (const nConfig) (writeConfig hFile)

main :: IO ()
main = do
    args <- getArgs
    let inFile = head args
    printHeader
    mp <- withFile inFile ReadMode (runReaderT readConfig) `catch` readHandler
    withFile inFile WriteMode (\h -> runReaderT (startInteractiveMode h) mp)

