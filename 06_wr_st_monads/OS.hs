module OS where

data Tag = Yield | Fork | Read | Write | Exit 
    deriving Show

data Context = Context
               { tag  :: Tag
               , args :: [String]
               , cont :: String -> Context
               }

--Cont Context String
kernel [] = return ()
kernel pp@((p, r):procs) = do
  let res = p r
  case tag res of
    Yield  -> do
            kernel (procs ++ [(cont res, "")])
    Fork -> do
            kernel (procs ++ [(cont res, "") , (cont res , "other")])
    Read -> do
            str <- getLine
            kernel (procs ++ [(cont res, str)])
    Write -> do
            putStrLn (args res !! 0)
            kernel (procs ++ [(cont res, "")])
    Exit -> do
            kernel procs

main _ = Context
    { tag = Read
    , args = []
    , cont = afterRead
    }

afterRead s = Context
  { tag = Write
  , args = [s]
  , cont = afterWrite }

afterWrite _ = Context
  { tag = Exit
  , args = ["0"]
  , cont = error "impossible!" }


test = kernel [ (main, "") ]

