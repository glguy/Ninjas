#!/usr/bin/env runhaskell
import System.Environment
import System.Exit
import System.IO
import System.Directory
import System.FilePath
import Data.Char
import Data.List

main :: IO ()
main =
  do as <- getArgs
     case as of
       [file,start] | [(s,cs)] <- reads start, all isSpace cs -> bump file s
       _ -> do p <- takeFileName `fmap` getProgName
               hPutStrLn stderr $
                 unwords [ "Usage", p, "FILE", "AMOUNT (+ve or -ve)" ]
               exitFailure


bump :: FilePath -> Int -> IO ()
bump file d
  | Just (pref,i,ext) <- fileParts name =
     do fs <- getDirectoryContents dir
        let is = sort [ x | f <- fs, Just (p,x,t) <- [ fileParts f ],
                                            x >= i, p == pref, t == ext ]

            mkName x = dir </> (pref ++ show x) <.> ext
        mapM_ (\a -> renameFile (mkName a) (mkName (a + d)))
              (if d > 0 then reverse is else is)
  | otherwise = fail $ "The file " ++ show file ++
                        "does not have a numeric suffix"
  where
  dir   = takeDirectory file
  name  = takeFileName file

-- Split somePrefixN.ext into (somePrefix,N,.ext)
fileParts :: FilePath -> Maybe (String,Int,String)
fileParts f | [(n,_)] <- reads (reverse rnums) = Just (reverse rf, n, ext)
            | otherwise                        = Nothing
  where
  (p1,ext) = splitExtension f
  (rnums,rf) = span isDigit (reverse p1)


