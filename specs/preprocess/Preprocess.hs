--------------------------------------------------------------------------------
-- 
-- Program     :  Preprocess
-- Copyright   :  (c) Sven Panne 2003
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- The .spec files from the SI are normally processed by Perl/AWK scripts and
-- have  therefore a rather ugly line-oriented syntax. To make things more
-- amenable to "real" parsing, some lexical preprocessing is useful. Note that
-- the following algorithm doesn't remove or insert lines, which is important
-- for good error messages later. After this preprocessing, whitespace is not
-- significant anymore, apart from its common use as a token separator.
-- 
-- For every line do:
-- 
--   1) Remove comments: Remove everything starting at the first '#'.
-- 
--   2) Ignore passthru-hack: Consider lines starting with 'passthru:' as empty.
-- 
--   3) Remove trailing whitespace.
-- 
--   4) Mangle property declarations: Append ';' to a line where the first ':'
--      is only preceded by non-TAB and non-SPC characters. Additionally, move
--      that ':' to the beginning of the line.
-- 
--   5) Separate definitions: Append ',' to a line starting with TAB and
--      followed (ignoring empty lines) by a line starting with TAB.
-- 
--   6) Terminate definitions: Append ';' to a line starting with TAB and not
--      followed (ignoring empty lines) by a line starting with TAB.
-- 
--------------------------------------------------------------------------------

module Main ( main ) where

import Control.Monad      ( liftM )
import Data.Char          ( isSpace )
import Data.List          ( isPrefixOf, tails )
import System.Environment ( getArgs )

--------------------------------------------------------------------------------
-- Preprocessing of spec files, making it more amenable to "real" parsing
--------------------------------------------------------------------------------

preprocess :: String -> String
preprocess = unlines .
             addSeparators . mangleColonLines .
             removeTrailingWhitespace . removePassthru . removeComments .
             lines

   where removeComments = map $ takeWhile (/= '#')
         removePassthru = map $ \l -> if "passthru:" `isPrefixOf` l then "" else l
         removeTrailingWhitespace = map $ reverse . dropWhile isSpace . reverse
         mangleColonLines = map $ \l ->
            case break (== ':') l of
               (xs, ':':ys) | noSpaceIn xs -> ":" ++ xs ++ " " ++ ys ++ ";"
               _ -> l
         noSpaceIn = not . any (`elem` ['\t',' '])

         addSeparators = map addSeparator . tails

         addSeparator []                                  = []
         addSeparator xs@(l:ls) | startsWithTabbedLine xs = l ++ separatorFor ls
                                | otherwise               = l 

         separatorFor ls | startsWithTabbedLine (dropEmpty ls) = ","
                         | otherwise                           = ";"

         dropEmpty = dropWhile ((== 0) . length)

         startsWithTabbedLine (('\t':_):_) = True
         startsWithTabbedLine _            = False

--------------------------------------------------------------------------------
-- The driver
--------------------------------------------------------------------------------

-- behave like 'cat'
mainWithArgs :: [String] -> IO ()
mainWithArgs fileNames = putStr . preprocess =<< input
   where input | null fileNames = getContents
               | otherwise      = liftM concat (mapM readFile fileNames)

main :: IO ()
main = getArgs >>= mainWithArgs
