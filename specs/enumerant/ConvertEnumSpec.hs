--------------------------------------------------------------------------------
-- Converter for the transformation of OpenGL enumerant spec files into Haskell
-- data types, including (un)marshaling functions. The spec files are available
-- via SGI's OSS ("Open Source at SGI") anonymous CVS at the following CVS roots:
-- 
--    :pserver:cvs@oss.sgi.com:/cvs/projects/ogl-sample/main/doc/registry/specs
--    (the official registry)
-- 
-- and
-- 
--    :pserver:cvs@oss.sgi.com:/cvs/projects/ogl-sample/main/gfx/include/gl/spec
--    (the specs used in SGI's OpenGL sample implementation)
--------------------------------------------------------------------------------

module Main ( main ) where

-- UGLY, UGLY, UGLY! This should really die someday...
#if __GLASGOW_HASKELL__ >= 504
import Control.Monad       ( liftM, when )
import Control.Monad.State ( State, runState, evalState, get, put, modify )
import Data.Char           ( isUpper, toUpper, isLower, toLower, isDigit,
                             isHexDigit, isSpace )
import Data.FiniteMap      ( FiniteMap, emptyFM, addToFM_C, lookupWithDefaultFM )
import Data.List           ( mapAccumL, isPrefixOf, tails )
import Data.Set            ( Set, mkSet, addToSet, elementOf )
import Numeric             ( readHex, showHex )
import Text.ParserCombinators.Parsec
                           ( SourceName, Parser, (<|>), (<?>), try, eof, char,
                             string, many, many1, satisfy, option, parse, sepBy,
                             spaces )
import System.Environment  ( getArgs )
#else
import Monad               ( liftM, when )
import MonadState          ( State, runState, evalState, get, put, modify )
import Char                ( isUpper, toUpper, isLower, toLower, isDigit,
                             isHexDigit, isSpace )
import FiniteMap           ( FiniteMap, emptyFM, addToFM_C, lookupWithDefaultFM )
import List                ( mapAccumL, isPrefixOf, tails )
import Set                 ( Set, mkSet, addToSet, elementOf )
import Numeric             ( readHex )
import NumExts             ( showHex )
import Parsec              ( SourceName, Parser, (<|>), (<?>), try, eof, char,
                             string, many, many1, satisfy, option, parse, sepBy,
                             spaces )
import System              ( getArgs )
#endif

--------------------------------------------------------------------------------
-- We have two kinds of identifiers, the primary ones are less likely to be
-- renamed than the secondary ones.
--------------------------------------------------------------------------------

data Identifier = Identifier {
   nameOf      :: String,
   isPrimaryID :: Bool
 }

instance Eq Identifier where
   i1 == i2 = nameOf i1 == nameOf i2

instance Ord Identifier where
   i1 <= i2 = nameOf i1 <= nameOf i2

primaryID   :: String -> Identifier
primaryID   i = Identifier { nameOf = i, isPrimaryID = True }

secondaryID :: String -> Identifier
secondaryID i = Identifier { nameOf = i, isPrimaryID = False }

--------------------------------------------------------------------------------
-- The spec file's abstract syntax
--------------------------------------------------------------------------------

newtype Spec = Spec [TypeDefinition]

data TypeDefinition = TypeDefinition String TypeName Kind [Equation]

data Kind = Enum | Mask | Float | Define

newtype TypeName = TypeName String

data Equation =
     Use TypeName Identifier
   | Definition Identifier RHS

data RHS =
     NextValue
   | Value Number
   | Reference Identifier Number
   deriving Eq

-- NOTE: No floating point stuff yet
newtype Number = Number Integer deriving Eq

--------------------------------------------------------------------------------
-- Show instances for abstract syntax
--------------------------------------------------------------------------------

instance Show Spec where
   showsPrec _ (Spec defs) = showsDefs defs
      where showsDefs []     = id
            showsDefs [d]    = shows d
            showsDefs (d:ds) = shows d . showChar '\n'. showsDefs ds

instance Show TypeDefinition where
   showsPrec _ (TypeDefinition _ t k eqs) =
      shows t . showChar ' ' . shows k . showChar '\n' .
      foldr (\e f -> shows e . showChar '\n'. f) id eqs

instance Show Kind where
   showsPrec _ Enum   = showString "enum:"
   showsPrec _ Mask   = showString "mask:"
   showsPrec _ Float  = showString "floating:"
   showsPrec _ Define = showString "define:"

instance Show TypeName where
   showsPrec _ (TypeName t) = showString t

instance Show Equation where
   showsPrec _ (Definition i NextValue) = showChar '\t' . shows i
   showsPrec _ (Definition i r        ) = showChar '\t' . shows i .
                                          showString " = " . shows r
   showsPrec _ (Use t i               ) = showString "\tuse " . shows t .
                                          showChar ' ' . shows i

instance Show Identifier where
   showsPrec _ = showString . nameOf

instance Show RHS where
   showsPrec _ NextValue                = showString "<<next>>"
   showsPrec _ (Value n)                = shows n
   showsPrec _ (Reference i (Number 0)) = showChar '$' . shows i
   showsPrec _ (Reference i n         ) = showChar '$' . shows i .
                                          showString " + " . shows n

instance Show Number where
   showsPrec _ (Number n) = shows n

--------------------------------------------------------------------------------
-- Convenience stuff
--------------------------------------------------------------------------------

instance Num Number where
   Number x + Number y = Number (x + y)
   Number x - Number y = Number (x - y)
   Number x * Number y = Number (x * y)
   abs (Number x) = Number (abs x)
   signum (Number x) = Number (signum x)
   fromInteger = Number

--------------------------------------------------------------------------------
--
--------------------------------------------------------------------------------

magicComment :: String
magicComment = "# USE_PREFIX "

handleSpecialComments :: String -> String
handleSpecialComments = unlines . walk . lines
   where walk [] = []
         walk [l] = [l]
         walk (l1:rest1@(l2:rest2))
            | magicComment `isPrefixOf` l1 =
                 l1 : ("=" ++ drop (length magicComment) l1 ++ " " ++ l2) : walk rest2
            | otherwise = l1 : walk rest1

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
-- Parser for spec files
--------------------------------------------------------------------------------

spec :: Parser Spec
spec = do
   typeDefs <- many (typeDefinition `followedBy` symbol ";")
   spaces
   eof
   return (Spec typeDefs)

typeDefinition :: Parser TypeDefinition
typeDefinition = do
   p <- option "" (do symbol "=" ; word)
   t <- typeName
   k <- kind
   symbol ":"
   eqs <- equations
   return (TypeDefinition p t k eqs)

kind :: Parser Kind
kind =   (do symbol "enum"  ; return Enum  )
     <|> (do symbol "mask"  ; return Mask  )
     <|> (do symbol "float" ; return Float )
     <|> (do symbol "define"; return Define)

equations :: Parser [Equation]
equations = equation `sepBy` symbol ","

equation :: Parser Equation
equation = use <|> definition

use :: Parser Equation
use = do
   symbol "use"
   t <- typeName
   i <- identifier
   return (Use t i)

definition :: Parser Equation
definition = do
   ident <- identifier
   rhs <- option NextValue (do symbol "=" ; value)
   return (Definition ident rhs)

value :: Parser RHS
value = reference <|> (Value `liftM` number)

reference :: Parser RHS
reference = do
   symbol "$"
   ident <- identifier
   num <- option 0 (do symbol "+"; number)
   return (Reference ident num)

number :: Parser Number
number = Number `liftM` (try hexNumber <|> try decNumber) <?> "number"

hexNumber :: Parser Integer
hexNumber = do
   spaces
   char '0'
   char 'x' <|> char 'X'
   (fst . head . readHex) `liftM` many1 (satisfy isHexDigit)

decNumber :: Parser Integer
decNumber = read `liftM` do spaces; many1 (satisfy isDigit)

typeName :: Parser TypeName
typeName = TypeName `liftM` try word <?> "type name"

identifier :: Parser Identifier
identifier = primaryID `liftM` try word <?> "identifier"

word :: Parser String
word = do spaces ; many1 (satisfy (not . (\c -> isSpace c || c `elem` ";:,=+$")))

symbol :: String -> Parser ()
symbol s = try (do spaces; string s; return ()) <?> show s

followedBy :: Parser a -> Parser b -> Parser a
p `followedBy` q = do
   x <- p
   q
   return x

parseSpec :: SourceName -> String -> Spec
parseSpec fileName content =
   case parse spec fileName content of
      Left err -> error ("parse error at " ++ show err)
      Right s  -> s

--------------------------------------------------------------------------------
-- Expand default values, leaving Equations built only from Values and Reference.
--------------------------------------------------------------------------------

expandSpec :: Spec -> Spec
expandSpec (Spec defs) = Spec
   [ TypeDefinition p t k (snd (mapAccumL expandEquation (defaultValues k) eqs)) |
     TypeDefinition p t k eqs <- defs ]

expandEquation :: [Number] -> Equation -> ([Number], Equation)
expandEquation (d:ds) (Definition i NextValue) = (ds, Definition i (Value d))
expandEquation ds     eq                       = (ds, eq)

defaultValues :: Kind -> [Number]
defaultValues Enum   = iterate (+1) 0
defaultValues Mask   = iterate (*2) 1
defaultValues Float  = iterate id   0
defaultValues Define = iterate id   0

--------------------------------------------------------------------------------
-- Evaluate all references on the RHS of equations to plain numbers. Note that
-- this can be done in a single pass, because the identifiers must be defined
-- before they can be used. After this pass we are left with simple equations
-- of the form "ident = number" and "use" equations only.
--------------------------------------------------------------------------------

type Environment = FiniteMap Identifier Number

type Evaluator a b = a -> State Environment b

evaluate :: Spec -> (Spec, Environment)
evaluate s = runState (evalSpec s) emptyFM

evalSpec :: Evaluator Spec Spec
evalSpec (Spec defs) = Spec `liftM` mapM evalTypeDefinition defs

evalTypeDefinition :: Evaluator TypeDefinition TypeDefinition
evalTypeDefinition (TypeDefinition p t k eqs) =
   TypeDefinition p t k `liftM` mapM evalEquation eqs

evalEquation :: Evaluator Equation Equation
evalEquation (Definition ident rhs) = do
   n <- evalRHS rhs
   modify (updateEnvironment ident n)
   return $ Definition ident (Value n)
evalEquation u@(Use _ _) = return u

updateEnvironment :: Identifier -> Number -> Environment -> Environment
updateEnvironment ident n env = addToFM_C insert env ident n
   where insert old new | old == new = old
                        | otherwise  = error (nameOf ident ++ " redefined from "
                                              ++ show old ++ " to " ++ show new)

evalRHS :: Evaluator RHS Number
evalRHS NextValue       = error "Huh? Still default values left?"
evalRHS (Value v)       = return v
evalRHS (Reference i n) = (+ n) `liftM` evalIdentifier i

evalIdentifier :: Evaluator Identifier Number
evalIdentifier ident = do
   env <- get
   let msg = "reference: " ++ nameOf ident ++ " undefined"
   return $ lookupWithDefaultFM env (error msg) ident

--------------------------------------------------------------------------------
-- Abstract syntax for backend
--------------------------------------------------------------------------------

data SimpleSpec = SimpleSpec [SimpleTypeDefinition]

data SimpleTypeDefinition = SimpleTypeDefinition TypeName Kind [SimpleEquation]

data SimpleEquation = SimpleEquation Identifier Number

--------------------------------------------------------------------------------
-- Show instances for the backend's abstract syntax
--------------------------------------------------------------------------------

instance Show SimpleSpec where
   showsPrec _ (SimpleSpec defs) = showsDefs defs
      where showsDefs []     = id
            showsDefs [d]    = shows d
            showsDefs (d:ds) = shows d . showChar '\n'. showsDefs ds

instance Show SimpleTypeDefinition where
   showsPrec _ (SimpleTypeDefinition t k eqs) =
      shows t . showChar ' ' . shows k . showChar '\n' .
      foldr (\e f -> shows e . showChar '\n'. f) id eqs

instance Show SimpleEquation where
   showsPrec _ (SimpleEquation i n) =
      showChar '\t' . shows i. showString " = " . shows n

--------------------------------------------------------------------------------
-- Transform everything into the backend's abstract syntax, looking up "use"
-- equations and haskellizing names on the way.
--------------------------------------------------------------------------------

simplify :: (Spec, Environment) -> SimpleSpec
simplify (Spec defs, env) = SimpleSpec (map (simplifyTypeDefinition env) defs)

simplifyTypeDefinition :: Environment -> TypeDefinition -> SimpleTypeDefinition
simplifyTypeDefinition env (TypeDefinition prefix (TypeName t) k eqs) =
   SimpleTypeDefinition (TypeName (haskellize True t)) k
                        (map (simplifyEquation prefix (shouldBeCapitalized k) env) eqs)

simplifyEquation :: String -> Bool -> Environment -> Equation -> SimpleEquation
simplifyEquation prefix cap _ (Definition ident (Value n)) =
   SimpleEquation (primaryID (prefix ++ haskellize cap (nameOf ident))) n
simplifyEquation _ _ _ (Definition _ _) = error "Huh? Still non-value left?"
simplifyEquation prefix cap env (Use _ ident) =
   let msg = "use: " ++ nameOf ident ++ " undefined"
   in SimpleEquation (secondaryID (prefix ++ (haskellize cap (nameOf ident))))
                     (lookupWithDefaultFM env (error msg) ident)

shouldBeCapitalized :: Kind -> Bool
shouldBeCapitalized Enum   = True
shouldBeCapitalized Mask   = True
shouldBeCapitalized Float  = False
shouldBeCapitalized Define = False

--------------------------------------------------------------------------------
-- Identifier conversion a.k.a. "haskellization"
--------------------------------------------------------------------------------

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy _ [] = []
splitBy p xs = case break p xs of
                (ys, []  ) -> [ys]
                (ys, _:zs) -> ys : splitBy p zs

-- words which we don't want to change
specialWords :: [String]
specialWords = [
   "3DFX", "APPLE", "ARB", "EXT", "HP", "I3D", "IBM",       -- extensions
   "INGR", "INTEL", "MESA", "NV", "PGI", "SGI", "SGIS",
   "SGIX", "SUN", "SUNX",
   "CMYK", "CMYKA", "RGB", "BGR", "RGBA", "BGRA", "ABGR",   -- color specs
   "CW", "CCW",  "LSB"                                      -- misc
 ]

capitalizeWord :: String -> String
capitalizeWord cs@(x:xs)
   | all (\c -> isUpper c || isDigit c) cs ||
     all (\c -> isLower c || isDigit c) cs = toUpper x : map toLower xs
capitalizeWord cs = cs

-- capitalize every "non-special" word after a possible inital digit prefix if
-- it consists solely of upper case letters and digits
caseWord :: Bool -> String -> String
caseWord cap w | not (upperW `elem` specialWords) = digits ++ capitalizeWord rest
               | cap                              = upperW
               | otherwise                        = map toLower w
   where (digits, rest) = span isDigit w
         upperW = map toUpper w

caseWords :: Bool -> [String] -> String
caseWords _ []       = ""
caseWords cap (x:xs) = caseWord cap x ++ concatMap (caseWord True) xs

-- Haskell ids can't start with a digit, so spell it
spellDigit :: String -> String
spellDigit ""       = ""
spellDigit ('0':cs) = "Zero"  ++ cs
spellDigit ('1':cs) = "One"   ++ cs
spellDigit ('2':cs) = "Two"   ++ cs
spellDigit ('3':cs) = "Three" ++ cs
spellDigit ('4':cs) = "Four"  ++ cs
spellDigit ('5':cs) = "Five"  ++ cs
spellDigit ('6':cs) = "Six"   ++ cs
spellDigit ('7':cs) = "Seven" ++ cs
spellDigit ('8':cs) = "Eight" ++ cs
spellDigit ('9':cs) = "Nine"  ++ cs
spellDigit cs       =            cs

capitalize :: Bool -> String -> String
capitalize _     ""     = ""
capitalize False (c:cs) = toLower c : cs
capitalize True  (c:cs) = toUpper c : cs

haskellize :: Bool -> String -> String
haskellize cap =
   capitalize cap . spellDigit . caseWords cap . splitBy (== '_')

--------------------------------------------------------------------------------
-- Renaming is done in two passes: First only the primary identifiers are
-- renamed (which should actually only happen for predefined Haskell IDs), then
-- the second pass renames the secondary identifiers. This way the ticks are
-- appended to the "use" identifiers, not the ones which define their values, if
-- possible.
--------------------------------------------------------------------------------

type UsedIdentifiers = Set Identifier

type Renamer a = Bool -> a -> State UsedIdentifiers a

haskellIds :: UsedIdentifiers
haskellIds = mkSet (map primaryID [ "False", "True", "Left", "Right" ])

rename :: SimpleSpec -> SimpleSpec
rename s =
   evalState (renameSimpleSpec True s >>= renameSimpleSpec False) haskellIds

renameSimpleSpec :: Renamer SimpleSpec
renameSimpleSpec primary (SimpleSpec defs) =
   SimpleSpec `liftM` mapM (renameSimpleTypeDefinition primary) defs

renameSimpleTypeDefinition :: Renamer SimpleTypeDefinition
renameSimpleTypeDefinition primary (SimpleTypeDefinition t k eqs) =
   SimpleTypeDefinition t k `liftM` mapM (renameSimpleEquation primary) eqs

renameSimpleEquation :: Renamer SimpleEquation
renameSimpleEquation primary (SimpleEquation ident n) =
   flip SimpleEquation n `liftM` renameIdentifier primary ident

renameIdentifier :: Renamer Identifier
renameIdentifier primary ident
   | isPrimaryID ident == primary = do
      usedIdents <- get
      let renamedIdent = makeUnique usedIdents ident
      put (addToSet usedIdents renamedIdent)
      return renamedIdent
   | otherwise = return ident
   
makeUnique :: UsedIdentifiers -> Identifier -> Identifier
makeUnique usedIdents ident =
   head [ newId | newId <- candidatesFor ident,
                  not (newId `elementOf` usedIdents)]

candidatesFor :: Identifier -> [Identifier]
candidatesFor ident = zipWith tickify (repeat prefix) [tickCount ..]
   where (prefix, tickCount) = splitIdentifier ident

splitIdentifier :: Identifier -> (String, Int)
splitIdentifier ident =
   case break (== '\'') (nameOf ident) of
      (prefix, "")                            -> (prefix, 0)
      (prefix, "'")                           -> (prefix, 1)
      (prefix, _:suffix) | all isDigit suffix -> (prefix, read suffix)
                         | otherwise          -> (prefix, 0)

tickify :: String -> Int -> Identifier
tickify ident 0 = primaryID ident
tickify ident 1 = primaryID (ident ++ "'")
tickify ident n = primaryID (ident ++ ('\'' : show n))

--------------------------------------------------------------------------------
-- Code generation
--------------------------------------------------------------------------------

codeGen :: SimpleSpec -> String
codeGen (SimpleSpec defs) = foldr (.) id (map gen defs) ""
   where gen d@(SimpleTypeDefinition t _ _) =
            showSeparator . 
            showIfdef "IMPORT_" t (genSimpleTypeDef d) .
            showEOL

genSimpleTypeDef :: SimpleTypeDefinition -> ShowS
genSimpleTypeDef d@(SimpleTypeDefinition _ Enum   _) = genType   "GLenum"     d
genSimpleTypeDef d@(SimpleTypeDefinition _ Mask   _) = genType   "GLbitfield" d
genSimpleTypeDef d@(SimpleTypeDefinition _ Float  _) = genValues "GLfloat"    d
genSimpleTypeDef d@(SimpleTypeDefinition _ Define _) = genValues "GLenum"     d

genType :: String -> SimpleTypeDefinition -> ShowS
genType _           (SimpleTypeDefinition _ _ []) = id
genType haskellType simpleDef =
   genDataType simpleDef . showEOL .
   genMarshaler haskellType simpleDef . showEOL .
   genUnmarshaler haskellType simpleDef . showEOL

genDataType :: SimpleTypeDefinition -> ShowS
genDataType (SimpleTypeDefinition t _ eqs) =
   showString "data " . shows t . showString " =" . showEOL .
   showString "     " . c . showEOL .
   vcat [ showString "   | " . x | x <- cs ] .
   showString "   deriving ( Eq, Ord, Show )" . showEOL
   where (c:cs) = [ shows i | SimpleEquation i _ <- eqs ]

genMarshaler :: String -> SimpleTypeDefinition -> ShowS
genMarshaler haskellType (SimpleTypeDefinition t _ eqs) =
   showIfdef "IMPORT_marshal" t $
      showString "marshal" . shows t . showString " :: " . shows t .
      showString " -> " . showString haskellType . showEOL .
      showString "marshal" . shows t . showString " x = case x of" . showEOL .
      vcat [ showString "   " . shows i . showString " -> " . showHex n |
             SimpleEquation i (Number n) <- eqs ] .
      showEOL

genUnmarshaler :: String -> SimpleTypeDefinition -> ShowS
genUnmarshaler haskellType (SimpleTypeDefinition t _ eqs) =
   showIfdef "IMPORT_unmarshal" t $
      showString "unmarshal" . shows t . showString " :: " .
      showString haskellType . showString " -> " . shows t . showEOL .
      showString "unmarshal" . shows t . showString " x" . showEOL .
      vcat [ showString "   | x == " . showHex n . showString " = " . shows i |
             SimpleEquation i (Number n) <- eqs ] .
      showString "   | otherwise = error (\"unmarshal" . shows t .
      showString ": illegal value \" ++ show x)" . showEOL .
      showEOL

genValues :: String -> SimpleTypeDefinition -> ShowS
genValues _           (SimpleTypeDefinition _ _ [])  = id
genValues haskellType (SimpleTypeDefinition t _ eqs) =
   showString "-- " . shows t . showEOL .
   vcat [ showString ident . showString " :: " . showString haskellType . showEOL .
          showString ident . showString " = " . shows n |
          SimpleEquation i (Number n) <- eqs,
          let ident = nameOf i ] . showEOL

showIfdef :: String -> TypeName -> ShowS -> ShowS
showIfdef prefix tn wrappedStuff =
   showString "#ifdef HOPENGL_" . showString prefix . shows tn . showEOL .
   showEOL .
   wrappedStuff .
   showString "#endif" . showEOL

showSeparator :: ShowS
showSeparator =
   showString (replicate 80 '-') . showEOL .
   showEOL

vcat :: [ShowS] -> ShowS
vcat = foldr (\x -> (.) (x . showEOL)) id

showEOL :: ShowS
showEOL = showChar '\n'

--------------------------------------------------------------------------------
-- The driver
--------------------------------------------------------------------------------

parseArguments :: [String] -> (Bool, String, IO String)
parseArguments args =
   case restArgs of
      []   -> (verbose, "<stdin>", getContents)
      [fn] -> (verbose, fn, readFile fn)
      _    -> error "usage: ConvertEnumSpec [-v] [input.spec]"
   where (verbose, restArgs) = case args of
                                  ("-v":rest) -> (True,  rest)
                                  rest        -> (False, rest)

execute :: Bool -> String -> (b -> String) -> (a -> b) -> a -> IO b
execute verbose header showFn f x = do
   let result = f x
   when verbose $ do
      putStrLn ("-- " ++ header ++ "----------------------------------------")
      putStrLn (showFn result)
   return result

mainWithArgs :: [String] -> IO ()
mainWithArgs args = do
   let (verbose, fileName, getInput) = parseArguments args
       exec = execute verbose
   getInput                                                      >>=
      exec "special comments" id           handleSpecialComments >>=
      exec "preprocessing"    id           preprocess            >>=
      exec "parsing"          show         (parseSpec fileName)  >>=
      exec "expansion"        show         expandSpec            >>=
      exec "evaluation"       (show . fst) evaluate              >>=
      exec "simplify"         show         simplify              >>=
      exec "renaming"         show         rename                >>=
      putStr . codeGen

main :: IO ()
main = getArgs >>= mainWithArgs
