module Main ( main ) where

import Control.Monad      ( liftM, when )
import Data.Char          ( isSpace )
import Data.FiniteMap     ( FiniteMap, emptyFM, addListToFM_C, elemFM, fmToList,
                            lookupWithDefaultFM )
import Data.List          ( isPrefixOf, tails, delete )
import System.Environment ( getArgs )
import Text.ParserCombinators.Parsec
                          ( SourceName, Parser, parse, try, eof, oneOf, noneOf,
                            string, (<|>), (<?>), option, skipMany, many, many1,
                            sepBy, between, chainl1 )

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
-- The function spec file's abstract syntax
--------------------------------------------------------------------------------

data Spec = Spec [PropertyName] [ValidProperty] [Category]

data ValidProperty = ValidProperty PropertyName PropertyValues

data PropertyValues =
     AnyValue
   | Values [PropertyValue]

data Category = Category (Maybe CategoryName) [FunctionDeclaration]

data FunctionDeclaration =
   FunctionDeclaration FunctionName [ParameterName] TypeName
                       [ParameterDeclaration] [FunctionProperty]

data ParameterDeclaration =
   ParameterDeclaration ParameterName ParameterType
                        (Maybe LengthDescriptor) [PropertyValue]

data ParameterType = ParameterType TypeName Direction TransferType

data Direction = In | Out | InOut

data TransferType = Array | Reference | Value

data LengthDescriptor = LengthDescriptor [IndexExpression]

data IndexExpression =
     Add IndexExpression IndexExpression
   | Sub IndexExpression IndexExpression
   | Mul IndexExpression IndexExpression
   | Div IndexExpression IndexExpression
   | Number Integer
   | Parameter ParameterName
   | CompSize [ParameterName]

data FunctionProperty = FunctionProperty PropertyName [MetaPropertyValue]

data MetaPropertyValue =
     AddAllPropertyValues
   | RemoveAllPropertyValues
   | AddPropertyValue PropertyValue
   | RemovePropertyValue PropertyValue
   deriving Eq

newtype PropertyValue = PropertyValue String   deriving Eq

newtype PropertyName  = PropertyName  String   deriving (Eq, Ord)
newtype CategoryName  = CategoryName  String
newtype FunctionName  = FunctionName  String
newtype TypeName      = TypeName      String
newtype ParameterName = ParameterName String

--------------------------------------------------------------------------------
-- Show instances for abstract syntax
--------------------------------------------------------------------------------

instance Show Spec where
   showsPrec _ (Spec reqProps validProps categories) =
      punctuate (showString "\n\n")
                (vcat (showReq reqProps ++ map shows validProps) :
                 map shows categories)
      where showReq [] = []
            showReq ps = [ hsep (showString "required-props:" : map shows ps) ]

instance Show ValidProperty where
   showsPrec _ (ValidProperty name values) =
      shows name . showChar ':' . showChar ' ' . shows values

instance Show PropertyValues where
   showsPrec _ AnyValue    = showChar '*'
   showsPrec _ (Values vs) = hsep (map shows vs)

instance Show Category where
   showsPrec _ (Category Nothing     decls) = vcat (map shows decls)
   showsPrec _ (Category (Just name) decls) =
      vcat [ showString "newcategory: " . shows name,
             vcat (map shows decls),
             showString "endcategory:" ]

instance Show FunctionDeclaration where
   showsPrec _ (FunctionDeclaration name params retType paramDecls props) =
      vcat ([ shows name . parens (punctuate (showString ", ") 
                                             (map shows params)),
              showString "\treturn " . shows retType ] ++
            map shows paramDecls ++
            map shows props)

instance Show ParameterDeclaration where
   showsPrec _ (ParameterDeclaration name typ mbLen vals) =
      hsep [ showString "\tparam", shows name, shows typ, showLen mbLen,
             hsep (map shows vals) ]
      where showLen Nothing   = showString ""
            showLen (Just ld) = brackets (shows ld)

instance Show ParameterType where
   showsPrec _ (ParameterType name dir trans) =
      hsep [ shows name, shows dir, shows trans ]

instance Show Direction where
   showsPrec _ In    = showString "in"
   showsPrec _ Out   = showString "out"
   showsPrec _ InOut = showString "in/out"

instance Show TransferType where
   showsPrec _ Array     = showString "array"
   showsPrec _ Reference = showString "reference"
   showsPrec _ Value     = showString "value"

instance Show LengthDescriptor where
   showsPrec _ (LengthDescriptor exprs) =
      punctuate (showString ", ") (map shows exprs)

instance Show IndexExpression where
    showsPrec _ (Add l r)        = parens (shows l . showString " + " . shows r)
    showsPrec _ (Sub l r)        = parens (shows l . showString " - " . shows r)
    showsPrec _ (Mul l r)        = parens (shows l . showString " * " . shows r)
    showsPrec _ (Div l r)        = parens (shows l . showString " / " . shows r)
    showsPrec _ (Number n)       = shows n
    showsPrec _ (Parameter p)    = shows p
    showsPrec _ (CompSize names) =
       showString "COMPSIZE" .
       parens (punctuate (showChar '/') (map shows names))

instance Show FunctionProperty where
   showsPrec _ (FunctionProperty name metaProps) =
      showChar '\t' . hsep (shows name : map shows metaProps)

instance Show MetaPropertyValue where
    showsPrec _ AddAllPropertyValues    = showString "all"
    showsPrec _ RemoveAllPropertyValues = showString "! all"
    showsPrec _ (AddPropertyValue v)    = shows v
    showsPrec _ (RemovePropertyValue v) = showString "! " . shows v

instance Show PropertyValue where
   showsPrec _ (PropertyValue v) = showString v

instance Show PropertyName where
   showsPrec _ (PropertyName n) = showString n

instance Show CategoryName where
   showsPrec _ (CategoryName n) = showString n

instance Show FunctionName where
   showsPrec _ (FunctionName n) = showString n

instance Show TypeName where
   showsPrec _ (TypeName n) = showString n

instance Show ParameterName where
   showsPrec _ (ParameterName n) = showString n

--------------------------------------------------------------------------------
-- Helper functions for Show instances
--------------------------------------------------------------------------------

hsep :: [ShowS] -> ShowS
hsep = punctuate (showChar ' ')

vcat :: [ShowS] -> ShowS
vcat = punctuate (showChar '\n')

punctuate :: ShowS -> [ShowS] -> ShowS
punctuate _ [] = id
punctuate p xs = foldr1 (\l r -> l . p . r) xs

parens :: ShowS -> ShowS
parens s = showChar '(' . s . showChar ')'

brackets :: ShowS -> ShowS
brackets s = showChar '[' . s . showChar ']'

--------------------------------------------------------------------------------
-- Parser for function spec files
--------------------------------------------------------------------------------

spec :: Parser Spec
spec = do
   reqProps   <- option [] requiredProperties
   validProps <- many validProperty
   categories <- many category
   spaces
   eof
   return $ Spec reqProps validProps categories

requiredProperties :: Parser [PropertyName]
requiredProperties =
   between (symbol ":required-props") semi (many propertyName)

validProperty :: Parser ValidProperty
validProperty = do
   symbol ":"
   name   <- validPropertyName
   values <- validPropertyValues
   semi
   return $ ValidProperty name values

validPropertyName :: Parser PropertyName
validPropertyName =
       (do symbol "param"; return $ PropertyName "param")
   <|> propertyName

validPropertyValues :: Parser PropertyValues
validPropertyValues =
   option (Values []) (    (do symbol "*"; return AnyValue)
                       <|> liftM Values (many1 propertyValue))

category :: Parser Category
category = do
       (do decl <- functionDeclaration
           return $ Category Nothing [decl])
   <|> do cat <- newCategory
          funcDecls <- many functionDeclaration
          endCategory
          return $ Category (Just cat) funcDecls

newCategory :: Parser CategoryName
newCategory =
   between (symbol ":newcategory") semi categoryName

endCategory :: Parser ()
endCategory = do
   symbol ":endcategory"
   semi

functionDeclaration :: Parser FunctionDeclaration
functionDeclaration = do
  name <- functionName
  params <- parameters
  retType <- returnType
  (paramDecls, props) <- option ([], []) (do comma; paramsAndProps)
  semi
  return $ FunctionDeclaration name params retType paramDecls props

parameters :: Parser [ParameterName]
parameters = inParens (parameterName `sepBy` comma)

returnType :: Parser TypeName
returnType = do
   symbol "return"
   typeName

paramsAndProps :: Parser ([ParameterDeclaration],[FunctionProperty])
paramsAndProps = do
       (do param <- parameterDeclaration
           (params, props) <- option ([], []) (do comma; paramsAndProps)
           return (param:params, props))
   <|> (do props <- functionProperty `sepBy` comma
           return ([], props))

parameterDeclaration :: Parser ParameterDeclaration
parameterDeclaration = do
   symbol "param"
   name <- parameterName
   typ  <- parameterType
   len  <- option Nothing (liftM Just lengthDescriptor)
   vals <- many propertyValue
   return $ ParameterDeclaration name typ len vals

parameterType :: Parser ParameterType
parameterType = do
    name  <- typeName
    dir   <- direction
    trans <- transferType
    return $ ParameterType name dir trans

direction :: Parser Direction
direction =
       (do symbol "in";     return In   )
   <|> (do symbol "out";    return Out  )
   <|> (do symbol "in/out"; return InOut)

transferType :: Parser TransferType
transferType =
       (do symbol "array";     return Array    )
   <|> (do symbol "reference"; return Reference)
   <|> (do symbol "value";     return Value    )

lengthDescriptor :: Parser LengthDescriptor
lengthDescriptor =
   inBrackets (liftM LengthDescriptor (indexExpression `sepBy` comma))

indexExpression :: Parser IndexExpression
indexExpression = term `chainl1` addOp

addOp :: Parser (IndexExpression -> IndexExpression -> IndexExpression)
addOp =
       (do symbol "+"; return Add)
   <|> (do symbol "-"; return Sub)

term :: Parser IndexExpression
term = factor `chainl1` mulOp

mulOp :: Parser (IndexExpression -> IndexExpression -> IndexExpression)
mulOp =
       (do symbol "*"; return Mul)
   <|> (do symbol "/"; return Div)

factor :: Parser IndexExpression
factor =
       try compsize
   <|> inParens indexExpression
   <|> liftM Number integer
   <|> liftM Parameter parameterName

compsize :: Parser IndexExpression
compsize = do
   symbol "COMPSIZE"
   inParens (liftM CompSize (parameterName `sepBy` symbol "/"))

integer :: Parser Integer
integer = read `liftM` do spaces; many1 (oneOf "0123456789")

functionProperty :: Parser FunctionProperty
functionProperty = do
   name <- propertyName
   metaProps <- many metaPropertyValue
   return $ FunctionProperty name metaProps

metaPropertyValue :: Parser MetaPropertyValue
metaPropertyValue = do
   remove <- option False (do symbol "!"; return True)
   (    (do symbol "all"
            return $ if remove then RemoveAllPropertyValues
                               else AddAllPropertyValues)
    <|> liftM (if remove then RemovePropertyValue else AddPropertyValue)
              propertyValue)

propertyValue :: Parser PropertyValue
propertyValue = liftM PropertyValue word <?> "property value"

propertyName :: Parser PropertyName
propertyName = liftM PropertyName word <?> "property name"

categoryName :: Parser CategoryName
categoryName = liftM CategoryName word <?> "category name"

functionName :: Parser FunctionName
functionName = liftM FunctionName word <?> "function name"

typeName :: Parser TypeName
typeName = liftM TypeName word <?> "type name"

parameterName :: Parser ParameterName
parameterName = liftM ParameterName word <?> "parameter name"

word :: Parser String
word = try $ do
   spaces
   many1 (noneOf wordTerminators)

wordTerminators :: String
wordTerminators = spaceChars ++ specialChars

symbol :: String -> Parser ()
symbol s = try (do spaces; string s; return ()) <?> show s

inParens :: Parser a -> Parser a
inParens = between (symbol "(") (symbol ")")

inBrackets :: Parser a -> Parser a
inBrackets = between (symbol "[") (symbol "]")

semi :: Parser ()
semi = symbol ";"

comma :: Parser ()
comma = symbol ","

spaces :: Parser ()
spaces = skipMany (oneOf spaceChars) <?> "white space"

spaceChars :: [Char]
spaceChars = " \t\n\r\f\v\xa0"

specialChars :: [Char]
specialChars = "()[]:,;+*/!"

parseSpec :: SourceName -> String -> Spec
parseSpec fileName content =
   case parse spec fileName content of
      Left err -> error ("parse error at " ++ show err)
      Right s  -> s

--------------------------------------------------------------------------------
-- Calculate a mapping from property names to their corresponding domains,
-- doing checks for duplicate names/values on the way...
--------------------------------------------------------------------------------

type PropertyEnvironment = FiniteMap PropertyName PropertyValues

lookupProperty :: PropertyEnvironment -> PropertyName -> PropertyValues
lookupProperty env name =
   lookupWithDefaultFM env (error ("unknow property '" ++ show name ++ "'")) name

buildPropertyEnvironment :: Spec -> PropertyEnvironment
buildPropertyEnvironment spec_ =
   case noDupReqProps . noDupPropNames . noDupPropValues $ spec_ of
      Spec _ validProps _ ->
         addListToFM_C (\old _ -> error ("duplicate property name '"++ show old ++ "'"))
                       emptyFM
                       [(name,values) | ValidProperty name values <- validProps]

noDupReqProps :: Spec -> Spec
noDupReqProps spec_@(Spec reqProps _ _) =
   noDups spec_ "required property" reqProps

noDupPropNames :: Spec -> Spec
noDupPropNames spec_@(Spec _ validProps _) =
   noDups spec_ "property name" [ name | ValidProperty name _ <- validProps ]

noDupPropValues :: Spec -> Spec
noDupPropValues spec_@(Spec _ validProps _) =
   foldl (\spc (ValidProperty name values) ->
              noDups spc ("property value for '" ++ show name ++ "'")
                     [ v | Values vs <- [values], v <- vs ])
         spec_
         validProps

-- Simply return retVal if there are no duplicates in xs, otherwise complain.
noDups :: (Show b, Eq b) => a -> String -> [b] -> a
noDups retVal what xs = check xs
   where check []                   = retVal
         check (y:ys) | y `elem` ys = error ("duplicate "++what++": '"++ show y ++ "'")
                      | otherwise   = check ys

--------------------------------------------------------------------------------
-- Expand MetaPropertyValues so that only AddPropertyValues are left, stealthily
-- throwing away (required) property declarations and category names on the way.
-- Checks for required properties are done here, too.
--------------------------------------------------------------------------------

expandMetaProperties :: PropertyEnvironment -> Spec -> [FunctionDeclaration]
expandMetaProperties env (Spec reqProps _ categories) =
   checkRequiredPropsDecl env reqProps .
   map (checkRequiredPropsUse reqProps) $
   [ expandMetaPropertiesFuncDecl env funcDecl
   | Category _ funcDecls <- categories
   , funcDecl <- funcDecls ]

expandMetaPropertiesFuncDecl :: PropertyEnvironment
                             -> FunctionDeclaration -> FunctionDeclaration
expandMetaPropertiesFuncDecl env (FunctionDeclaration name params retType parmDecls props) =
   FunctionDeclaration
      name params retType parmDecls
      [ FunctionProperty pName (expandMetaPropertyValues values metaProps)
      | FunctionProperty pName metaProps <- props
      , let values = lookupProperty env pName ]

expandMetaPropertyValues :: PropertyValues -> [MetaPropertyValue] -> [MetaPropertyValue]
expandMetaPropertyValues values = foldl (go values) []
   where go AnyValue    _ AddAllPropertyValues = error "can't use all with *"
         go (Values vs) _ AddAllPropertyValues = map AddPropertyValue vs
         go _ _    RemoveAllPropertyValues     = []
         go _ accu v@(AddPropertyValue _)      = v : accu
         go _ accu (RemovePropertyValue v)     = delete (AddPropertyValue v) accu

checkRequiredPropsDecl :: PropertyEnvironment -> [PropertyName] -> a -> a
checkRequiredPropsDecl env reqProps retVal =
   case [ reqProp | reqProp <- reqProps, not (reqProp `elemFM` env) ] of
      []    -> retVal
      (p:_) -> error ("unknown required property '" ++ show p ++ "'")

checkRequiredPropsUse :: [PropertyName] -> FunctionDeclaration -> FunctionDeclaration
checkRequiredPropsUse reqProps f@(FunctionDeclaration funcName _ _ _ props) =
    case [ reqProp | reqProp <- reqProps, not (reqProp `elem` usedProps) ] of
       []    -> f
       (p:_) -> error ("function '" ++ show funcName ++
                       "' does not use required property '" ++ show p ++ "'")
    where usedProps = [ propName | FunctionProperty propName _ <- props ]

--------------------------------------------------------------------------------
-- The driver
--------------------------------------------------------------------------------

parseArguments :: [String] -> (Bool, String, IO String)
parseArguments args =
   case restArgs of
      []   -> (verbose, "<stdin>", getContents)
      [fn] -> (verbose, fn, readFile fn)
      _    -> error "usage: ConvertSpec [-v] [input.spec]"
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

-- TODO: Ugly!
mainWithArgs :: [String] -> IO ()
mainWithArgs args = do
   let (verbose, fileName, getInput) = parseArguments args
       exec = execute verbose
   input        <- getInput
   preprocInput <- exec "preprocessing" id preprocess input
   spec_        <- exec "parsing" show (parseSpec fileName) preprocInput
   propEnv      <- exec "building property environment" (unlines . map show . fmToList) buildPropertyEnvironment spec_
   expandedSpec <- exec "expanding properties" (unlines . map show) (expandMetaProperties propEnv) spec_
   return ()

main :: IO ()
main = getArgs >>= mainWithArgs
