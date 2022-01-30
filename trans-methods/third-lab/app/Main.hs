module Main where

import Lexer
import Parser

class RecShow a where
  recShow :: a -> Int -> String

tab :: String
tab = "&nbsp;&nbsp;&nbsp;&nbsp;"

duplicateTab :: Int -> String
duplicateTab n = concat $ replicate n tab

showModifiers :: [Modifier] -> String
showModifiers = foldl (\acc m -> show m ++ " " ++ acc) ""

instance Show Modifier where
  show Public = "<span style=\"color:orange;\">public</span>"
  show Private = "<span style=\"color:orange;\">private</span>"
  show Protected = "<span style=\"color:orange;\">protected</span>"
  show Static = "<span style=\"color:orange;\">static</span>"
  show Final = "<span style=\"color:orange;\">final</span>"

instance Show PClassDeclaration where
  show (PClassDeclaration modifiers (Var x)) =
    showModifiers modifiers ++ "<span style=\"color:orange;\">class</span>" ++ " " ++ x

instance Show Type where
  show PInt =     "<span style=\"color:orange;\">int</span>"
  show PBoolean = "<span style=\"color:orange;\">boolean</span>"
  show PChar =    "<span style=\"color:orange;\">char</span>"
  show PLong =    "<span style=\"color:orange;\">long</span>"
  show PByte =    "<span style=\"color:orange;\">byte</span>"
  show PShort =    "<span style=\"color:orange;\">short</span>"
  show PDouble = "<span style=\"color:orange;\">double</span>"
  show PFloat = "<span style=\"color:orange;\">float</span>"
  show PVoid = "<span style=\"color:orange;\">void</span>"

instance Show Expression where
  show (PConst v) = show v
  show (VarExpr (Var x)) = x
  show (Add expr1 expr2) = show expr1 ++ " + " ++ show expr2
  show (Div expr1 expr2) = show expr1 ++ " / " ++ show expr2
  show (Sub expr1 expr2) = show expr1 ++ " - " ++ show expr2
  show (Mul expr1 expr2) = show expr1 ++ " * " ++ show expr2

instance Show BooleanExpr where
  show PTrue =  "<span style=\"color:orange;\">true</span>"
  show PFalse = "<span style=\"color:orange;\">false</span>"
  show (PEQ e1 e2) = show e1 ++ " == " ++ show e2
  show (PNEQ e1 e2) = show e1 ++ " != " ++ show e2
  show (PGT e1 e2) = show e1 ++ " > " ++ show e2
  show (PLT e1 e2) = show e1 ++ " < " ++ show e2
  show (PGQ e1 e2) = show e1 ++ " >= " ++ show e2
  show (PLQ e1 e2) = show e1 ++ " <= " ++ show e2

instance RecShow Statement where
  recShow (PAssign tp (Var x) expr) recLevel = duplicateTab recLevel ++ show tp ++ " " ++ x ++ " = " ++ show expr ++ ";"
  recShow (PReturn expr) recLevel = duplicateTab recLevel ++ "<span style=\"color:orange;\">return</span>" ++ " " ++ show expr ++ ";"
  recShow (IfExpr bExpr statements (Just elseStatements)) recLevel =
    duplicateTab recLevel ++ "<span style=\"color:orange;\">if</span>" ++ " (" ++ show bExpr ++ ") {<br>" ++ foldl (\acc s -> recShow s (recLevel + 1) ++ "<br>" ++ acc) "" statements ++ (duplicateTab recLevel) ++ "} " ++ "<span style=\"color:orange;\">else</span>" ++ " {<br>" ++ foldl (\acc s -> recShow s (recLevel + 1) ++ "<br>" ++ acc) "" elseStatements ++ (duplicateTab recLevel) ++ "}"
  recShow (IfExpr bExpr statements Nothing) recLevel =
    duplicateTab recLevel ++ "<span style=\"color:orange;\">if</span>" ++ " (" ++ show bExpr ++ ") {<br>" ++ foldl (\acc s -> recShow s (recLevel + 1) ++ "<br>" ++ acc) "" statements ++ (duplicateTab recLevel) ++ "}"

instance Show Parameter where
  show (Parameter tp (Var x)) = show tp ++ " " ++ x

instance Show PFunctionDeclaration where
  show (PFunctionDeclaration modifiers tp (Var x) parameters) =
    showModifiers modifiers ++ show tp ++ " " ++ x ++ "(" ++ foldr1 (\p acc -> acc ++ ", " ++ p) (map show parameters) ++ ")"

instance Show PFunction where
  show (PFunction decl statements) =
    tab ++ show decl ++ " {<br>" ++ foldl (\acc s -> recShow s 2 ++ "<br>" ++ acc) "" statements ++ tab ++ "}"

instance Show PClass where
  show (PClass declaration functions) =
    show declaration ++ " {<br>" ++ foldl (\acc f -> show f ++ "<br>" ++ acc) "" functions ++ "}"

main :: IO ()
main = do
  s <- readFile "input"
  writeFile "output.html" $ show $ parse $ alexScanTokens s
