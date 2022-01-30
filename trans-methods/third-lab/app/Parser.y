{
module Parser where
import Lexer
}

%name parse
%tokentype { Token }
%error { parserError }

%token

        'class'  { TClass }
        'static' { TStatic }
        'final'  { TFinal }

        'private' { TPrivate }
        'public'  { TPublic }
        'protected' { TProtected }

        'void' { TVoid }
        'double' { TDouble }
        'float' { TFloat }
        'int'  { TInt  }
        'long' { TLong }
        'byte' { TByte }
        'char'  { TChar }
        'short' { TShort }
        'boolean' { TBoolean }

        '+' { TPlus }
        '-' { TSub  }
        '*' { TMul  }
        '/' { TDiv  }

        'true' { TTrue }
        'false' { TFalse }

        cst { TConst $$}

        '>'    { TGT }
        '<'    { TLT }
        '<='   { TLQ }
        '>='   { TGQ }
        '=='   { TEQ }
        '!='   { TNEQ }

        'if'   { TIf }
        'else' { TElse }

        '(' { TRoundOpen }
        ')' { TRoundClose }
        '{' { TCurlyOpen }
        '}' { TCurlyClose }

        'return' { TReturn }

        ';'   { TSemicolon }
        ','   { TComa      }

        '='   { TAssign    }
        var   { TVar $$    }
%%

Class:
  ClassDeclaration Functions         { PClass $1 $2 }

ClassDeclaration:
  Modifiers 'class' var              { PClassDeclaration $1 (Var $3) }

Functions:
  '{' FunctionList { $2 }

FunctionList:
    FunctionList Function { $2 : $1 }
  | FunctionList '}'      { $1     }
  | Function               { [$1]  }
  | {- empty -}         { []    }

FunctionDeclaration:
  Modifiers Type var Parameters  { PFunctionDeclaration $1 $2 (Var $3) $4 }

Parameters:
  '(' ParametersList        { $2 }

ParametersList:
    ParametersList ',' Parameter                { $3 : $1 }
  | ParametersList ')'                          { $1      }
  | Parameter                                   { [$1]    }
  | {- empty -}                                 { []      }

Parameter:
  Type var                             { Parameter $1 (Var $2) }

Function:
  FunctionDeclaration Statements       { PFunction $1 $2 }

Statements:
  '{' StatementsList      { $2 }

StatementsList:
    StatementsList Statement     { $2 : $1 }
  | StatementsList           '}' { $1      }
  | Statement                    { [$1]    }
  | {- empty -}                  { []      }

Statement:
    AssignStatement     { $1 }
  | IfStatement         { $1 }
  | ReturnStatement     { $1 }

AssignStatement:
  Type var '=' Expression ';'      { PAssign $1 (Var $2) $4 }

ReturnStatement:
  'return' Expression ';'           { PReturn $2 }

IfStatement:
    'if' '(' BooleanExpression ')' Statements                         { IfExpr $3 $5 Nothing    }
  | 'if' '(' BooleanExpression ')' Statements 'else'  Statements      { IfExpr $3 $5 (Just $7) }

BooleanExpression:
    'true'        { PTrue  }
  | 'false'       { PFalse }
  | Expression '>' Expression     { PGT  $1 $3  }
  | Expression '>=' Expression    { PGQ  $1 $3  }
  | Expression '<' Expression     { PLT  $1 $3  }
  | Expression '<=' Expression    { PLQ  $1 $3  }
  | Expression '==' Expression    { PEQ  $1 $3  }
  | Expression '!=' Expression    { PNEQ $1 $3  }

Expression:
    cst                           { PConst $1 }
  | var                           { VarExpr (Var $1) }
  | Expression '+' Expression     { Add $1 $3 }
  | Expression '-' Expression     { Sub $1 $3 }
  | Expression '*' Expression     { Mul $1 $3 }
  | Expression '/' Expression     { Div $1 $3 }


Type:
      'int'     { PInt     }
    | 'boolean' { PBoolean }
    | 'char'    { PChar    }
    | 'long'    { PLong    }
    | 'byte'    { PByte    }
    | 'short'   { PShort   }
    | 'double'  { PDouble  }
    | 'float'   { PFloat   }
    | 'void'    { PVoid    }


Modifiers:
    Modifiers Modifier  { $2 : $1 }
  | Modifiers           { $1      }
  | {- empty -}         { []      }

Modifier:
    'public'    { Public    }
  | 'private'   { Private   }
  | 'protected' { Protected }
  | 'final'     { Final     }
  | 'static'    { Static    }

{

parserError :: [Token] -> a
parserError _ = error "Parse error"

data Modifier = Public
              | Private
              | Protected
              | Static
              | Final
               deriving (Eq)

data Type = PInt
          | PBoolean
          | PChar
          | PLong
          | PByte
          | PShort
          | PDouble
          | PFloat
          | PVoid
               deriving (Eq)

data Statement  = PAssign Type Var Expression
                | PReturn Expression
                | IfExpr BooleanExpr [Statement] (Maybe [Statement])               deriving (Eq)

data Var = Var String deriving (Eq)

data Expression = PConst Integer
                | VarExpr Var
                | Add Expression Expression
                | Div Expression Expression
                | Sub Expression Expression
                | Mul Expression Expression
               deriving (Eq)

data BooleanExpr =
    PTrue
  | PFalse
  | PEQ  Expression Expression
  | PNEQ Expression Expression
  | PGT  Expression Expression
  | PGQ  Expression Expression
  | PLT  Expression Expression
  | PLQ  Expression Expression
              deriving (Eq)


data Parameter = Parameter Type Var               deriving (Eq)


data PClassDeclaration = PClassDeclaration [Modifier] Var               deriving (Eq)


data PClass = PClass PClassDeclaration [PFunction]               deriving (Eq)


data PFunctionDeclaration = PFunctionDeclaration [Modifier] Type Var [Parameter]               deriving (Eq)


data PFunction = PFunction PFunctionDeclaration [Statement]               deriving (Eq)

}
