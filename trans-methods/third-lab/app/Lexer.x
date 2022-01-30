{
module Lexer where
}

%wrapper "basic"

$digit=0-9
$alpha=[a-zA-Z]


tokens :-
  $white;
  \=                { \s -> TAssign           }
  \+                { \s -> TPlus             }
  \*                { \s -> TMul              }
  \/                { \s -> TDiv              }
  \-                { \s -> TSub              }
  \;                { \s -> TSemicolon        }
  \,                { \s -> TComa             }
  \>                { \s -> TGT               }
  \<                { \s -> TLT               }
  \<=               { \s -> TLQ               }
  \>=               { \s -> TGQ               }
  \==               { \s -> TEQ               }
  \!=               { \s -> TNEQ              }
  \class            { \s -> TClass            }
  \static           { \s -> TStatic           }
  \private          { \s -> TPrivate          }
  \public           { \s -> TPublic           }
  \protected        { \s -> TProtected        }
  \int              { \s -> TInt              }
  \char             { \s -> TChar             }
  \byte             { \s -> TByte             }
  \long             { \s -> TLong             }
  \double           { \s -> TDouble           }
  \float            { \s -> TFloat            }
  \boolean          { \s -> TBoolean          }
  "void"            { \s -> TVoid             }
  "final"           { \s -> TFinal            }
  "return"          { \s -> TReturn           }
  \if               { \s -> TIf               }
  \else             { \s -> TElse             }
  \true             { \s -> TTrue             }
  \false            { \s -> TFalse            }
  \(                { \s -> TRoundOpen        }
  \)                { \s -> TRoundClose       }
  \{                { \s -> TCurlyOpen        }
  \}                { \s -> TCurlyClose       }
  [$digit]+         { \s -> TConst (read s)   }
  $alpha+           { \s -> TVar s            }

{
data Token =
    TPublic
  | TPrivate
  | TProtected
  | TClass
  | TStatic
  | TFinal
  | TInt
  | TChar
  | TByte
  | TLong
  | TDouble
  | TFloat
  | TShort
  | TBoolean
  | TVoid
  | TReturn
  | TIf
  | TElse
  | TTrue
  | TFalse
  | TPlus
  | TSub
  | TMul
  | TDiv
  | TAssign
  | TRoundOpen
  | TRoundClose
  | TCurlyOpen
  | TCurlyClose
  | TConst Integer
  | TVar String
  | TSemicolon
  | TComa
  | TGT
  | TLT
  | TLQ
  | TGQ
  | TEQ
  | TNEQ
  deriving (Show, Eq)
}
