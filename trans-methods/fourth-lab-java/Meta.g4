grammar Meta;

@header {
  import java.util.List;
  import java.util.Map;
  import util.Terminal;
  import util.NonTerminal;
  import util.Block;
  import util.Import;
  import util.NonTerminal.JavaCell;
  import util.NonTerminal.TerminalTransition;
  import util.NonTerminal.StateTransition;
  import util.NonTerminal.Transition;
  import util.NonTerminal.SubTransition;
  import util.NonTerminal.AttributeWithType;
  import util.NonTerminal.SubRuleCell;
  import java.util.*;
}

result returns [List<Block> values, List<String> imports]
@init {$values = new ArrayList<>(); $imports = new ArrayList();}
  : (term[$values] SEMI)+
  ;

required [List<Block> values] returns []
  : REGEX {$values.add(new Import($REGEX.text.substring(1, $REGEX.text.length() - 1)));}
  ;

term [List<Block> values] returns []
  : EXCL terminal [$values]
  | AMP required [$values]
  | QUEST nonTerminal [$values]
  ;

terminal [List<Block> values] returns [Terminal t] @init {$t = new Terminal();}
  : IDENT{$t.name = $IDENT.text;} EQ pattern[]{$t.pattern = $pattern.p.substring(1, $pattern.p.length() - 1);} {$values.add($t);}
  ;

pattern [] returns [String p]
  : OPEN REGEX CLOSE {$p = $REGEX.text;}
  ;

nonTerminal [List<Block> values] returns [NonTerminal r] @init {$r = new NonTerminal();}
  : nonTerminalHeader[$r] (DOT nonTerminalPart[$r])+ {$values.add($r);}
  ;

nonTerminalHeader [NonTerminal r] returns []
  : declarations{$r.inherited = $declarations.d;} IDENT{$r.name = $IDENT.text;} declarations{$r.inner = $declarations.d;}
  ;


declarations returns [List<AttributeWithType> d] @init {$d = new ArrayList<>();}
  : OPEN (declaration{$d.add($declaration.attr);}
  | declaration{$d.add($declaration.attr);} COMMA)* CLOSE
  ;

declaration returns [AttributeWithType attr] @init {$attr = new AttributeWithType();}
  : REGEX{$attr.type = $REGEX.text.substring(1, $REGEX.text.length() - 1);} IDENT {$attr.name = $IDENT.text;}
  ;

nonTerminalPart [NonTerminal r] returns []
  : (nonTerminalPartBlock[$r]{$r.transitions.add($nonTerminalPartBlock.line);} | PIPE nonTerminalPartBlock[$r]{$r.transitions.add($nonTerminalPartBlock.line);})+
  ;

nonTerminalPartBlock [NonTerminal r] returns [Transition line, String name] @init {$line = new Transition();}
  : (REGEX {$line.add(new JavaCell($REGEX.text.substring(1, $REGEX.text.length() - 1)));}
  | nextStateCell[]{$line.add($nextStateCell.c);})+
  ;

nextStateCell [] returns [SubRuleCell c, boolean isTerminal, String name, StateTransition maybe] @init {$isTerminal = true;}
  : IDENT{$name=$IDENT.text;} (OPEN {$isTerminal = false; $maybe = new StateTransition($name);} ((IDENT{$maybe.addArg($IDENT.text);} COMMA)* IDENT{$maybe.addArg($IDENT.text);})* CLOSE)?
      {if ($isTerminal) {$c = new TerminalTransition($name);} else {$c = $maybe;}}
  ;

DOT : ':';
EXCL : '!';
SEMI : ';';
QUEST : '?';
AMP : '&' ;
EQ : '=';
FIG_OPEN : '{';
FIG_CLOSE : '}';
COMMA : ',';
OPEN : '[';
CLOSE : ']';
IDENT : [a-zA-Z0-9\\.]+;
REGEX : '\''(~['])+'\'';
RIGHT : '\''(~[';])+'\'';
WS : [ \r\n\t] -> skip;
PIPE : '|';