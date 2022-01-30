// Generated from /Users/maximgran/ITMO/trans-methods/fourth-lab-java/Meta.g4 by ANTLR 4.9.2

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

import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.TokenStream;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.misc.*;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast"})
public class MetaLexer extends Lexer {
	static { RuntimeMetaData.checkVersion("4.9.2", RuntimeMetaData.VERSION); }

	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		DOT=1, EXCL=2, SEMI=3, QUEST=4, AMP=5, EQ=6, FIG_OPEN=7, FIG_CLOSE=8, 
		COMMA=9, OPEN=10, CLOSE=11, IDENT=12, REGEX=13, RIGHT=14, WS=15, PIPE=16;
	public static String[] channelNames = {
		"DEFAULT_TOKEN_CHANNEL", "HIDDEN"
	};

	public static String[] modeNames = {
		"DEFAULT_MODE"
	};

	private static String[] makeRuleNames() {
		return new String[] {
			"DOT", "EXCL", "SEMI", "QUEST", "AMP", "EQ", "FIG_OPEN", "FIG_CLOSE", 
			"COMMA", "OPEN", "CLOSE", "IDENT", "REGEX", "RIGHT", "WS", "PIPE"
		};
	}
	public static final String[] ruleNames = makeRuleNames();

	private static String[] makeLiteralNames() {
		return new String[] {
			null, "':'", "'!'", "';'", "'?'", "'&'", "'='", "'{'", "'}'", "','", 
			"'['", "']'", null, null, null, null, "'|'"
		};
	}
	private static final String[] _LITERAL_NAMES = makeLiteralNames();
	private static String[] makeSymbolicNames() {
		return new String[] {
			null, "DOT", "EXCL", "SEMI", "QUEST", "AMP", "EQ", "FIG_OPEN", "FIG_CLOSE", 
			"COMMA", "OPEN", "CLOSE", "IDENT", "REGEX", "RIGHT", "WS", "PIPE"
		};
	}
	private static final String[] _SYMBOLIC_NAMES = makeSymbolicNames();
	public static final Vocabulary VOCABULARY = new VocabularyImpl(_LITERAL_NAMES, _SYMBOLIC_NAMES);

	/**
	 * @deprecated Use {@link #VOCABULARY} instead.
	 */
	@Deprecated
	public static final String[] tokenNames;
	static {
		tokenNames = new String[_SYMBOLIC_NAMES.length];
		for (int i = 0; i < tokenNames.length; i++) {
			tokenNames[i] = VOCABULARY.getLiteralName(i);
			if (tokenNames[i] == null) {
				tokenNames[i] = VOCABULARY.getSymbolicName(i);
			}

			if (tokenNames[i] == null) {
				tokenNames[i] = "<INVALID>";
			}
		}
	}

	@Override
	@Deprecated
	public String[] getTokenNames() {
		return tokenNames;
	}

	@Override

	public Vocabulary getVocabulary() {
		return VOCABULARY;
	}


	public MetaLexer(CharStream input) {
		super(input);
		_interp = new LexerATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}

	@Override
	public String getGrammarFileName() { return "Meta.g4"; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public String getSerializedATN() { return _serializedATN; }

	@Override
	public String[] getChannelNames() { return channelNames; }

	@Override
	public String[] getModeNames() { return modeNames; }

	@Override
	public ATN getATN() { return _ATN; }

	public static final String _serializedATN =
		"\3\u608b\ua72a\u8133\ub9ed\u417c\u3be7\u7786\u5964\2\22T\b\1\4\2\t\2\4"+
		"\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b\t\b\4\t\t\t\4\n\t\n\4\13\t"+
		"\13\4\f\t\f\4\r\t\r\4\16\t\16\4\17\t\17\4\20\t\20\4\21\t\21\3\2\3\2\3"+
		"\3\3\3\3\4\3\4\3\5\3\5\3\6\3\6\3\7\3\7\3\b\3\b\3\t\3\t\3\n\3\n\3\13\3"+
		"\13\3\f\3\f\3\r\6\r;\n\r\r\r\16\r<\3\16\3\16\6\16A\n\16\r\16\16\16B\3"+
		"\16\3\16\3\17\3\17\6\17I\n\17\r\17\16\17J\3\17\3\17\3\20\3\20\3\20\3\20"+
		"\3\21\3\21\2\2\22\3\3\5\4\7\5\t\6\13\7\r\b\17\t\21\n\23\13\25\f\27\r\31"+
		"\16\33\17\35\20\37\21!\22\3\2\6\7\2\60\60\62;C\\^^c|\3\2))\4\2))==\5\2"+
		"\13\f\17\17\"\"\2V\2\3\3\2\2\2\2\5\3\2\2\2\2\7\3\2\2\2\2\t\3\2\2\2\2\13"+
		"\3\2\2\2\2\r\3\2\2\2\2\17\3\2\2\2\2\21\3\2\2\2\2\23\3\2\2\2\2\25\3\2\2"+
		"\2\2\27\3\2\2\2\2\31\3\2\2\2\2\33\3\2\2\2\2\35\3\2\2\2\2\37\3\2\2\2\2"+
		"!\3\2\2\2\3#\3\2\2\2\5%\3\2\2\2\7\'\3\2\2\2\t)\3\2\2\2\13+\3\2\2\2\r-"+
		"\3\2\2\2\17/\3\2\2\2\21\61\3\2\2\2\23\63\3\2\2\2\25\65\3\2\2\2\27\67\3"+
		"\2\2\2\31:\3\2\2\2\33>\3\2\2\2\35F\3\2\2\2\37N\3\2\2\2!R\3\2\2\2#$\7<"+
		"\2\2$\4\3\2\2\2%&\7#\2\2&\6\3\2\2\2\'(\7=\2\2(\b\3\2\2\2)*\7A\2\2*\n\3"+
		"\2\2\2+,\7(\2\2,\f\3\2\2\2-.\7?\2\2.\16\3\2\2\2/\60\7}\2\2\60\20\3\2\2"+
		"\2\61\62\7\177\2\2\62\22\3\2\2\2\63\64\7.\2\2\64\24\3\2\2\2\65\66\7]\2"+
		"\2\66\26\3\2\2\2\678\7_\2\28\30\3\2\2\29;\t\2\2\2:9\3\2\2\2;<\3\2\2\2"+
		"<:\3\2\2\2<=\3\2\2\2=\32\3\2\2\2>@\7)\2\2?A\n\3\2\2@?\3\2\2\2AB\3\2\2"+
		"\2B@\3\2\2\2BC\3\2\2\2CD\3\2\2\2DE\7)\2\2E\34\3\2\2\2FH\7)\2\2GI\n\4\2"+
		"\2HG\3\2\2\2IJ\3\2\2\2JH\3\2\2\2JK\3\2\2\2KL\3\2\2\2LM\7)\2\2M\36\3\2"+
		"\2\2NO\t\5\2\2OP\3\2\2\2PQ\b\20\2\2Q \3\2\2\2RS\7~\2\2S\"\3\2\2\2\6\2"+
		"<BJ\3\b\2\2";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}