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

import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.misc.*;
import org.antlr.v4.runtime.tree.*;
import java.util.List;
import java.util.Iterator;
import java.util.ArrayList;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast"})
public class MetaParser extends Parser {
	static { RuntimeMetaData.checkVersion("4.9.2", RuntimeMetaData.VERSION); }

	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		DOT=1, EXCL=2, SEMI=3, QUEST=4, AMP=5, EQ=6, FIG_OPEN=7, FIG_CLOSE=8, 
		COMMA=9, OPEN=10, CLOSE=11, IDENT=12, REGEX=13, RIGHT=14, WS=15, PIPE=16;
	public static final int
		RULE_result = 0, RULE_required = 1, RULE_term = 2, RULE_terminal = 3, 
		RULE_pattern = 4, RULE_nonTerminal = 5, RULE_nonTerminalHeader = 6, RULE_declarations = 7, 
		RULE_declaration = 8, RULE_nonTerminalPart = 9, RULE_nonTerminalPartBlock = 10, 
		RULE_nextStateCell = 11;
	private static String[] makeRuleNames() {
		return new String[] {
			"result", "required", "term", "terminal", "pattern", "nonTerminal", "nonTerminalHeader", 
			"declarations", "declaration", "nonTerminalPart", "nonTerminalPartBlock", 
			"nextStateCell"
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

	@Override
	public String getGrammarFileName() { return "Meta.g4"; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public String getSerializedATN() { return _serializedATN; }

	@Override
	public ATN getATN() { return _ATN; }

	public MetaParser(TokenStream input) {
		super(input);
		_interp = new ParserATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}

	public static class ResultContext extends ParserRuleContext {
		public List<Block> values;
		public List<String> imports;
		public List<TermContext> term() {
			return getRuleContexts(TermContext.class);
		}
		public TermContext term(int i) {
			return getRuleContext(TermContext.class,i);
		}
		public List<TerminalNode> SEMI() { return getTokens(MetaParser.SEMI); }
		public TerminalNode SEMI(int i) {
			return getToken(MetaParser.SEMI, i);
		}
		public ResultContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_result; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MetaListener ) ((MetaListener)listener).enterResult(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MetaListener ) ((MetaListener)listener).exitResult(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof MetaVisitor ) return ((MetaVisitor<? extends T>)visitor).visitResult(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ResultContext result() throws RecognitionException {
		ResultContext _localctx = new ResultContext(_ctx, getState());
		enterRule(_localctx, 0, RULE_result);
		((ResultContext)_localctx).values =  new ArrayList<>(); ((ResultContext)_localctx).imports =  new ArrayList();
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(27); 
			_errHandler.sync(this);
			_la = _input.LA(1);
			do {
				{
				{
				setState(24);
				term(_localctx.values);
				setState(25);
				match(SEMI);
				}
				}
				setState(29); 
				_errHandler.sync(this);
				_la = _input.LA(1);
			} while ( (((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << EXCL) | (1L << QUEST) | (1L << AMP))) != 0) );
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class RequiredContext extends ParserRuleContext {
		public List<Block> values;
		public Token REGEX;
		public TerminalNode REGEX() { return getToken(MetaParser.REGEX, 0); }
		public RequiredContext(ParserRuleContext parent, int invokingState) { super(parent, invokingState); }
		public RequiredContext(ParserRuleContext parent, int invokingState, List<Block> values) {
			super(parent, invokingState);
			this.values = values;
		}
		@Override public int getRuleIndex() { return RULE_required; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MetaListener ) ((MetaListener)listener).enterRequired(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MetaListener ) ((MetaListener)listener).exitRequired(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof MetaVisitor ) return ((MetaVisitor<? extends T>)visitor).visitRequired(this);
			else return visitor.visitChildren(this);
		}
	}

	public final RequiredContext required(List<Block> values) throws RecognitionException {
		RequiredContext _localctx = new RequiredContext(_ctx, getState(), values);
		enterRule(_localctx, 2, RULE_required);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(31);
			((RequiredContext)_localctx).REGEX = match(REGEX);
			_localctx.values.add(new Import((((RequiredContext)_localctx).REGEX!=null?((RequiredContext)_localctx).REGEX.getText():null).substring(1, (((RequiredContext)_localctx).REGEX!=null?((RequiredContext)_localctx).REGEX.getText():null).length() - 1)));
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class TermContext extends ParserRuleContext {
		public List<Block> values;
		public TerminalNode EXCL() { return getToken(MetaParser.EXCL, 0); }
		public TerminalContext terminal() {
			return getRuleContext(TerminalContext.class,0);
		}
		public TerminalNode AMP() { return getToken(MetaParser.AMP, 0); }
		public RequiredContext required() {
			return getRuleContext(RequiredContext.class,0);
		}
		public TerminalNode QUEST() { return getToken(MetaParser.QUEST, 0); }
		public NonTerminalContext nonTerminal() {
			return getRuleContext(NonTerminalContext.class,0);
		}
		public TermContext(ParserRuleContext parent, int invokingState) { super(parent, invokingState); }
		public TermContext(ParserRuleContext parent, int invokingState, List<Block> values) {
			super(parent, invokingState);
			this.values = values;
		}
		@Override public int getRuleIndex() { return RULE_term; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MetaListener ) ((MetaListener)listener).enterTerm(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MetaListener ) ((MetaListener)listener).exitTerm(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof MetaVisitor ) return ((MetaVisitor<? extends T>)visitor).visitTerm(this);
			else return visitor.visitChildren(this);
		}
	}

	public final TermContext term(List<Block> values) throws RecognitionException {
		TermContext _localctx = new TermContext(_ctx, getState(), values);
		enterRule(_localctx, 4, RULE_term);
		try {
			setState(40);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case EXCL:
				enterOuterAlt(_localctx, 1);
				{
				setState(34);
				match(EXCL);
				setState(35);
				terminal(_localctx.values);
				}
				break;
			case AMP:
				enterOuterAlt(_localctx, 2);
				{
				setState(36);
				match(AMP);
				setState(37);
				required(_localctx.values);
				}
				break;
			case QUEST:
				enterOuterAlt(_localctx, 3);
				{
				setState(38);
				match(QUEST);
				setState(39);
				nonTerminal(_localctx.values);
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class TerminalContext extends ParserRuleContext {
		public List<Block> values;
		public Terminal t;
		public Token IDENT;
		public PatternContext pattern;
		public TerminalNode IDENT() { return getToken(MetaParser.IDENT, 0); }
		public TerminalNode EQ() { return getToken(MetaParser.EQ, 0); }
		public PatternContext pattern() {
			return getRuleContext(PatternContext.class,0);
		}
		public TerminalContext(ParserRuleContext parent, int invokingState) { super(parent, invokingState); }
		public TerminalContext(ParserRuleContext parent, int invokingState, List<Block> values) {
			super(parent, invokingState);
			this.values = values;
		}
		@Override public int getRuleIndex() { return RULE_terminal; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MetaListener ) ((MetaListener)listener).enterTerminal(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MetaListener ) ((MetaListener)listener).exitTerminal(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof MetaVisitor ) return ((MetaVisitor<? extends T>)visitor).visitTerminal(this);
			else return visitor.visitChildren(this);
		}
	}

	public final TerminalContext terminal(List<Block> values) throws RecognitionException {
		TerminalContext _localctx = new TerminalContext(_ctx, getState(), values);
		enterRule(_localctx, 6, RULE_terminal);
		((TerminalContext)_localctx).t =  new Terminal();
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(42);
			((TerminalContext)_localctx).IDENT = match(IDENT);
			_localctx.t.name = (((TerminalContext)_localctx).IDENT!=null?((TerminalContext)_localctx).IDENT.getText():null);
			setState(44);
			match(EQ);
			setState(45);
			((TerminalContext)_localctx).pattern = pattern();
			_localctx.t.pattern = ((TerminalContext)_localctx).pattern.p.substring(1, ((TerminalContext)_localctx).pattern.p.length() - 1);
			_localctx.values.add(_localctx.t);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class PatternContext extends ParserRuleContext {
		public String p;
		public Token REGEX;
		public TerminalNode OPEN() { return getToken(MetaParser.OPEN, 0); }
		public TerminalNode REGEX() { return getToken(MetaParser.REGEX, 0); }
		public TerminalNode CLOSE() { return getToken(MetaParser.CLOSE, 0); }
		public PatternContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_pattern; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MetaListener ) ((MetaListener)listener).enterPattern(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MetaListener ) ((MetaListener)listener).exitPattern(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof MetaVisitor ) return ((MetaVisitor<? extends T>)visitor).visitPattern(this);
			else return visitor.visitChildren(this);
		}
	}

	public final PatternContext pattern() throws RecognitionException {
		PatternContext _localctx = new PatternContext(_ctx, getState());
		enterRule(_localctx, 8, RULE_pattern);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(49);
			match(OPEN);
			setState(50);
			((PatternContext)_localctx).REGEX = match(REGEX);
			setState(51);
			match(CLOSE);
			((PatternContext)_localctx).p =  (((PatternContext)_localctx).REGEX!=null?((PatternContext)_localctx).REGEX.getText():null);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class NonTerminalContext extends ParserRuleContext {
		public List<Block> values;
		public NonTerminal r;
		public NonTerminalHeaderContext nonTerminalHeader() {
			return getRuleContext(NonTerminalHeaderContext.class,0);
		}
		public List<TerminalNode> DOT() { return getTokens(MetaParser.DOT); }
		public TerminalNode DOT(int i) {
			return getToken(MetaParser.DOT, i);
		}
		public List<NonTerminalPartContext> nonTerminalPart() {
			return getRuleContexts(NonTerminalPartContext.class);
		}
		public NonTerminalPartContext nonTerminalPart(int i) {
			return getRuleContext(NonTerminalPartContext.class,i);
		}
		public NonTerminalContext(ParserRuleContext parent, int invokingState) { super(parent, invokingState); }
		public NonTerminalContext(ParserRuleContext parent, int invokingState, List<Block> values) {
			super(parent, invokingState);
			this.values = values;
		}
		@Override public int getRuleIndex() { return RULE_nonTerminal; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MetaListener ) ((MetaListener)listener).enterNonTerminal(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MetaListener ) ((MetaListener)listener).exitNonTerminal(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof MetaVisitor ) return ((MetaVisitor<? extends T>)visitor).visitNonTerminal(this);
			else return visitor.visitChildren(this);
		}
	}

	public final NonTerminalContext nonTerminal(List<Block> values) throws RecognitionException {
		NonTerminalContext _localctx = new NonTerminalContext(_ctx, getState(), values);
		enterRule(_localctx, 10, RULE_nonTerminal);
		((NonTerminalContext)_localctx).r =  new NonTerminal();
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(54);
			nonTerminalHeader(_localctx.r);
			setState(57); 
			_errHandler.sync(this);
			_la = _input.LA(1);
			do {
				{
				{
				setState(55);
				match(DOT);
				setState(56);
				nonTerminalPart(_localctx.r);
				}
				}
				setState(59); 
				_errHandler.sync(this);
				_la = _input.LA(1);
			} while ( _la==DOT );
			_localctx.values.add(_localctx.r);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class NonTerminalHeaderContext extends ParserRuleContext {
		public NonTerminal r;
		public DeclarationsContext declarations;
		public Token IDENT;
		public List<DeclarationsContext> declarations() {
			return getRuleContexts(DeclarationsContext.class);
		}
		public DeclarationsContext declarations(int i) {
			return getRuleContext(DeclarationsContext.class,i);
		}
		public TerminalNode IDENT() { return getToken(MetaParser.IDENT, 0); }
		public NonTerminalHeaderContext(ParserRuleContext parent, int invokingState) { super(parent, invokingState); }
		public NonTerminalHeaderContext(ParserRuleContext parent, int invokingState, NonTerminal r) {
			super(parent, invokingState);
			this.r = r;
		}
		@Override public int getRuleIndex() { return RULE_nonTerminalHeader; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MetaListener ) ((MetaListener)listener).enterNonTerminalHeader(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MetaListener ) ((MetaListener)listener).exitNonTerminalHeader(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof MetaVisitor ) return ((MetaVisitor<? extends T>)visitor).visitNonTerminalHeader(this);
			else return visitor.visitChildren(this);
		}
	}

	public final NonTerminalHeaderContext nonTerminalHeader(NonTerminal r) throws RecognitionException {
		NonTerminalHeaderContext _localctx = new NonTerminalHeaderContext(_ctx, getState(), r);
		enterRule(_localctx, 12, RULE_nonTerminalHeader);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(63);
			((NonTerminalHeaderContext)_localctx).declarations = declarations();
			_localctx.r.inherited = ((NonTerminalHeaderContext)_localctx).declarations.d;
			setState(65);
			((NonTerminalHeaderContext)_localctx).IDENT = match(IDENT);
			_localctx.r.name = (((NonTerminalHeaderContext)_localctx).IDENT!=null?((NonTerminalHeaderContext)_localctx).IDENT.getText():null);
			setState(67);
			((NonTerminalHeaderContext)_localctx).declarations = declarations();
			_localctx.r.inner = ((NonTerminalHeaderContext)_localctx).declarations.d;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class DeclarationsContext extends ParserRuleContext {
		public List<AttributeWithType> d;
		public DeclarationContext declaration;
		public TerminalNode OPEN() { return getToken(MetaParser.OPEN, 0); }
		public TerminalNode CLOSE() { return getToken(MetaParser.CLOSE, 0); }
		public List<DeclarationContext> declaration() {
			return getRuleContexts(DeclarationContext.class);
		}
		public DeclarationContext declaration(int i) {
			return getRuleContext(DeclarationContext.class,i);
		}
		public List<TerminalNode> COMMA() { return getTokens(MetaParser.COMMA); }
		public TerminalNode COMMA(int i) {
			return getToken(MetaParser.COMMA, i);
		}
		public DeclarationsContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_declarations; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MetaListener ) ((MetaListener)listener).enterDeclarations(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MetaListener ) ((MetaListener)listener).exitDeclarations(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof MetaVisitor ) return ((MetaVisitor<? extends T>)visitor).visitDeclarations(this);
			else return visitor.visitChildren(this);
		}
	}

	public final DeclarationsContext declarations() throws RecognitionException {
		DeclarationsContext _localctx = new DeclarationsContext(_ctx, getState());
		enterRule(_localctx, 14, RULE_declarations);
		((DeclarationsContext)_localctx).d =  new ArrayList<>();
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(70);
			match(OPEN);
			setState(80);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==REGEX) {
				{
				setState(78);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,3,_ctx) ) {
				case 1:
					{
					setState(71);
					((DeclarationsContext)_localctx).declaration = declaration();
					_localctx.d.add(((DeclarationsContext)_localctx).declaration.attr);
					}
					break;
				case 2:
					{
					setState(74);
					((DeclarationsContext)_localctx).declaration = declaration();
					_localctx.d.add(((DeclarationsContext)_localctx).declaration.attr);
					setState(76);
					match(COMMA);
					}
					break;
				}
				}
				setState(82);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(83);
			match(CLOSE);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class DeclarationContext extends ParserRuleContext {
		public AttributeWithType attr;
		public Token REGEX;
		public Token IDENT;
		public TerminalNode REGEX() { return getToken(MetaParser.REGEX, 0); }
		public TerminalNode IDENT() { return getToken(MetaParser.IDENT, 0); }
		public DeclarationContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_declaration; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MetaListener ) ((MetaListener)listener).enterDeclaration(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MetaListener ) ((MetaListener)listener).exitDeclaration(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof MetaVisitor ) return ((MetaVisitor<? extends T>)visitor).visitDeclaration(this);
			else return visitor.visitChildren(this);
		}
	}

	public final DeclarationContext declaration() throws RecognitionException {
		DeclarationContext _localctx = new DeclarationContext(_ctx, getState());
		enterRule(_localctx, 16, RULE_declaration);
		((DeclarationContext)_localctx).attr =  new AttributeWithType();
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(85);
			((DeclarationContext)_localctx).REGEX = match(REGEX);
			_localctx.attr.type = (((DeclarationContext)_localctx).REGEX!=null?((DeclarationContext)_localctx).REGEX.getText():null).substring(1, (((DeclarationContext)_localctx).REGEX!=null?((DeclarationContext)_localctx).REGEX.getText():null).length() - 1);
			setState(87);
			((DeclarationContext)_localctx).IDENT = match(IDENT);
			_localctx.attr.name = (((DeclarationContext)_localctx).IDENT!=null?((DeclarationContext)_localctx).IDENT.getText():null);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class NonTerminalPartContext extends ParserRuleContext {
		public NonTerminal r;
		public NonTerminalPartBlockContext nonTerminalPartBlock;
		public List<NonTerminalPartBlockContext> nonTerminalPartBlock() {
			return getRuleContexts(NonTerminalPartBlockContext.class);
		}
		public NonTerminalPartBlockContext nonTerminalPartBlock(int i) {
			return getRuleContext(NonTerminalPartBlockContext.class,i);
		}
		public List<TerminalNode> PIPE() { return getTokens(MetaParser.PIPE); }
		public TerminalNode PIPE(int i) {
			return getToken(MetaParser.PIPE, i);
		}
		public NonTerminalPartContext(ParserRuleContext parent, int invokingState) { super(parent, invokingState); }
		public NonTerminalPartContext(ParserRuleContext parent, int invokingState, NonTerminal r) {
			super(parent, invokingState);
			this.r = r;
		}
		@Override public int getRuleIndex() { return RULE_nonTerminalPart; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MetaListener ) ((MetaListener)listener).enterNonTerminalPart(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MetaListener ) ((MetaListener)listener).exitNonTerminalPart(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof MetaVisitor ) return ((MetaVisitor<? extends T>)visitor).visitNonTerminalPart(this);
			else return visitor.visitChildren(this);
		}
	}

	public final NonTerminalPartContext nonTerminalPart(NonTerminal r) throws RecognitionException {
		NonTerminalPartContext _localctx = new NonTerminalPartContext(_ctx, getState(), r);
		enterRule(_localctx, 18, RULE_nonTerminalPart);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(97); 
			_errHandler.sync(this);
			_la = _input.LA(1);
			do {
				{
				setState(97);
				_errHandler.sync(this);
				switch (_input.LA(1)) {
				case IDENT:
				case REGEX:
					{
					setState(90);
					((NonTerminalPartContext)_localctx).nonTerminalPartBlock = nonTerminalPartBlock(_localctx.r);
					_localctx.r.transitions.add(((NonTerminalPartContext)_localctx).nonTerminalPartBlock.line);
					}
					break;
				case PIPE:
					{
					setState(93);
					match(PIPE);
					setState(94);
					((NonTerminalPartContext)_localctx).nonTerminalPartBlock = nonTerminalPartBlock(_localctx.r);
					_localctx.r.transitions.add(((NonTerminalPartContext)_localctx).nonTerminalPartBlock.line);
					}
					break;
				default:
					throw new NoViableAltException(this);
				}
				}
				setState(99); 
				_errHandler.sync(this);
				_la = _input.LA(1);
			} while ( (((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << IDENT) | (1L << REGEX) | (1L << PIPE))) != 0) );
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class NonTerminalPartBlockContext extends ParserRuleContext {
		public NonTerminal r;
		public Transition line;
		public String name;
		public Token REGEX;
		public NextStateCellContext nextStateCell;
		public List<TerminalNode> REGEX() { return getTokens(MetaParser.REGEX); }
		public TerminalNode REGEX(int i) {
			return getToken(MetaParser.REGEX, i);
		}
		public List<NextStateCellContext> nextStateCell() {
			return getRuleContexts(NextStateCellContext.class);
		}
		public NextStateCellContext nextStateCell(int i) {
			return getRuleContext(NextStateCellContext.class,i);
		}
		public NonTerminalPartBlockContext(ParserRuleContext parent, int invokingState) { super(parent, invokingState); }
		public NonTerminalPartBlockContext(ParserRuleContext parent, int invokingState, NonTerminal r) {
			super(parent, invokingState);
			this.r = r;
		}
		@Override public int getRuleIndex() { return RULE_nonTerminalPartBlock; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MetaListener ) ((MetaListener)listener).enterNonTerminalPartBlock(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MetaListener ) ((MetaListener)listener).exitNonTerminalPartBlock(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof MetaVisitor ) return ((MetaVisitor<? extends T>)visitor).visitNonTerminalPartBlock(this);
			else return visitor.visitChildren(this);
		}
	}

	public final NonTerminalPartBlockContext nonTerminalPartBlock(NonTerminal r) throws RecognitionException {
		NonTerminalPartBlockContext _localctx = new NonTerminalPartBlockContext(_ctx, getState(), r);
		enterRule(_localctx, 20, RULE_nonTerminalPartBlock);
		((NonTerminalPartBlockContext)_localctx).line =  new Transition();
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(106); 
			_errHandler.sync(this);
			_alt = 1;
			do {
				switch (_alt) {
				case 1:
					{
					setState(106);
					_errHandler.sync(this);
					switch (_input.LA(1)) {
					case REGEX:
						{
						setState(101);
						((NonTerminalPartBlockContext)_localctx).REGEX = match(REGEX);
						_localctx.line.add(new JavaCell((((NonTerminalPartBlockContext)_localctx).REGEX!=null?((NonTerminalPartBlockContext)_localctx).REGEX.getText():null).substring(1, (((NonTerminalPartBlockContext)_localctx).REGEX!=null?((NonTerminalPartBlockContext)_localctx).REGEX.getText():null).length() - 1)));
						}
						break;
					case IDENT:
						{
						setState(103);
						((NonTerminalPartBlockContext)_localctx).nextStateCell = nextStateCell();
						_localctx.line.add(((NonTerminalPartBlockContext)_localctx).nextStateCell.c);
						}
						break;
					default:
						throw new NoViableAltException(this);
					}
					}
					break;
				default:
					throw new NoViableAltException(this);
				}
				setState(108); 
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,8,_ctx);
			} while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER );
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class NextStateCellContext extends ParserRuleContext {
		public SubRuleCell c;
		public boolean isTerminal;
		public String name;
		public StateTransition maybe;
		public Token IDENT;
		public List<TerminalNode> IDENT() { return getTokens(MetaParser.IDENT); }
		public TerminalNode IDENT(int i) {
			return getToken(MetaParser.IDENT, i);
		}
		public TerminalNode OPEN() { return getToken(MetaParser.OPEN, 0); }
		public TerminalNode CLOSE() { return getToken(MetaParser.CLOSE, 0); }
		public List<TerminalNode> COMMA() { return getTokens(MetaParser.COMMA); }
		public TerminalNode COMMA(int i) {
			return getToken(MetaParser.COMMA, i);
		}
		public NextStateCellContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_nextStateCell; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MetaListener ) ((MetaListener)listener).enterNextStateCell(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MetaListener ) ((MetaListener)listener).exitNextStateCell(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof MetaVisitor ) return ((MetaVisitor<? extends T>)visitor).visitNextStateCell(this);
			else return visitor.visitChildren(this);
		}
	}

	public final NextStateCellContext nextStateCell() throws RecognitionException {
		NextStateCellContext _localctx = new NextStateCellContext(_ctx, getState());
		enterRule(_localctx, 22, RULE_nextStateCell);
		((NextStateCellContext)_localctx).isTerminal =  true;
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(110);
			((NextStateCellContext)_localctx).IDENT = match(IDENT);
			((NextStateCellContext)_localctx).name = (((NextStateCellContext)_localctx).IDENT!=null?((NextStateCellContext)_localctx).IDENT.getText():null);
			setState(130);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==OPEN) {
				{
				setState(112);
				match(OPEN);
				((NextStateCellContext)_localctx).isTerminal =  false; ((NextStateCellContext)_localctx).maybe =  new StateTransition(_localctx.name);
				setState(126);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la==IDENT) {
					{
					{
					setState(119);
					_errHandler.sync(this);
					_alt = getInterpreter().adaptivePredict(_input,9,_ctx);
					while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
						if ( _alt==1 ) {
							{
							{
							setState(114);
							((NextStateCellContext)_localctx).IDENT = match(IDENT);
							_localctx.maybe.addArg((((NextStateCellContext)_localctx).IDENT!=null?((NextStateCellContext)_localctx).IDENT.getText():null));
							setState(116);
							match(COMMA);
							}
							} 
						}
						setState(121);
						_errHandler.sync(this);
						_alt = getInterpreter().adaptivePredict(_input,9,_ctx);
					}
					setState(122);
					((NextStateCellContext)_localctx).IDENT = match(IDENT);
					_localctx.maybe.addArg((((NextStateCellContext)_localctx).IDENT!=null?((NextStateCellContext)_localctx).IDENT.getText():null));
					}
					}
					setState(128);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				setState(129);
				match(CLOSE);
				}
			}

			if (_localctx.isTerminal) {((NextStateCellContext)_localctx).c =  new TerminalTransition(_localctx.name);} else {((NextStateCellContext)_localctx).c =  _localctx.maybe;}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static final String _serializedATN =
		"\3\u608b\ua72a\u8133\ub9ed\u417c\u3be7\u7786\u5964\3\22\u0089\4\2\t\2"+
		"\4\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b\t\b\4\t\t\t\4\n\t\n\4\13"+
		"\t\13\4\f\t\f\4\r\t\r\3\2\3\2\3\2\6\2\36\n\2\r\2\16\2\37\3\3\3\3\3\3\3"+
		"\4\3\4\3\4\3\4\3\4\3\4\5\4+\n\4\3\5\3\5\3\5\3\5\3\5\3\5\3\5\3\6\3\6\3"+
		"\6\3\6\3\6\3\7\3\7\3\7\6\7<\n\7\r\7\16\7=\3\7\3\7\3\b\3\b\3\b\3\b\3\b"+
		"\3\b\3\b\3\t\3\t\3\t\3\t\3\t\3\t\3\t\3\t\7\tQ\n\t\f\t\16\tT\13\t\3\t\3"+
		"\t\3\n\3\n\3\n\3\n\3\n\3\13\3\13\3\13\3\13\3\13\3\13\3\13\6\13d\n\13\r"+
		"\13\16\13e\3\f\3\f\3\f\3\f\3\f\6\fm\n\f\r\f\16\fn\3\r\3\r\3\r\3\r\3\r"+
		"\3\r\3\r\7\rx\n\r\f\r\16\r{\13\r\3\r\3\r\7\r\177\n\r\f\r\16\r\u0082\13"+
		"\r\3\r\5\r\u0085\n\r\3\r\3\r\3\r\2\2\16\2\4\6\b\n\f\16\20\22\24\26\30"+
		"\2\2\2\u0089\2\35\3\2\2\2\4!\3\2\2\2\6*\3\2\2\2\b,\3\2\2\2\n\63\3\2\2"+
		"\2\f8\3\2\2\2\16A\3\2\2\2\20H\3\2\2\2\22W\3\2\2\2\24c\3\2\2\2\26l\3\2"+
		"\2\2\30p\3\2\2\2\32\33\5\6\4\2\33\34\7\5\2\2\34\36\3\2\2\2\35\32\3\2\2"+
		"\2\36\37\3\2\2\2\37\35\3\2\2\2\37 \3\2\2\2 \3\3\2\2\2!\"\7\17\2\2\"#\b"+
		"\3\1\2#\5\3\2\2\2$%\7\4\2\2%+\5\b\5\2&\'\7\7\2\2\'+\5\4\3\2()\7\6\2\2"+
		")+\5\f\7\2*$\3\2\2\2*&\3\2\2\2*(\3\2\2\2+\7\3\2\2\2,-\7\16\2\2-.\b\5\1"+
		"\2./\7\b\2\2/\60\5\n\6\2\60\61\b\5\1\2\61\62\b\5\1\2\62\t\3\2\2\2\63\64"+
		"\7\f\2\2\64\65\7\17\2\2\65\66\7\r\2\2\66\67\b\6\1\2\67\13\3\2\2\28;\5"+
		"\16\b\29:\7\3\2\2:<\5\24\13\2;9\3\2\2\2<=\3\2\2\2=;\3\2\2\2=>\3\2\2\2"+
		">?\3\2\2\2?@\b\7\1\2@\r\3\2\2\2AB\5\20\t\2BC\b\b\1\2CD\7\16\2\2DE\b\b"+
		"\1\2EF\5\20\t\2FG\b\b\1\2G\17\3\2\2\2HR\7\f\2\2IJ\5\22\n\2JK\b\t\1\2K"+
		"Q\3\2\2\2LM\5\22\n\2MN\b\t\1\2NO\7\13\2\2OQ\3\2\2\2PI\3\2\2\2PL\3\2\2"+
		"\2QT\3\2\2\2RP\3\2\2\2RS\3\2\2\2SU\3\2\2\2TR\3\2\2\2UV\7\r\2\2V\21\3\2"+
		"\2\2WX\7\17\2\2XY\b\n\1\2YZ\7\16\2\2Z[\b\n\1\2[\23\3\2\2\2\\]\5\26\f\2"+
		"]^\b\13\1\2^d\3\2\2\2_`\7\22\2\2`a\5\26\f\2ab\b\13\1\2bd\3\2\2\2c\\\3"+
		"\2\2\2c_\3\2\2\2de\3\2\2\2ec\3\2\2\2ef\3\2\2\2f\25\3\2\2\2gh\7\17\2\2"+
		"hm\b\f\1\2ij\5\30\r\2jk\b\f\1\2km\3\2\2\2lg\3\2\2\2li\3\2\2\2mn\3\2\2"+
		"\2nl\3\2\2\2no\3\2\2\2o\27\3\2\2\2pq\7\16\2\2q\u0084\b\r\1\2rs\7\f\2\2"+
		"s\u0080\b\r\1\2tu\7\16\2\2uv\b\r\1\2vx\7\13\2\2wt\3\2\2\2x{\3\2\2\2yw"+
		"\3\2\2\2yz\3\2\2\2z|\3\2\2\2{y\3\2\2\2|}\7\16\2\2}\177\b\r\1\2~y\3\2\2"+
		"\2\177\u0082\3\2\2\2\u0080~\3\2\2\2\u0080\u0081\3\2\2\2\u0081\u0083\3"+
		"\2\2\2\u0082\u0080\3\2\2\2\u0083\u0085\7\r\2\2\u0084r\3\2\2\2\u0084\u0085"+
		"\3\2\2\2\u0085\u0086\3\2\2\2\u0086\u0087\b\r\1\2\u0087\31\3\2\2\2\16\37"+
		"*=PRcelny\u0080\u0084";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}