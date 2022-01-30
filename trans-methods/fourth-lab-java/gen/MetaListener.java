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

import org.antlr.v4.runtime.tree.ParseTreeListener;

/**
 * This interface defines a complete listener for a parse tree produced by
 * {@link MetaParser}.
 */
public interface MetaListener extends ParseTreeListener {
	/**
	 * Enter a parse tree produced by {@link MetaParser#result}.
	 * @param ctx the parse tree
	 */
	void enterResult(MetaParser.ResultContext ctx);
	/**
	 * Exit a parse tree produced by {@link MetaParser#result}.
	 * @param ctx the parse tree
	 */
	void exitResult(MetaParser.ResultContext ctx);
	/**
	 * Enter a parse tree produced by {@link MetaParser#required}.
	 * @param ctx the parse tree
	 */
	void enterRequired(MetaParser.RequiredContext ctx);
	/**
	 * Exit a parse tree produced by {@link MetaParser#required}.
	 * @param ctx the parse tree
	 */
	void exitRequired(MetaParser.RequiredContext ctx);
	/**
	 * Enter a parse tree produced by {@link MetaParser#term}.
	 * @param ctx the parse tree
	 */
	void enterTerm(MetaParser.TermContext ctx);
	/**
	 * Exit a parse tree produced by {@link MetaParser#term}.
	 * @param ctx the parse tree
	 */
	void exitTerm(MetaParser.TermContext ctx);
	/**
	 * Enter a parse tree produced by {@link MetaParser#terminal}.
	 * @param ctx the parse tree
	 */
	void enterTerminal(MetaParser.TerminalContext ctx);
	/**
	 * Exit a parse tree produced by {@link MetaParser#terminal}.
	 * @param ctx the parse tree
	 */
	void exitTerminal(MetaParser.TerminalContext ctx);
	/**
	 * Enter a parse tree produced by {@link MetaParser#pattern}.
	 * @param ctx the parse tree
	 */
	void enterPattern(MetaParser.PatternContext ctx);
	/**
	 * Exit a parse tree produced by {@link MetaParser#pattern}.
	 * @param ctx the parse tree
	 */
	void exitPattern(MetaParser.PatternContext ctx);
	/**
	 * Enter a parse tree produced by {@link MetaParser#nonTerminal}.
	 * @param ctx the parse tree
	 */
	void enterNonTerminal(MetaParser.NonTerminalContext ctx);
	/**
	 * Exit a parse tree produced by {@link MetaParser#nonTerminal}.
	 * @param ctx the parse tree
	 */
	void exitNonTerminal(MetaParser.NonTerminalContext ctx);
	/**
	 * Enter a parse tree produced by {@link MetaParser#nonTerminalHeader}.
	 * @param ctx the parse tree
	 */
	void enterNonTerminalHeader(MetaParser.NonTerminalHeaderContext ctx);
	/**
	 * Exit a parse tree produced by {@link MetaParser#nonTerminalHeader}.
	 * @param ctx the parse tree
	 */
	void exitNonTerminalHeader(MetaParser.NonTerminalHeaderContext ctx);
	/**
	 * Enter a parse tree produced by {@link MetaParser#declarations}.
	 * @param ctx the parse tree
	 */
	void enterDeclarations(MetaParser.DeclarationsContext ctx);
	/**
	 * Exit a parse tree produced by {@link MetaParser#declarations}.
	 * @param ctx the parse tree
	 */
	void exitDeclarations(MetaParser.DeclarationsContext ctx);
	/**
	 * Enter a parse tree produced by {@link MetaParser#declaration}.
	 * @param ctx the parse tree
	 */
	void enterDeclaration(MetaParser.DeclarationContext ctx);
	/**
	 * Exit a parse tree produced by {@link MetaParser#declaration}.
	 * @param ctx the parse tree
	 */
	void exitDeclaration(MetaParser.DeclarationContext ctx);
	/**
	 * Enter a parse tree produced by {@link MetaParser#nonTerminalPart}.
	 * @param ctx the parse tree
	 */
	void enterNonTerminalPart(MetaParser.NonTerminalPartContext ctx);
	/**
	 * Exit a parse tree produced by {@link MetaParser#nonTerminalPart}.
	 * @param ctx the parse tree
	 */
	void exitNonTerminalPart(MetaParser.NonTerminalPartContext ctx);
	/**
	 * Enter a parse tree produced by {@link MetaParser#nonTerminalPartBlock}.
	 * @param ctx the parse tree
	 */
	void enterNonTerminalPartBlock(MetaParser.NonTerminalPartBlockContext ctx);
	/**
	 * Exit a parse tree produced by {@link MetaParser#nonTerminalPartBlock}.
	 * @param ctx the parse tree
	 */
	void exitNonTerminalPartBlock(MetaParser.NonTerminalPartBlockContext ctx);
	/**
	 * Enter a parse tree produced by {@link MetaParser#nextStateCell}.
	 * @param ctx the parse tree
	 */
	void enterNextStateCell(MetaParser.NextStateCellContext ctx);
	/**
	 * Exit a parse tree produced by {@link MetaParser#nextStateCell}.
	 * @param ctx the parse tree
	 */
	void exitNextStateCell(MetaParser.NextStateCellContext ctx);
}