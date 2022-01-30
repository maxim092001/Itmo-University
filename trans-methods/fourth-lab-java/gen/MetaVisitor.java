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

import org.antlr.v4.runtime.tree.ParseTreeVisitor;

/**
 * This interface defines a complete generic visitor for a parse tree produced
 * by {@link MetaParser}.
 *
 * @param <T> The return type of the visit operation. Use {@link Void} for
 * operations with no return type.
 */
public interface MetaVisitor<T> extends ParseTreeVisitor<T> {
	/**
	 * Visit a parse tree produced by {@link MetaParser#result}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitResult(MetaParser.ResultContext ctx);
	/**
	 * Visit a parse tree produced by {@link MetaParser#required}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitRequired(MetaParser.RequiredContext ctx);
	/**
	 * Visit a parse tree produced by {@link MetaParser#term}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTerm(MetaParser.TermContext ctx);
	/**
	 * Visit a parse tree produced by {@link MetaParser#terminal}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTerminal(MetaParser.TerminalContext ctx);
	/**
	 * Visit a parse tree produced by {@link MetaParser#pattern}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPattern(MetaParser.PatternContext ctx);
	/**
	 * Visit a parse tree produced by {@link MetaParser#nonTerminal}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitNonTerminal(MetaParser.NonTerminalContext ctx);
	/**
	 * Visit a parse tree produced by {@link MetaParser#nonTerminalHeader}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitNonTerminalHeader(MetaParser.NonTerminalHeaderContext ctx);
	/**
	 * Visit a parse tree produced by {@link MetaParser#declarations}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDeclarations(MetaParser.DeclarationsContext ctx);
	/**
	 * Visit a parse tree produced by {@link MetaParser#declaration}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDeclaration(MetaParser.DeclarationContext ctx);
	/**
	 * Visit a parse tree produced by {@link MetaParser#nonTerminalPart}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitNonTerminalPart(MetaParser.NonTerminalPartContext ctx);
	/**
	 * Visit a parse tree produced by {@link MetaParser#nonTerminalPartBlock}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitNonTerminalPartBlock(MetaParser.NonTerminalPartBlockContext ctx);
	/**
	 * Visit a parse tree produced by {@link MetaParser#nextStateCell}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitNextStateCell(MetaParser.NextStateCellContext ctx);
}