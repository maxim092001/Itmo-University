package python;

import java.text.ParseException;
import util.base.*;
public class GeneratedParser {
public GeneratedEnum curToken;
public final GeneratedLexer lexer;
public GeneratedParser(String text) {
lexer = new GeneratedLexer(text);
curToken = lexer.next();
}

public static class EAttributes extends Node {
public EAttributes(String name) {
super(name);
}



}
public EAttributes e() throws ParseException {

	TAttributes t = null;
	XAttributes x = null;
	EAttributes _inner = new EAttributes("e");
t = t();
_inner.children.add(t);
x = x();
_inner.children.add(x);


	return _inner;
}

public static class XAttributes extends Node {
public XAttributes(String name) {
super(name);
}



}
public XAttributes x() throws ParseException {

	GeneratedEnum EPS = null;
	TAttributes t = null;
	XAttributes x = null;
	GeneratedEnum XOR = null;
	XAttributes _inner = new XAttributes("x");
switch (curToken.token) {
case XOR -> {
XOR = curToken;
_inner.children.add(new Node(curToken.toString()));
curToken = lexer.next();

t = t();
_inner.children.add(t);;x = x();
_inner.children.add(x);
}
case EPS -> {
EPS = curToken;


}
default -> {}
}


	return _inner;
}

public static class TAttributes extends Node {
public TAttributes(String name) {
super(name);
}



}
public TAttributes t() throws ParseException {

	ZAttributes z = null;
	MAttributes m = null;
	TAttributes _inner = new TAttributes("t");
m = m();
_inner.children.add(m);
z = z();
_inner.children.add(z);


	return _inner;
}

public static class ZAttributes extends Node {
public ZAttributes(String name) {
super(name);
}



}
public ZAttributes z() throws ParseException {

	ZAttributes z = null;
	GeneratedEnum OR = null;
	GeneratedEnum EPS = null;
	MAttributes m = null;
	ZAttributes _inner = new ZAttributes("z");
switch (curToken.token) {
case OR -> {
OR = curToken;
_inner.children.add(new Node(curToken.toString()));
curToken = lexer.next();

m = m();
_inner.children.add(m);;z = z();
_inner.children.add(z);
}
case EPS -> {
EPS = curToken;


}
default -> {}
}


	return _inner;
}

public static class MAttributes extends Node {
public MAttributes(String name) {
super(name);
}



}
public MAttributes m() throws ParseException {

	YAttributes y = null;
	GAttributes g = null;
	MAttributes _inner = new MAttributes("m");
g = g();
_inner.children.add(g);
y = y();
_inner.children.add(y);


	return _inner;
}

public static class YAttributes extends Node {
public YAttributes(String name) {
super(name);
}



}
public YAttributes y() throws ParseException {

	GeneratedEnum EPS = null;
	GeneratedEnum AND = null;
	YAttributes y = null;
	GAttributes g = null;
	YAttributes _inner = new YAttributes("y");
switch (curToken.token) {
case AND -> {
AND = curToken;
_inner.children.add(new Node(curToken.toString()));
curToken = lexer.next();

g = g();
_inner.children.add(g);;y = y();
_inner.children.add(y);
}
case EPS -> {
EPS = curToken;


}
default -> {}
}


	return _inner;
}

public static class GAttributes extends Node {
public GAttributes(String name) {
super(name);
}



}
public GAttributes g() throws ParseException {

	EAttributes e = null;
	GeneratedEnum OPEN = null;
	GeneratedEnum NOT = null;
	GeneratedEnum CLOSE = null;
	GAttributes g = null;
	GeneratedEnum VAR = null;
	KAttributes k = null;
	GAttributes _inner = new GAttributes("g");
switch (curToken.token) {
case OPEN -> {
OPEN = curToken;
_inner.children.add(new Node(curToken.toString()));
curToken = lexer.next();

e = e();
_inner.children.add(e);;CLOSE = curToken;
if (GeneratedEnum.EToken.CLOSE != curToken.token) {
throw new ParseException("UNEXPECTED TOKEN(((", lexer.position());
}
curToken = lexer.next();

_inner.children.add(new Node("CLOSE"));
}
case NOT -> {
NOT = curToken;
_inner.children.add(new Node(curToken.toString()));
curToken = lexer.next();

g = g();
_inner.children.add(g);
}
case VAR -> {
VAR = curToken;
_inner.children.add(new Node(curToken.toString()));
curToken = lexer.next();

k = k();
_inner.children.add(k);
}
		default -> throw new ParseException("Unexpected token", lexer.position());
}


	return _inner;
}

public static class KAttributes extends Node {
public KAttributes(String name) {
super(name);
}



}
public KAttributes k() throws ParseException {

	GeneratedEnum NOT = null;
	GeneratedEnum EPS = null;
	GeneratedEnum IN = null;
	IAttributes i = null;
	GeneratedEnum VAR = null;
	KAttributes _inner = new KAttributes("k");
switch (curToken.token) {
case NOT -> {
NOT = curToken;
_inner.children.add(new Node(curToken.toString()));
curToken = lexer.next();

i = i();
_inner.children.add(i);
}
case IN -> {
IN = curToken;
_inner.children.add(new Node(curToken.toString()));
curToken = lexer.next();

VAR = curToken;
if (GeneratedEnum.EToken.VAR != curToken.token) {
throw new ParseException("UNEXPECTED TOKEN(((", lexer.position());
}
curToken = lexer.next();

_inner.children.add(new Node("VAR"));
}
case EPS -> {
EPS = curToken;


}
default -> {}
}


	return _inner;
}

public static class IAttributes extends Node {
public IAttributes(String name) {
super(name);
}



}
public IAttributes i() throws ParseException {

	GeneratedEnum NOT = null;
	GeneratedEnum IN = null;
	IAttributes i = null;
	GeneratedEnum VAR = null;
	IAttributes _inner = new IAttributes("i");
switch (curToken.token) {
case NOT -> {
NOT = curToken;
_inner.children.add(new Node(curToken.toString()));
curToken = lexer.next();

i = i();
_inner.children.add(i);
}
case IN -> {
IN = curToken;
_inner.children.add(new Node(curToken.toString()));
curToken = lexer.next();

VAR = curToken;
if (GeneratedEnum.EToken.VAR != curToken.token) {
throw new ParseException("UNEXPECTED TOKEN(((", lexer.position());
}
curToken = lexer.next();

_inner.children.add(new Node("VAR"));
}
		default -> throw new ParseException("Unexpected token", lexer.position());
}


	return _inner;
}


public static class Node extends BaseNode {
public Node(String name) {
super(name);
}
}

}