package arithmetics;
public class GeneratedEnum {
public String text;
public EToken token;
public GeneratedEnum(EToken token, String text) {
this.text = text;
this.token = token;
}
enum EToken {SUB, NUM, ADD, MUL, DIV, LPAR, RPAR, SEMI, NOT, AND, OR, XOR, END, EPS;}
@Override
public String toString() {
return text;
}
}