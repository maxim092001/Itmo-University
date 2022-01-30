package python;
public class GeneratedEnum {
public String text;
public EToken token;
public GeneratedEnum(EToken token, String text) {
this.text = text;
this.token = token;
}
enum EToken {IN, OPEN, CLOSE, NOT, AND, OR, XOR, VAR, END, EPS;}
@Override
public String toString() {
return text;
}
}