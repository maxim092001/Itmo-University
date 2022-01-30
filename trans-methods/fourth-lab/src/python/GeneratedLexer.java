package python;
import java.util.Map;
import java.util.HashMap;
import java.util.regex.*;
import java.util.Iterator;

public class GeneratedLexer implements Iterator<GeneratedEnum>, Iterable<GeneratedEnum> {
private final Map<GeneratedEnum.EToken, Pattern> patterns = Map.ofEntries(
Map.entry(GeneratedEnum.EToken.IN, Pattern.compile("in")),
Map.entry(GeneratedEnum.EToken.OPEN, Pattern.compile("\\(")),
Map.entry(GeneratedEnum.EToken.CLOSE, Pattern.compile("\\)")),
Map.entry(GeneratedEnum.EToken.NOT, Pattern.compile("not")),
Map.entry(GeneratedEnum.EToken.AND, Pattern.compile("and")),
Map.entry(GeneratedEnum.EToken.OR, Pattern.compile("or")),
Map.entry(GeneratedEnum.EToken.XOR, Pattern.compile("xor")),
Map.entry(GeneratedEnum.EToken.VAR, Pattern.compile("[a-zA-Z]+"))
);

private final Pattern skip = Pattern.compile("[ \t\n\r]+");
private final Matcher matcher = skip.matcher("");
private int curStart, curEnd;
private String text;
private boolean hasNext;

public int position() { return curEnd; }

@Override
public Iterator<GeneratedEnum> iterator() {
return this;
}

public GeneratedLexer(String text) {
this.text = text;
curStart = 0;
curEnd = 0;
hasNext = true;
}

@Override
public boolean hasNext() {
return hasNext;
}

private boolean matchLookingAt() {
if (matcher.lookingAt()) {
curStart = curEnd;
curEnd = curStart + matcher.end();
matcher.reset(text.substring(curEnd));
return true;
}
return false;
}

@Override
public GeneratedEnum next() {
curStart = curEnd;
matcher.usePattern(skip);
matcher.reset(text.substring(curStart));
matchLookingAt();
for (var t : GeneratedEnum.EToken.values()) {
if (t == GeneratedEnum.EToken.END || t == GeneratedEnum.EToken.EPS) {
continue;
}
matcher.usePattern(patterns.get(t));
if (matchLookingAt()) {
return new GeneratedEnum(t, text.substring(curStart, curEnd));
}
}
if (curEnd != text.length()) {
throw new Error();
}
hasNext = false;
return new GeneratedEnum(GeneratedEnum.EToken.END, null);
}
}