import generator.MainGen;
import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.CommonTokenStream;

import java.io.*;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

public class MetaRunner {
    public static final Path ARITHMETICS_PATH = Paths.get("arithmetics.txt");
    public static final Path PYTHON_PATH = Paths.get("python.txt");

    public static void main(String[] args) throws IOException {
        arithmetics();
        python();
    }

    private static void arithmetics() throws IOException {
        String input = String.join("\n", Files.readAllLines(ARITHMETICS_PATH));
        MetaParser.ResultContext resultContext = parseFromString(input);
        var res = resultContext.values;
        new MainGen("arithmetics").generate(res);
    }

    private static void python() throws IOException {
        String input = String.join("\n", Files.readAllLines(PYTHON_PATH));
        MetaParser.ResultContext resultContext = parseFromString(input);
        var res = resultContext.values;
        new MainGen("python").generate(res);
    }

    public static MetaParser.ResultContext parseFromString(String inputString) throws IOException {
        return parseFromStream(new ByteArrayInputStream(inputString.getBytes()));
    }

    public static MetaParser.ResultContext parseFromStream(InputStream is) throws IOException {
        return getParser(is).result();
    }

    private static MetaParser getParser(InputStream is) throws IOException {
        ANTLRInputStream input = new ANTLRInputStream(is);
        MetaLexer lexer = new MetaLexer(input);
        CommonTokenStream tokens = new CommonTokenStream(lexer);
        return new MetaParser(tokens);
    }

}
