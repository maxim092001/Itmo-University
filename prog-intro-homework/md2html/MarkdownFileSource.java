package md2html;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.io.Writer;
import java.nio.charset.StandardCharsets;

public class MarkdownFileSource {

    private BufferedReader reader;
    private Writer writer;
    private final char END = '\0';
    private char c;
    private String line = "";
    private StringBuilder paragraph = new StringBuilder();

    public MarkdownFileSource(final String inputFileName, final String outputFileName) {
        try {
            reader = new BufferedReader(new FileReader(inputFileName, StandardCharsets.UTF_8));
        } catch (UnsupportedEncodingException e) {
            System.out.println("Wrong file coding");
        } catch (IOException e) {
            System.out.println("Something wrong with input file");
        }

        try {
            writer = new BufferedWriter(new FileWriter(outputFileName, StandardCharsets.UTF_8));
        } catch (IOException e) {
            System.out.println("Something wrong with output file");
        }
    }


    public char getChar() {
        return c;
    }

    public String getParagraph() {
        return paragraph.toString();
    }

    public String getLine() {
        return line;
    }

    private char readChar() throws IOException {
        final int read = reader.read();
        return read == -1 ? END : (char) read;
    }

    private String nextLine() throws IOException {
        return line = reader.readLine();
    }

    public char nextChar() throws IOException {
        c = readChar();
        return c;
    }

    public void write(final String out) throws IOException {
        writer.write(out);
    }

    public String nextParagraph() throws IOException {
        if (line == null) {
            return "";
        }

        while (line != null && line.isEmpty()) {
            line = nextLine();
        }

        paragraph = new StringBuilder();
        paragraph.append(line);

        while (nextLine() != null && !getLine().isEmpty()) {
            paragraph.append('\n');
            paragraph.append(getLine());
        }

        while (line != null && line.isEmpty()) {
            nextLine();
        }

        return paragraph.toString();
    }

    public void close() throws IOException {
        reader.close();
        writer.close();
    }
}
