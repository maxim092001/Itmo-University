package util;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Locale;
import java.util.stream.Collectors;


public class NonTerminal implements Block {
    private static final String INNER_ATTR_VAR = "_inner";
    private static final String LEXER_NAME = "lexer";
    private static final String PARSE_EXCEPTION_NAME = "ParseException";

    // attributes
    public List<AttributeWithType> inherited = new ArrayList<>();
    public List<AttributeWithType> inner = new ArrayList<>();

    // transitions
    public List<Transition> transitions = new ArrayList<>();

    // rule name
    public String name;

    // rule name to inner attribute wrapper class name
    private static String stringToClassName(String s) {
        String name = s.toLowerCase(Locale.ROOT);
        name = Character.toUpperCase(name.charAt(0)) + name.substring(1) + "Attributes";
        return name;
    }

    public String toJava() {
        String representation = generateDataType();
        return '\n' + representation + '\n' + generateHeader() + " {\n" + generateBody() + "\n}";
    }

    private String generateBody() {
        String innerAttributesDeclaration = innerAttributesDeclaration();
        String transitionVariablesDeclaration = generateTransitionVariablesDeclaration();
        String inheritedAssignment = wrapAttributes(inherited);
        String innerAttributesWrapper = innerAttributesWrapper();
        String innerAssignment = wrapAttributes(inner);
        String calls = generateTransitions();
        String returnTail = generateReturn();
        return innerAttributesDeclaration + '\n'
                + transitionVariablesDeclaration + '\n'
                + innerAttributesWrapper + '\n'
                + calls + '\n'
                + inheritedAssignment + '\n'
                + innerAssignment + '\n'
                + returnTail;
    }

    private String generateReturn() {
        return String.format("\treturn %s;", INNER_ATTR_VAR);
    }

    private String attributesToVars(final List<AttributeWithType> attributes, final String prefix) {
        return attributes.stream().map(x -> prefix + x.toString() + ";").collect(Collectors.joining("\n"));
    }

    private String generateRequiredConstructor() {
        return String.format("""
                public %s(String name) {
                super(name);
                }
                """, stringToClassName(name));
    }

    private String generateDataType() {
        return String.format("public static class %s extends Node {\n%s\n%s\n%s\n}",
                stringToClassName(name),
                generateRequiredConstructor(),
                attributesToVars(inner, "public "),
                attributesToVars(inherited, "public "));
    }

    private String generateHeader() {
        return String.format("public %s %s(%s) throws %s",
                stringToClassName(name),
                name,
                inherited.stream().map(AttributeWithType::toString).collect(Collectors.joining(",\n")),
                PARSE_EXCEPTION_NAME);
    }

    private String innerAttributesDeclaration() {
        return inner.stream().map(x -> '\t' + x.toString() + " = null;").collect(Collectors.joining("\n"));
    }

    private String innerAttributesWrapper() {
        return String.format("\t%s _inner = new %s(\"%s\");", stringToClassName(name), stringToClassName(name), name);
    }

    private String wrapAttributes(List<AttributeWithType> attrs) {
        return attrs.stream()
                .map(attr -> String.format("%s.%s = %s;", INNER_ATTR_VAR, attr.name, attr.name))
                .collect(Collectors.joining("\n"));
    }

    private String generateInnerSwitch() {
        StringBuilder sb = new StringBuilder();
        for (var transition : transitions) {
            sb.append(String.format("\ncase %s -> {\n%s = curToken;\n%s\n%s\n}",
                    transition.subTransitions.get(0).toString(),
                    transition.subTransitions.get(0).toString(),
                    transition.subTransitions.get(0).toString().equals("END") || transition.subTransitions.get(0).toString().equals("EPS") ? "" : ADD_NEXT,
                    transition.subTransitions.stream()
                            .skip(1)
                            .map(x -> x.toJava() +
                                    (
                                            x instanceof JavaCell || (x instanceof TerminalTransition
                                                    && ((TerminalTransition) x).name.equals("EPS"))
                                                    ? "" : String.format("\n_inner.children.add(%s);", x.toNode())))
                            .collect(Collectors.joining(";"))));
            if (transition.subTransitions.get(0).toString().equals("EPS")) {
                sb.append(String.format("\ndefault -> {%s}", transition.subTransitions.stream().skip(1).map(SubRuleCell::toJava).collect(Collectors.joining(";"))));
            }
        }
        if (transitions.stream()
                .noneMatch(
                        x -> x.subTransitions.get(0) instanceof TerminalTransition
                                && x.subTransitions.get(0).toString().equals("EPS"))) {
            sb.append("\n\t\tdefault -> throw new ParseException(\"Unexpected token\", lexer.position());");
        }
        return sb.toString();
    }

    private static final String ADD_NEXT = """
            _inner.children.add(new Node(curToken.toString()));
            curToken = lexer.next();
            """;

    private String generateSwitchCase() {
        return String.format("""
                switch (curToken.token) {%s
                }""", generateInnerSwitch());
    }

    private String generateTransitions() {
        if (transitions.size() > 1) {
            return generateSwitchCase();
        }
        return transitions.get(0).subTransitions.stream()
                .map(x -> x.toJava() + (x instanceof JavaCell ? "\n" : String.format("\n_inner.children.add(%s);", x.toNode())))
                .collect(Collectors.joining("\n"));
    }

    private String generateTransitionVariablesDeclaration() {
        return String.join("\n", transitions.stream()
                .map(x -> x.subTransitions)
                .flatMap(Collection::stream)
                .filter(x -> x instanceof SubTransition)
                .map(x -> (SubTransition) x)
                .map(SubTransition::declaration)
                .collect(Collectors.toSet()));
    }

    public interface SubRuleCell {
        String toJava();

        String toNode();
    }

    public static abstract class SubTransition implements SubRuleCell {
        public String name = null;

        public abstract String getVarClassName();

        public String declaration() {
            return String.format("\t%s %s = null;", getVarClassName(), name);
        }
    }

    public static class JavaCell implements SubRuleCell {
        public String s;

        public JavaCell(String s) {
            this.s = s;
        }

        @Override
        public String toString() {
            return this.s;
        }

        @Override
        public String toJava() {
            return toString();
        }

        @Override
        public String toNode() {
            throw new IllegalStateException();
        }

    }

    public static class TerminalTransition extends SubTransition {
        public TerminalTransition(String name) {
            this.name = name;
        }

        @Override
        public String getVarClassName() {
            return "GeneratedEnum";
        }

        @Override
        public String toString() {
            return name;
        }

        @Override
        public String toJava() {
            return String.format(
                    """
                            %s = curToken;
                            if (GeneratedEnum.EToken.%s != curToken.token) {
                            throw new ParseException("UNEXPECTED TOKEN(((", lexer.position());
                            }
                            curToken = lexer.next();
                            """,
                    name, name);
        }

        @Override
        public String toNode() {
            return String.format("new Node(\"%s\")", this);
        }

    }

    public static class StateTransition extends SubTransition {
        public List<String> args = new ArrayList<>();

        public StateTransition(String name) {
            this.name = name.toLowerCase(Locale.ROOT);
        }

        public void addArg(String arg) {
            args.add(arg);
        }

        @Override
        public String getVarClassName() {
            return stringToClassName(name);
        }

        @Override
        public String toJava() {
            String argName = name;
            return String.format("%s = %s(%s);",
                    argName,
                    name,
                    String.join(", ", args));
        }

        @Override
        public String toNode() {
            return name;
        }

        @Override
        public String toString() {
            return name;
        }
    }

    public static class Transition {
        public List<SubRuleCell> subTransitions = new ArrayList<>();

        public void add(SubRuleCell c) {
            subTransitions.add(c);
        }

        @Override
        public String toString() {
            return subTransitions.stream().map(Object::toString).collect(Collectors.joining("\n"));
        }
    }

    public static class AttributeWithType {
        public String type, name;

        @Override
        public String toString() {
            return type + " " + name;
        }
    }

}
