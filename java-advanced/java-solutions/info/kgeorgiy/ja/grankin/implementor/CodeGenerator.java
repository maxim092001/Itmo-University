package info.kgeorgiy.ja.grankin.implementor;

import info.kgeorgiy.java.advanced.implementor.ImplerException;

import java.io.File;
import java.lang.reflect.Constructor;
import java.lang.reflect.Executable;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.*;
import java.util.stream.Collectors;

/**
 * Class for generating sourcecode.
 */
public class CodeGenerator {

    /* ----------------------------------- Package ----------------------------------- */

    /**
     * Returns generated package name for given {@link Class} token.
     *
     * <p>For example, suppose token as "{@code java.io.IOException}",
     * then invoking this method will result: "{@code package java.io;}"
     *
     * @param token given token.
     * @return generated package name.
     */
    private static String packageName(final Class<?> token) {
        Package packageObj = token.getPackage();

        return Objects.isNull(packageObj) ? GeneratorUtils.EMPTY :
                String.join(GeneratorUtils.SPACE, GeneratorUtils.PACKAGE, packageObj.getName(), GeneratorUtils.SEMICOLON, GeneratorUtils.LINE_SEPARATOR);
    }

    /* ----------------------------------- Class Declaration ----------------------------------- */


    /**
     * Returns generated class declaration for given {@link Class} token.
     *
     * <p>For example, suppose token as "{@code java.io.IOException}",
     * then invoking this method will result: "{@code public class IOException extends ... implements ...}"
     *
     * @param token given token.
     * @return generated class declaration.
     */
    private static String classDeclaration(final Class<?> token) {
        return String.join(GeneratorUtils.SPACE,
                GeneratorUtils.PUBLIC,
                GeneratorUtils.CLASS,
                GeneratorUtils.generateClassImplByToken(token),
                (token.isInterface() ? GeneratorUtils.IMPLEMENTS : GeneratorUtils.EXTENDS),
                token.getCanonicalName(),
                GeneratorUtils.CURLY_OPEN_BRACKET
        );
    }

    /* ----------------------------------- Constructors ----------------------------------- */

    /**
     * Returns generated constructor sourcecode for given {@link Constructor} constructor.
     *
     * <p>For example, suppose constructor as "{@code public ArrayList(int initialCapacity)}",
     * then invoking this method will result:
     * <pre>
     * public ArrayList(int initialCapacity) {
     *     super(0); // 0 was chosen via {@link CodeGenerator#defaultValue(Class)}
     * }
     * </pre>
     *
     * @param constructor given
     * @return generated constructor sourcecode.
     */
    private static String generateConstructor(final Constructor<?> constructor) {
        return String.join(GeneratorUtils.LINE_SEPARATOR,
                GeneratorUtils.TAB + generateExecutable(constructor, constructor.getDeclaringClass().getSimpleName() + GeneratorUtils.IMPL),
                String.join(GeneratorUtils.EMPTY,
                        GeneratorUtils.TAB.repeat(2),
                        GeneratorUtils.SUPER,
                        GeneratorUtils.OPEN_BRACKET,
                        generateArguments(constructor, false),
                        GeneratorUtils.CLOSE_BRACKET
                ) + GeneratorUtils.SEMICOLON,
                GeneratorUtils.TAB + GeneratorUtils.CURLY_CLOSE_BRACKET
        );
    }

    /**
     * Returns {@link List} of constructors generated for given {@link Class} token.
     *
     * <p>Each constructor generated via {@link CodeGenerator#generateConstructor(Constructor)}.
     *
     * @param token given token.
     * @return {@link List} of constructors. Each constructor represented as sourcecode.
     * @throws ImplerException if no public or protected constructors provided.
     */
    private static List<String> generateConstructors(final Class<?> token) throws ImplerException {
        if (token.isInterface()) {
            return Collections.emptyList();
        }

        final List<String> constructors = Arrays.stream(token.getDeclaredConstructors())
                .filter(constructor -> !Modifier.isPrivate(constructor.getModifiers()))
                .map(CodeGenerator::generateConstructor).collect(Collectors.toList());

        if (constructors.isEmpty()) {
            throw new ImplerException("No public/protected constructors.");
        }

        return constructors;
    }


    /* ----------------------------------- Executable ----------------------------------- */

    /**
     * Returns open line for the given executable.
     *
     * @param executable     given executable.
     * @param executableName executable name.
     * @return open line for executable.
     */
    private static String generateExecutable(final Executable executable, final String executableName) {
        return String.join(GeneratorUtils.SPACE,
                GeneratorUtils.PUBLIC,
                String.format("%s%s%s%s", executableName, GeneratorUtils.OPEN_BRACKET, generateArguments(executable, true), GeneratorUtils.CLOSE_BRACKET),
                generateExceptions(executable),
                GeneratorUtils.CURLY_OPEN_BRACKET
        );
    }

    /* ----------------------------------- Exceptions ----------------------------------- */

    /**
     * Returns thrown exceptions for given executable.
     *
     * <p>For example, suppose executable throws "{@link java.io.IOException}" and "{@link ImplerException}",
     * then invoking this method will result:
     * <pre>
     *     throws IOException, ImplerException
     * </pre>
     *
     * @param executable given executable.
     * @return thrown exceptions.
     */
    private static String generateExceptions(final Executable executable) {
        final var exceptions = executable.getExceptionTypes();
        return exceptions.length > 0 ?
                String.join(GeneratorUtils.SPACE,
                        GeneratorUtils.THROWS,
                        Arrays.stream(exceptions).map(Class::getCanonicalName).collect(Collectors.joining(GeneratorUtils.PARAMETER_DELIMITER))
                ) : GeneratorUtils.EMPTY;
    }

    /* ----------------------------------- Methods ----------------------------------- */

    /**
     * Returns wrapped methods.
     *
     * @param methods given array of methods.
     * @return {@link Set} of {@link MethodWrapper}. Wrapped methods.
     */
    private static Set<MethodWrapper> wrapMethods(final Method[] methods) {
        return Arrays.stream(methods).map(MethodWrapper::new).collect(Collectors.toCollection(HashSet::new));
    }

    /**
     * Returns {@link List} of methods sourcecode for given {@link Class} token.
     *
     * <p>Each method generates via {@link CodeGenerator#generateMethod(Method)}
     *
     * @param token given token.
     * @return {@link List} of generated methods.
     */
    private static List<String> generateMethods(final Class<?> token) {
        final Set<MethodWrapper> methods = goSuperClassMethods(token);

        return methods.stream().map(MethodWrapper::getMethod)
                .filter(m -> Modifier.isAbstract(m.getModifiers()))
                .map(CodeGenerator::generateMethod)
                .collect(Collectors.toList());
    }

    /**
     * Returns {@link Set} of {@link MethodWrapper}
     * for all public/protected parents methods for given {@link Class} token.
     *
     * @param token given token.
     * @return All public/protected parents methods.
     */
    private static Set<MethodWrapper> goSuperClassMethods(final Class<?> token) {
        if (token == null) {
            return Collections.emptySet();
        }
        final var methods = wrapMethods(token.getMethods());
        methods.addAll(wrapMethods(token.getDeclaredMethods()));
        methods.addAll(goSuperClassMethods(token.getSuperclass()));
        return methods;
    }

    /**
     * Returns generated sourcecode for method.
     *
     * <p>For example, suppose method is "{@code public int max(int a, int b)}", then invoking this method will result:
     * <pre>
     *     public int max(int a, int b) {
     *         return 0;
     *     }
     * </pre>
     *
     * @param method given method.
     * @return generated sourcecode for method.
     */
    private static String generateMethod(final Method method) {
        return String.join(GeneratorUtils.LINE_SEPARATOR,
                GeneratorUtils.TAB + generateExecutable(method, String.join(GeneratorUtils.SPACE, method.getReturnType().getCanonicalName(), method.getName())),
                String.join(
                        GeneratorUtils.SPACE,
                        GeneratorUtils.TAB.repeat(2) + GeneratorUtils.RETURN, defaultValue(method.getReturnType())) + GeneratorUtils.SEMICOLON,
                GeneratorUtils.TAB + GeneratorUtils.CURLY_CLOSE_BRACKET
        );
    }

    /* ----------------------------------- Arguments ----------------------------------- */

    /**
     * Returns generated sourcecode for arguments by given executable.
     *
     * <p>For example, suppose executable is "{@code public int max(int a, int b)}"
     * and {@code isDeclaration} is {@code true}, then invoking this method will result:
     * <pre>
     *     int a, int b
     * </pre>
     * If {@code isDeclaration} is {@code false}, then this result:
     * <pre>
     *     a, b
     * </pre>
     *
     * @param executable    given executable.
     * @param isDeclaration boolean flag for declaration.
     * @return generated sourcecode for arguments.
     */
    private static String generateArguments(final Executable executable, final boolean isDeclaration) {
        return Arrays.stream(executable.getParameters())
                .map(parameter -> (isDeclaration ? parameter.getType().getCanonicalName() + GeneratorUtils.SPACE : GeneratorUtils.EMPTY) +
                        parameter.getName()).collect(Collectors.joining(GeneratorUtils.PARAMETER_DELIMITER));
    }

    /* ----------------------------------- Utils ----------------------------------- */

    /**
     * Point of entry for class generation.
     *
     * @param token given {@link Class} token.
     * @return generated class sourcecode via {@link String}.
     * @throws ImplerException if exception occurred during generation.
     */
    public static String generate(final Class<?> token) throws ImplerException {
        return String.join(GeneratorUtils.LINE_SEPARATOR,
                packageName(token),
                classDeclaration(token),
                String.join(GeneratorUtils.LINE_SEPARATOR, generateConstructors(token)),
                String.join(GeneratorUtils.LINE_SEPARATOR, generateMethods(token)),
                GeneratorUtils.CURLY_CLOSE_BRACKET
        );
    }

    /**
     * Returns default value depending on type.
     *
     * <p>Default values:</p>
     * <ul>
     * <li>Any reference type default value is "{@code null}"</li>
     * <li>Primitive boolean type default value is "{@code true}"</li>
     * <li>Primitive void type default value is "" (empty string)</li>
     * <li>Numerical primitive type default value is "{@code 0}" </li>
     * </ul>
     *
     * @param token given token.
     * @return default value.
     */
    private static String defaultValue(final Class<?> token) {
        if (!token.isPrimitive()) {
            return "null";
        } else if (token.equals(void.class)) {
            return GeneratorUtils.EMPTY;
        } else if (token.equals(boolean.class)) {
            return "true";
        } else {
            return "0";
        }
    }

    /**
     * Utility class for generating classes and path.
     */
    public static final class GeneratorUtils {

        /**
         * Suffix name for generated classes.
         */
        public static final String IMPL = "Impl";

        /**
         * Char separator for generated path.
         */
        public static final char CHAR_SEPARATOR = File.separatorChar;

        /**
         * Path separator for generated path.
         */
        public static final String PATH_SEPARATOR = File.pathSeparator;

        /**
         * Line separator for generated class.
         */
        private static final String LINE_SEPARATOR = System.lineSeparator();

        /**
         * Class - reserved keyword. Used for generated class.
         */
        private static final String CLASS = "class";

        /**
         * Package - reserved keyword. Used for generated class.
         */
        private static final String PACKAGE = "package";

        /**
         * Return - reserved keyword. Used for generated class.
         */
        private static final String RETURN = "return";

        /**
         * Public - reserved keyword. Used for generated class.
         */
        private static final String PUBLIC = "public";

        /**
         * Super - reserved keyword. Used for generated class.
         */
        private static final String SUPER = "super";

        /**
         * Tab symbol.
         */
        private static final String TAB = "\t";

        /**
         * Delimiter for parameters.
         */
        private static final String PARAMETER_DELIMITER = ", ";

        /**
         * Empty string.
         */
        private static final String EMPTY = "";

        /**
         * Space.
         */
        private static final String SPACE = " ";

        /**
         * Implements - reserved keyword. Used for generated class.
         */
        private static final String IMPLEMENTS = "implements";

        /**
         * Extends - reserved keyword. Used for generated class.
         */
        private static final String EXTENDS = "extends";

        /**
         * Throws - reserved keyword. Used for generated class.
         */
        private static final String THROWS = "throws";

        /**
         * Curly open bracket. Starts code block.
         */
        private static final String CURLY_OPEN_BRACKET = "{";

        /**
         * Curly open bracket. Finishes code block.
         */
        private static final String CURLY_CLOSE_BRACKET = "}";

        /**
         * Open bracket. Starts parameters list.
         */
        private static final String OPEN_BRACKET = "(";

        /**
         * Open bracket. Ends parameters list.
         */
        private static final String CLOSE_BRACKET = ")";

        /**
         * Semicolon for generated class.
         */
        private static final String SEMICOLON = ";";

        /**
         * Returns name for implemented class. For example, suppose token os "{@code IOException}",
         * then invoking this method will result: "{@code IOExceptionImpl}"
         *
         * @param token given class to be generated.
         * @return name for given class token.
         */
        public static String generateClassImplByToken(final Class<?> token) {
            return token.getSimpleName() + IMPL;
        }
    }

}