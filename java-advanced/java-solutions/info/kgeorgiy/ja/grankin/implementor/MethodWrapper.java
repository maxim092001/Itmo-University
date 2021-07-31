package info.kgeorgiy.ja.grankin.implementor;

import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.Objects;

/**
 * Wrap class for method with custom equals and hashCode.
 */
public class MethodWrapper {

    /**
     * Hash and compared method.
     */
    private final Method method;

    /**
     * Constructor with method.
     *
     * @param method initial method.
     */
    public MethodWrapper(final Method method) {
        this.method = method;
    }

    /**
     * Getter.
     *
     * @return method.
     */
    public Method getMethod() {
        return method;
    }

    /**
     * Comparing via:
     *
     * <ul>
     * <li>{@link Method#getReturnType()}</li>
     * <li>{@link Method#getName()}</li>
     * <li>{@link Method#getParameterTypes()}</li>
     * </ul>
     *
     * @param o object to be compared with.
     * @return {@code true} if equal, {@code false} otherwise
     */
    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }

        if (o == null || getClass() != o.getClass()) {
            return false;
        }

        final MethodWrapper that = (MethodWrapper) o;

        return Objects.equals(method.getReturnType(), that.method.getReturnType()) &&
                method.getName().equals(that.method.getName()) &&
                Arrays.equals(method.getParameterTypes(), that.method.getParameterTypes());
    }

    /**
     * Computing hash by:
     *
     * <ul>
     * <li>{@link Method#getName()}</li>
     * <li>{@link Method#getReturnType()}</li>
     * <li>{@link Method#getParameterTypes()}</li>
     * </ul>
     *
     * @return hash code.
     */
    @Override
    public int hashCode() {
        return Objects.hash(method.getName(), method.getReturnType(), Arrays.hashCode(method.getParameterTypes()));
    }
}
