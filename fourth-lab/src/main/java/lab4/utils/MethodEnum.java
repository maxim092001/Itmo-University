package lab4.utils;

public enum MethodEnum {
    CLASSIC_NEWTON("Classic"), ONE_DIRECTION_NEWTON("One Direction"), DESCEND_NEWTON("Descend");

    public final String name;

    MethodEnum(final String name) {
        this.name = name + " Newton's Method";
    }
}
