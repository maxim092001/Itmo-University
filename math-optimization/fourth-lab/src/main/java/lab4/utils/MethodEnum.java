package lab4.utils;

public enum MethodEnum {
    CLASSIC_NEWTON("Classic Newton's Method"), ONE_DIRECTION_NEWTON("One Direction Newton's Method"), DESCEND_NEWTON("Descend Newton's Method"),
    DAVIDON("Davidon-Fletcher-Powell Method"), POWELL("Powell Method");

    public final String name;

    MethodEnum(final String name) {
        this.name = name;
    }
}
