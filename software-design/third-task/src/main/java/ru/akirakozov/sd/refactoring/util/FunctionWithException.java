package ru.akirakozov.sd.refactoring.util;

@FunctionalInterface
public interface FunctionWithException<T, R> {
    R apply(T arg) throws Exception;
}
