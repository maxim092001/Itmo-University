package ru.akirakozov.sd.refactoring.config;

import com.typesafe.config.Config;

public class ServerConfig {

    private final int port;

    public ServerConfig(final int port) {
        this.port = port;
    }

    public int getPort() {
        return port;
    }

    public static ServerConfig fromConfig(final Config config) {
       return new ServerConfig(config.getInt("port"));
    }
}
