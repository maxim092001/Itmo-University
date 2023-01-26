package ru.akirakozov.sd.refactoring.config;

import com.typesafe.config.Config;
import com.typesafe.config.ConfigFactory;

public class ConfigReader {

    private final Config conf;

    public ConfigReader(final String resourceBasename, final String appName) {
        this.conf = ConfigFactory.load(resourceBasename).getConfig(appName);
    }

    public DatabaseConfig getDatabaseConfig(final String env) {
        final var config = conf.getConfig(env);
        return DatabaseConfig.fromConfig(config.getConfig("db"));
    }

    public ServerConfig getServerConfig(final String env) {
        final var config = conf.getConfig(env);
        return ServerConfig.fromConfig(config.getConfig("server"));
    }

    public AppConfig getAppConfig(final String env) {
        return new AppConfig(getDatabaseConfig(env), getServerConfig(env));
    }
}
