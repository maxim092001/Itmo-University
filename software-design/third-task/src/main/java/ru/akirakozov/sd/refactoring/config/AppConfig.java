package ru.akirakozov.sd.refactoring.config;

public class AppConfig {

    private final DatabaseConfig databaseConfig;
    private final ServerConfig serverConfig;

    public AppConfig(final DatabaseConfig databaseConfig, final ServerConfig serverConfig) {
        this.databaseConfig = databaseConfig;
        this.serverConfig = serverConfig;
    }

    public String getDatabaseUrl() {
        return databaseConfig.getUrl();
    }

    public int getServerPort() {
        return serverConfig.getPort();
    }

    public DatabaseConfig getDatabaseConfig() {
        return databaseConfig;
    }

    public ServerConfig getServerConfig() {
        return serverConfig;
    }
}
