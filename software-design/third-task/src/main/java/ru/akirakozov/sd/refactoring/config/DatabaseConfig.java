package ru.akirakozov.sd.refactoring.config;

import com.typesafe.config.Config;

public class DatabaseConfig {
    private final String url;

    public DatabaseConfig(final String url) {
        this.url = url;
    }

    public String getUrl() {
        return url;
    }

   public static DatabaseConfig fromConfig(final Config config) {
        return new DatabaseConfig(config.getString("url"));
   }
}
