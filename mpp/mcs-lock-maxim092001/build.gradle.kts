import org.jetbrains.kotlin.gradle.plugin.*

plugins {
    kotlin("jvm") version "1.6.0"
    java
}

group = "ru.ifmo.mpp"
version = "1.0-SNAPSHOT"

repositories {
    mavenCentral()
}

dependencies {
    implementation(kotlin("stdlib-jdk8"))
    testImplementation(kotlin("test-junit"))
}

sourceSets {
    main {
        java.setSrcDirs(listOf("src"))
        withConvention(KotlinSourceSet::class) {
            kotlin.setSrcDirs(listOf("src"))
        }
    }
    test {
        withConvention(KotlinSourceSet::class) {
            kotlin.setSrcDirs(listOf("test"))
        }
    }
}
