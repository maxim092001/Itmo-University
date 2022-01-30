import org.jetbrains.kotlin.gradle.plugin.*

buildscript {
    repositories {
        jcenter()
    }
    dependencies {
        classpath("org.jetbrains.kotlinx:atomicfu-gradle-plugin:0.14.1")
    }
}

plugins {
    kotlin("jvm") version "1.3.61"
    java
}

apply(plugin = "kotlinx-atomicfu")

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

tasks {
    test {
        testLogging.showExceptions = true
        testLogging.showStandardStreams = true
    }
}

// This is needed for the current version of AtomicFu that does not support Java 11 bytecode
java {
    sourceCompatibility = JavaVersion.VERSION_1_8
    targetCompatibility = JavaVersion.VERSION_1_8
}
