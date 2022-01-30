buildscript {
    repositories {
        mavenCentral()
    }
    dependencies {
        classpath("org.jetbrains.kotlinx:atomicfu-gradle-plugin:0.17.0")
    }
}

plugins {
    kotlin("jvm") version "1.6.0"
    java
}

tasks {
    test {
        maxHeapSize = "4g"
    }
}

apply(plugin = "kotlinx-atomicfu")

group = "ru.ifmo.mpp"
version = "1.0-SNAPSHOT"

repositories {
    mavenCentral()
}

dependencies {
    implementation(kotlin("stdlib-jdk8"))
    implementation(kotlin("reflect"))
    testImplementation(kotlin("test-junit"))
    testImplementation("org.jetbrains.kotlinx:lincheck:2.14.1")
}

sourceSets["main"].java.setSrcDirs(listOf("src"))
sourceSets["test"].java.setSrcDirs(listOf("test"))

java {
    sourceCompatibility = JavaVersion.VERSION_1_8
    targetCompatibility = JavaVersion.VERSION_1_8
}