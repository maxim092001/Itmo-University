import org.jetbrains.kotlin.gradle.plugin.*

plugins {
    kotlin("jvm") version "1.4.0"
    java
    application
}

group = "ru.ifmo.mpp"
version = "1.0-SNAPSHOT"

repositories {
    mavenCentral()
}

dependencies {
    implementation(kotlin("stdlib-jdk8"))
}

sourceSets["main"].java.setSrcDirs(listOf("src"))
sourceSets["test"].java.setSrcDirs(listOf("test"))

application {
    mainClassName = "VerifyMonotonicClockKt"
}

tasks["build"].dependsOn("run")