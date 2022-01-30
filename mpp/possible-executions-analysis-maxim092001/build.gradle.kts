import org.jetbrains.kotlin.gradle.plugin.*

plugins {
    kotlin("jvm") version "1.4.0"
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

sourceSets["main"].withConvention(KotlinSourceSet::class) {
    kotlin.srcDir("src")
}

application {
    mainClassName = "PossibleExecutionsVerifierKt"
}

tasks["build"].dependsOn("run")