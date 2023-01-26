import org.jetbrains.kotlin.gradle.tasks.KotlinCompile

plugins {
    kotlin("jvm") version "1.7.10"
    id("io.spring.dependency-management") version "1.0.11.RELEASE"
    id("org.springframework.boot") version "2.6.2"
    kotlin("plugin.spring") version "1.6.10"
}

group = "me.maximgran"
version = "1.0-SNAPSHOT"

repositories {
    mavenCentral()
}

dependencies {
    testImplementation(kotlin("test"))
    implementation("org.springframework.boot:spring-boot-starter")
    implementation("org.jetbrains.kotlin:kotlin-reflect")
    implementation("org.jetbrains.kotlin:kotlin-stdlib-jdk8")
    testImplementation("org.springframework.boot:spring-boot-starter-test")
    implementation("org.springframework:spring-aop:5.2.0.RELEASE")
    implementation("org.aspectj:aspectjrt:1.6.11")
    implementation("org.aspectj:aspectjweaver:1.6.11")
    implementation("org.springframework:spring-context:5.2.0.RELEASE")
    implementation("de.vandermeer:asciitable:0.3.2")
}

tasks.test {
    useJUnit()
}

tasks.withType<KotlinCompile> {
    kotlinOptions {
        freeCompilerArgs = listOf("-Xjsr305=strict")
        jvmTarget = "11"
    }
}