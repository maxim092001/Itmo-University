FROM openjdk:15-alpine as build-stage
WORKDIR /srv     
COPY .mvn ./.mvn
COPY mvnw pom.xml ./
RUN ./mvnw dependency:go-offline
COPY . .
RUN ./mvnw clean package

FROM openjdk:15-alpine as run-stage
WORKDIR /srv
COPY --from=build-stage /srv/target/optimization-methods-lab1.jar optimization-methods-lab1.jar
EXPOSE 8090
CMD ["java", "-Xmx1024m", "-jar", "optimization-methods-lab1.jar"]
