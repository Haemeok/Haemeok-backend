FROM --platform=linux/amd64 openjdk:17-jdk-slim
VOLUME /tmp
COPY build/libs/recipe-service-0.0.1-SNAPSHOT.jar app.jar
EXPOSE 8080
ENTRYPOINT ["java", "-Xmx512m", "-Xms512m", "-jar", "/app.jar", "--spring.profiles.active=prod"]
#ENTRYPOINT ["java", "-Xmx700m", "-jar", "/app.jar","--spring.profiles.active=prod"]
