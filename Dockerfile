FROM amazoncorretto:17-alpine
VOLUME /tmp
COPY build/libs/recipe-service-0.0.1-SNAPSHOT.jar app.jar
EXPOSE 8080
ENTRYPOINT ["java", "-Duser.timezone=UTC", "-Xms1280m", "-Xmx1280m", "-jar", "/app.jar", "--spring.profiles.active=prod"]
#ENTRYPOINT ["java", "-Xms1024m", "-Xmx1024m", "-jar", "/app.jar", "--spring.profiles.active=prod"]