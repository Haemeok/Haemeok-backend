FROM amazoncorretto:17-alpine

RUN apk add --no-cache \
    ca-certificates \
    bash \
    curl \
    ffmpeg \
    python3 \
    py3-pip \
    nodejs \
    npm \
    deno \
  && python3 -m venv /opt/venv \
  && /opt/venv/bin/pip install --no-cache-dir -U pip setuptools wheel \
  && /opt/venv/bin/pip install --no-cache-dir -U yt-dlp

ENV PATH="/opt/venv/bin:${PATH}"

RUN yt-dlp --version && node --version && deno --version && ffmpeg -version

WORKDIR /app

ARG JAR_FILE=build/libs/*.jar
COPY ${JAR_FILE} /app/app.jar

EXPOSE 8080

ENTRYPOINT ["java", "-Duser.timezone=UTC", "-Xms1024m", "-Xmx1024m", "-jar", "/app/app.jar", "--spring.profiles.active=prod"]