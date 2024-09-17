FROM gradle:jdk18-alpine as build

COPY --chown=gradle:gradle . /home/gradle/src
WORKDIR /home/gradle/src
RUN gradle clean shadowJar --no-daemon

FROM eclipse-temurin:18-jre-jammy

## Install PlantUML
RUN apt-get update
RUN apt install -y graphviz

RUN mkdir /app
RUN curl https://github.com/plantuml/plantuml/releases/download/v1.2024.7/plantuml-1.2024.7.jar -L --output /app/plantuml.jar
COPY --from=build /home/gradle/src/build/libs/*.jar /app/struct-c4-layout.jar
COPY ./docker/exporter.sh /app/exporter.sh

ENTRYPOINT ["/app/exporter.sh"]