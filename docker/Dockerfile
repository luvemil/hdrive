FROM ubuntu:20.04

RUN mkdir -p /opt/myapp
WORKDIR /opt/myapp

ENV LANG C.UTF-8
ENV LC_ALL C.UTF-8

RUN apt-get update && apt-get install -y \
  ca-certificates \
  libgmp-dev \
  locales

COPY hdrive-exe .
CMD ["/opt/myapp/hdrive-exe"]