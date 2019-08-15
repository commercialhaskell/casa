FROM fpco/stack-build:lts-13.28 as build
RUN mkdir /opt/build
COPY . /opt/build
RUN cd /opt/build && stack build

FROM ubuntu:19.04
RUN mkdir -p /opt/casa-server
ARG BINARY_PATH
WORKDIR /opt/casa-server
RUN apt-get update && apt-get install -y \
    ca-certificates \
    libgmp-dev \
    libpq-dev
COPY --from=build /opt/build/.stack-work/install/x86_64-linux/*/8.6.5/bin .
CMD ["/opt/casa-server/casa-server"]