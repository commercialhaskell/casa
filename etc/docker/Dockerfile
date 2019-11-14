#fpco/stack-run:lts-5
FROM fpco/stack-run@sha256:4b170ac899755a89c0295b7726c5530015211055125f6e3f6c5b902cb3e9b74b

# RUN export DEBIAN_FRONTEND=noninteractive && \
#     apt-get update && \
#     wget -qO- https://get.haskellstack.org/ | sh -x && \
#     unset DEBIAN_FRONTEND

################################################################################
# Haskell system dependencies (basically never changes)

RUN apt-get update && \
    apt-get install -yq --no-install-suggests --no-install-recommends --force-yes -y -qq \
            netbase ca-certificates xz-utils curl

################################################################################
# Download a specific Stack version

RUN curl https://github.com/commercialhaskell/stack/releases/download/v2.1.3/stack-2.1.3-linux-x86_64-static.tar.gz \
    --silent -L \
    -o stack.tar.gz && \
    tar zxf stack.tar.gz && mv stack-2.1.3-linux-x86_64-static/stack /usr/bin/

###

COPY _artifacts/casa-server /usr/local/bin/casa-server
