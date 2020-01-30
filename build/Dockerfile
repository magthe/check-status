FROM alpine:edge

RUN echo "http://dl-cdn.alpinelinux.org/alpine/edge/testing" >> /etc/apk/repositories \
    && apk update \
    && apk upgrade \
    && apk add bash coreutils curl docker git lftp nix openssh-client

CMD /bin/bash
