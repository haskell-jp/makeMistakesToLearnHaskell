FROM phadej/ghc:8.4.2-ubuntu

# <stdout>: commitAndReleaseBuffer: invalid argument (invalid character)
# 上記のエラーを解決するために以下の設定をコピペしてきた。
# https://github.com/haskell-works/stack-build/blob/master/minimal/Dockerfile
ENV LANG=C.UTF-8 \
    LC_ALL=C.UTF-8 \
    MMLH_DOCKER_MODE=1

RUN apt-get update                                                                  && \
    apt-get -y install locales curl                                                 && \
    locale-gen en_US.UTF-8 && curl -sSL https://get.haskellstack.org/ | sh -s - -f  && \
    rm -rf /var/lib/apt/lists/*

RUN apt-get update                                                          && \
    apt-get -y install libsnappy1v5 libtinfo-dev libsnappy-dev jq           && \
    apt-get -y install liblzma5 liblzma-dev                                 && \
    apt-get -y install libz3-dev                                            && \
    rm -rf /var/lib/apt/lists/*