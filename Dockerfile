FROM debian:stable-slim

RUN apt-get update -y && apt-get install libgmp10 git -y && apt-get clean -y
RUN mkdir -p /opt/mmlh-reporter/

ENV LANG C.UTF-8
ENV LANGUAGE C.UTF-8
ENV LC_ALL C.UTF-8

# NOTE: The executable file must be built on the same distribution. I omit to build on the container!
COPY ./dist/mmlh-reporter /opt/mmlh-reporter/

WORKDIR /opt/mmlh-reporter/

CMD /opt/mmlh-reporter/mmlh-reporter
