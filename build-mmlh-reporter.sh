#!/bin/bash

set -eu

stack --local-bin-path=./dist/ install mmlh-reporter:mmlh-reporter
sudo docker build --network=host -tmmlh-reporter:latest .
