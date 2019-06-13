#!/bin/bash

sudo docker run --env-file dist/env-vars.txt -p 8080:8080 -it mmlh-reporter:latest /opt/mmlh-reporter/mmlh-reporter
