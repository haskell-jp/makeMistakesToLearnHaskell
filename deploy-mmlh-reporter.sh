#!/bin/sh

heroku container:push web
heroku container:release web
