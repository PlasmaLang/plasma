#!/bin/sh

set -e

docker build -t paulbone/plasma-dep:rotd -t paulbone/plasma-dep:latest .
docker push paulbone/plasma-dep:rotd
docker push paulbone/plasma-dep:latest

