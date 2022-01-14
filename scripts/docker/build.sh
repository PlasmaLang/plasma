#!/bin/sh

set -e

# Set this to your dockerhub name.
USER=paulbone

# The name of the docker image
IMAGE=plasma-dep

# The version string
VERSION=latest

docker build -t $USER/$IMAGE:$VERSION .

# Comment these out if you don't wish to upload the image.
docker push $USER/$IMAGE:$VERSION

