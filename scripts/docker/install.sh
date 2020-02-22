#!/bin/sh

set -e

apt-get update
apt-get install --no-install-recommends -yq $*
rm -rf /var/lib/apt/lists/*

