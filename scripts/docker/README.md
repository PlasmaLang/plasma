
Build / get a docker image
==========================

This directory contains a Dockerfile to setup an environment
for developing Plasma.  To build the image edit build.sh and then execute
it: 

    $ vim build.sh
    $ ./build.sh

Or download it from [docker
hub](https://hub.docker.com/r/paulbone/plasma-dep) with

    $ docker pull paulbone/plasma-dep

More details
============

The files in this directory do the following:

README.md:  You're reading it.
build.sh:   A script to ask docker to build the image.
Dockerfile: The Dockerfile (docker's script) to build the image.

These files are part of the docker image:

gitconfig:    A basic ~/.gitconfig
install.sh:   A script to call apt within docker
mercury.list: An apt sources list for Mercury
paul.gpg:     Paul's GPG key (for signed Mercury packages)
vimrc:        A suitable .vimrc with this and options in the docker file
              syntax highlighting will be available for Plasma and Mercury.
welcome.sh:   A greeting when opening the docker image.

