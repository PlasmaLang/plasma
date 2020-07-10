FROM debian:buster

##
## The first part of this dockerfile is identical to the Mercury one
## https://github.com/Mercury-Language/packaging/tree/master/docker/min-rotd
## To improve caching
#####################################
RUN apt-get update; apt-get upgrade -yq

WORKDIR /tmp
COPY install.sh .

# Install some mercury dependencies, this creates another docker layer
# allowing some caching.

RUN ./install.sh \
    gcc \
    libhwloc-dev \
    libreadline-dev \
    perl

RUN ./install.sh gnupg
COPY paul.asc .
RUN apt-key add paul.asc
COPY mercury.list /etc/apt/sources.list.d/

# Install a minimal set of Mercury grades
RUN ./install.sh \
    mercury-rotd-llc-dev \
    mercury-rotd-tools=20200531-1

#############################
## End of Mercury dockerfile
##

##
## Likewise, this snippet is from the plasma-ci-dep image.
##
# Install some Plasma build dependencies.  We will test with both gcc and
# clang so install both.
RUN ./install.sh \
    ctags \
    gcc \
    g++ \
    clang \
    make



# Install the extra dependencies / things someone might want for working on
# Plasma.
RUN ./install.sh \
    asciidoc \
    ca-certificates \
    cdecl \
    git \
    patchutils \
    pinfo \
    procps \
    screen \
    source-highlight \
    tig \
    vim \
    mercury-rotd-recommended

# Setup git and vim.
# TODO: should be able to do this with vim packages and without the pathogen
# script.
WORKDIR /root
COPY gitconfig .gitconfig
COPY vimrc .vimrc
RUN mkdir .vim .vim/bundle .vim/autoload
ADD https://raw.githubusercontent.com/tpope/vim-pathogen/master/autoload/pathogen.vim .vim/autoload/pathogen.vim
RUN cp -r /usr/share/doc/mercury-rotd-tools/examples/vim .vim/bundle/mercury
RUN git clone --depth 1 https://github.com/PlasmaLang/vim.git .vim/bundle/plasma
# Some of the Mercury vim files may be compressed
RUN find .vim -name \*.gz | xargs gunzip

# Get Plasma.
RUN git clone https://github.com/PlasmaLang/plasma.git

# One more tweak to vim.
RUN cp plasma/scripts/do_mmc_make /usr/bin
RUN echo "setlocal makeprg=do_mmc_make" >> .vim/bundle/mercury/ftplugin/mercury.vim

COPY welcome.sh /usr/bin
ENTRYPOINT /usr/bin/welcome.sh

