FROM debian:bullseye

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

COPY paul.gpg /etc/apt/trusted.gpg.d/paul-6507444DBDF4EAD2.gpg
COPY mercury.list /etc/apt/sources.list.d/

# Install a minimal set of Mercury grades
RUN ./install.sh \
    mercury-llc-dev \
    mercury-tools=22.01-bullseye1

#############################
## End of Mercury dockerfile
##

##
## Likewise, this snippet is from the plasma-ci-dep image.
##
# Install some Plasma build dependencies.  We will test with both gcc and
# clang so install both.
RUN ./install.sh \
    exuberant-ctags \
    gcc \
    g++ \
    clang \
    make \
    unzip

# Plasma needs a more recent version of ninja
ADD https://github.com/ninja-build/ninja/releases/download/v1.10.2/ninja-linux.zip .
RUN unzip ninja-linux.zip && mv ninja /usr/local/bin && rm ninja-linux.zip


# Install the extra dependencies / things someone might want for working on
# Plasma.
RUN ./install.sh \
    asciidoc \
    ca-certificates \
    cdecl \
    git \
    less \
    patchutils \
    pinfo \
    procps \
    screen \
    source-highlight \
    tig \
    vim \
    mercury-recommended

# Setup git and vim.
# TODO: should be able to do this with vim packages and without the pathogen
# script.
WORKDIR /root
COPY gitconfig .gitconfig
COPY vimrc .vimrc
RUN mkdir .vim .vim/bundle .vim/autoload
ADD https://raw.githubusercontent.com/tpope/vim-pathogen/master/autoload/pathogen.vim .vim/autoload/pathogen.vim
RUN cp -r /usr/share/doc/mercury-tools/examples/vim .vim/bundle/mercury
RUN git clone --depth 1 https://github.com/PlasmaLang/vim.git .vim/bundle/plasma
# Some of the Mercury vim files may be compressed
RUN find .vim -name \*.gz | xargs gunzip

# Get Plasma.
RUN git clone https://github.com/PlasmaLang/plasma.git

# Update to later git versions.
WORKDIR .vim/bundle/plasma
RUN git remote update -p && git checkout fa0dcf83c496b34db22e255a2cfec3bd3ee97812 && git checkout master && git merge --ff origin/master 
WORKDIR /root/plasma 
RUN git remote update -p && git checkout f25d3d2c0f412b230482b43cf8fafaedaee6b844 && git checkout master && git merge --ff origin/master

WORKDIR /root
RUN apt-get update; apt-get upgrade -yq

# One more tweak to vim.
RUN cp plasma/scripts/do_mmc_make /usr/bin
RUN echo "setlocal makeprg=do_mmc_make" >> .vim/bundle/mercury/ftplugin/mercury.vim

COPY welcome.sh /usr/bin
ENTRYPOINT /usr/bin/welcome.sh

