
# Scripts

## do\_mmc\_make

The [do_mmc_make](do_mmc_make) script in this directory can be placed in
the path and will run the [.vim_mmc_make](../src/.vim_mmc_make) script
from the current directory.
Such as the one in the `src` directory.  This makes it easy to use `mmc
--make` from vim by pressing F9 with the following configuration change.
Make this change to the `ftplugin/mercury.vim` file of Mercury's vim plugin.

    setlocal makeprg=do_mmc_make

## docker

The [docker](docker) directory contains a Dockerfile to setup an environment
for developing Plasma.  You can build the image for yourself or download it
from [docker hub](https://hub.docker.com/r/paulbone/plasma-dep) with

    docker pull paulbone/plasma-dep

