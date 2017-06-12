#!/bin/env bash

# Find script parent dir path.
SOURCE="${BASH_SOURCE[0]}"
while [ -h "$SOURCE" ]; do
  DIR="$( cd -P "$( dirname "$SOURCE" )" && pwd )"
  SOURCE="$(readlink "$SOURCE")"
  [[ $SOURCE != /* ]] && SOURCE="$DIR/$SOURCE"
done

CONFIG_DIR="$( cd -P "$( dirname "$SOURCE" )" && pwd )"
CONFIG_SRC=$CONFIG_DIR/src
CONFIG_ASSETS=$CONFIG_DIR/assets

TMP_DIR=/tmp/emacs

PREFIX_BUILD=$TMP_DIR/build
BUILD_PLUGINS=$PREFIX_BUILD/plugins
BUILD_INIT=$PREFIX_BUILD/init.el

PREFIX_GIT=$TMP_DIR/git

DESTINATION_DIR=~/.emacs.d

DESTINATION_ELPA=$DESTINATION_DIR/elpa
DESTINATION_DOC=$DESTINATION_DIR/doc
DESTINATION_PLUGINS=$DESTINATION_DIR/plugin
DESTINATION_SAVE=$DESTINATION_DIR/.save

DESTINATION_INIT=$DESTINATION_DIR/init.el

config_update_plugins () {
    SOURCE_YASNIPPET=https://github.com/joaotavora/yasnippet
    SOURCE_POMIDOR=https://github.com/TatriX/pomidor
    SOURCE_RUN_ASSOC=https://raw.githubusercontent.com/mikae/emacswiki.org/master/run-assoc.el
    SOURCE_HELP_FNSP=https://raw.githubusercontent.com/mikae/emacswiki.org/master/help-fns%2B.el
    SOURCE_LUA_MODE=https://github.com/mikae/lua-mode
    SOURCE_WEB_BEAUTIFY=https://github.com/mikae/web-beautify
    SOURCE_COMPANY_LUA=https://github.com/ptrv/company-lua

    # yasnippet
    git clone $SOURCE_YASNIPPET $PREFIX_GIT/yasnippet
    cp -Rv $PREFIX_GIT/yasnippet $DESTINATION_PLUGINS/yasnippet
    cd $DESTINATION_PLUGINS/yasnippet
    rake

    # pomidor
    git clone $SOURCE_POMIDOR $PREFIX_GIT/pomidor
    cp -Rv $PREFIX_GIT/pomidor $DESTINATION_PLUGINS/pomidor

    # run-assoc
    wget $SOURCE_RUN_ASSOC -O $PREFIX_GIT/run-assoc/run-assoc.el
    cp -Rv $PREFIX_GIT/run-assoc $DESTINATION_PLUGINS

    # help-fns+
    wget $SOURCE_HELP_FNSP -O $PREFIX_GIT/help-fns+.el
    cp -Rv $PREFIX_GIT/help-fns+.el $DESTINATION_PLUGINS

    # lua-mode
    git clone $SOURCE_LUA_MODE $PREFIX_GIT/lua-mode
    cp -Rv $PREFIX_GIT/lua-mode $DESTINATION_PLUGINS/lua-mode

    # web-beautify
    git clone $SOURCE_WEB_BEAUTIFY $PREFIX_GIT/web-beautify
    cp -Rv $PREFIX_GIT/web-beautify $DESTINATION_PLUGINS/web-beautify

    # company-lua
    git clone $SOURCE_COMPANY_LUA $PREFIX_GIT/company-lua
    cp -Rv $PREFIX_GIT/company-lua $DESTINATION_PLUGINS/company-lua
}

config_clean () {
    rm -rfv $TMP_DIR
    rm -rfv $DESTINATION_DIR
}

config_configure () {
    mkdir $TMP_DIR
    mkdir $PREFIX_BUILD
    mkdir $BUILD_PLUGINS

    mkdir $PREFIX_GIT

    mkdir $DESTINATION_DIR
    mkdir $DESTINATION_DOC

    mkdir -p $DESTINATION_PLUGINS
    mkdir -p $DESTINATION_SAVE
}

config_install () {
    for f in $(find $CONFIG_SRC -type f); do
        case $f in
            *)
                if $INSTALL; then
                    relative_name=`realpath --relative-to="$CONFIG_SRC" "$f"`
                    source_path=$CONFIG_SRC/$relative_name
                    destination_path=$DESTINATION_DIR/$relative_name
                    destination_dir="$(dirname $destination_path)"

                    [ ! -d $destination_dir ] && mkdir -p $destination_dir
                    cp -v $source_path $destination_path
                fi
                ;;
        esac
    done
}

usage() {
    echo "Usage:"
    echo "      --clean: wipe old configuration"
    echo "      --install-plugins: install plugins"
    echo "      --install: install configuration files"
    echo "      --all: wipe old configuration, then install plugin files, then install configuration files"

    exit 0
}

CLEAN=false
INSTALL_PLUGINS=false
INSTALL=false

if [[ $# -eq 0 ]]; then
    usage
fi

while [[ $# -gt 0 ]]
do
    key="$1"
    case $key in
        --clean)
            CLEAN=true
        ;;
        --install-plugins)
            INSTALL_PLUGINS=true
        ;;
        --install)
            INSTALL=true
        ;;
        --all)
            CLEAN=true
            INSTALL_PLUGINS=true
            INSTALL=true
        ;;
        --help)
            usage
        ;;
    esac

    shift
done

if $CLEAN; then
    config_clean
fi

config_configure

if $INSTALL_PLUGINS; then
    # install Cask
    curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python

    config_update_plugins
fi

config_install

clear
