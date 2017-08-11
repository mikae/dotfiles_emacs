#!/usr/bin/env bash

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

DESTINATION_DIR=~/.emacs.d

usage() {
    echo "Usage:"
    echo "      --clean: wipe old configuration"
    echo "      --install-plugins: install plugins"
    echo "      --install: install configuration files"
    echo "      --all: wipe old configuration, then install plugin files, then install configuration files"
    echo "      --test-all: run tests"
    echo "      --test-<target>: run target"

    exit 0
}

CLEAN=false
INSTALL=false
TEST=false
declare -a TEST_TARGETS='()'

if [ $# -eq 0 ]; then
    usage
fi

while [ $# -gt 0 ]
do
    key="$1"
    case $key in
        --clean)
            CLEAN=true
        ;;
        --install)
            INSTALL=true
        ;;
        --test)
            TEST=true
        ;;
        --all)
            CLEAN=true
            INSTALL=true
        ;;
        --help)
            usage
        ;;
    esac

    shift
done

if $TEST; then
    emacs -batch -l ert -L $CONFIG_DIR/src/core -l $CONFIG_DIR/test/core/core--execution.el -f ert-run-tests-batch-and-exit
    exit 0
fi

if $CLEAN; then
    rm -rfv $DESTINATION_DIR
fi

if $INSTALL; then
    mkdir $DESTINATION_DIR

    mkdir -p $DESTINATION_DIR/plugin
    mkdir -p $DESTINATION_DIR/lib
    mkdir -p $DESTINATION_DIR/.save

    # install Cask
    curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python

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
fi

#clear
