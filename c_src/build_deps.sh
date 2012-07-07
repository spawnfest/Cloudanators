#!/bin/bash -e

# Living dangerously
LIBUV_VSN="master"

if [ `basename $PWD` != "c_src" ]; then
    # originally "pushd c_src" of bash
    # but no need to use directory stack push here
    cd c_src
fi

BASEDIR="$PWD"

# detecting gmake and if exists use it
# if not use make
# (code from github.com/tuncer/re2/c_src/build_deps.sh
which gmake 1>/dev/null 2>/dev/null && MAKE=gmake
MAKE=${MAKE:-make}

case "$1" in
    clean)
        rm -rf libuv
        ;;

    test)
        (cd libuv && $MAKE test)

        ;;

    get-deps)
        if [ ! -d libuv ]; then
            git clone git://github.com/joyent/libuv.git
            (cd libuv && git checkout $LIBUV_VSN)
        fi
        ;;

    *)
        if [ ! -d libuv ]; then
            git clone git://github.com/joyent/libuv.git
            (cd libuv && git checkout $LIBUV_VSN)
        fi

        if [ ! -d libuv/build/gyp ]; then
            (cd libuv && svn co http://gyp.googlecode.com/svn/trunk build/gyp)
        fi

        if [ ! -f libuv/.patched ]; then
            (cd libuv && patch -p1 < ../libuv.patch && touch .patched)
        fi

        (cd libuv && ./gyp_uv -f make >/dev/null 2>&1)
        (cd libuv && $MAKE)

        ;;
esac

