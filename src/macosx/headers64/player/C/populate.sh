#!/bin/sh
if [ "$(basename "$(pwd)")" = C -a "$(basename "$(dirname "$(pwd)")")" = player ] ; then
    rm -rf Player
    CFLAGS="-m64 -msse2 -fobjc-abi-version=2 -isysroot ${SDK} -mmacosx-version-min=10.5"
    export CFLAGS
    h-to-ffi.sh -D__Types__ -I/Library/Frameworks/MidiShare.framework/Headers/ /Library/Frameworks/Player.framework/Headers/Player.h
else
    echo "Please   cd player/C   before running   sh ./populate.sh"
fi

