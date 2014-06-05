#!/bin/sh
if [ "$(basename "$(pwd)")" = C -a "$(basename "$(dirname "$(pwd)")")" = player ] ; then
    rm -rf System Developer usr
    CFLAGS="-m64 -fobjc-abi-version=2 -isysroot / -mmacosx-version-min=10.6 -D__Types__=1"
    export CFLAGS
    h-to-ffi.sh  -I/Library/Frameworks/MidiShare.framework/Headers/  /Library/Frameworks/Player.framework/Headers/Player.h
else
    echo "Please   cd player/C   before running   sh ./populate.sh"
fi
