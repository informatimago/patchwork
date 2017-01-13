#!/bin/sh
if [ "$(basename "$(pwd)")" = C -a "$(basename "$(dirname "$(pwd)")")" = midishare ] ; then
    rm -rf System Developer usr
    CFLAGS="-m32 -msse2 -fobjc-abi-version=2 -isysroot / -mmacosx-version-min=10.6 -D__Types__=1"
    export CFLAGS
    h-to-ffi.sh    /Library/Frameworks/MidiShare.framework/Headers/MidiShare.h
else
    echo "Please   cd midishare/C   before running   sh ./populate.sh"
fi
