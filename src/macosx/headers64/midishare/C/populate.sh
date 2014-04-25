#!/bin/sh
if [ "$(basename "$(pwd)")" = C -a "$(basename "$(dirname "$(pwd)")")" = midishare ] ; then
    rm -rf MidiShare
    CFLAGS="-m64 -msse2 -fobjc-abi-version=2 -isysroot ${SDK} -mmacosx-version-min=10.5"
    export CFLAGS
    h-to-ffi.sh -D__Types__ /Library/Frameworks/MidiShare.framework/Headers/MidiShare.h
    # h-to-ffi.sh -D__Types__ -I/Library/Frameworks/MidiShare.framework/Headers/ /Library/Frameworks/Player.framework/Headers/Player.h
else
    echo "Please   cd midishare/C   before running   sh ./populate.sh"
fi

