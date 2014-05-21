#!/bin/sh
if [ "$(basename "$(pwd)")" = C -a "$(basename "$(dirname "$(pwd)")")" = coregraphics ] ; then
    rm -rf System Developer usr
    CFLAGS="-m32 -msse2 -fobjc-abi-version=2 -isysroot / -mmacosx-version-min=10.6 "
    export CFLAGS
    h-to-ffi.sh    /System/Library/Frameworks/CoreGraphics.framework/Headers/CoreGraphics.h
else
    echo "Please   cd coregraphics/C   before running   sh ./populate.sh"
fi
