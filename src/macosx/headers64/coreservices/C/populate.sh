#!/bin/sh
if [ "$(basename "$(pwd)")" = C -a "$(basename "$(dirname "$(pwd)")")" = coreservices ] ; then
    rm -rf System Developer usr
    CFLAGS="-m64 -fobjc-abi-version=2 -isysroot / -mmacosx-version-min=10.6 "
    export CFLAGS
    h-to-ffi.sh    /System/Library/Frameworks/CoreServices.framework/Headers/CoreServices.h
else
    echo "Please   cd coreservices/C   before running   sh ./populate.sh"
fi
