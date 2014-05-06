
if [ "$(basename "$(pwd)")" = C -a "$(basename "$(dirname "$(pwd)")")" = player ] ; then
    rm -rf System Developer usr
    SDK=/Developer/SDKs/MacOSX10.6.sdk
    CFLAGS="-m32 -msse2 -fobjc-abi-version=2 -isysroot ${SDK} -mmacosx-version-min=10.6"
    export CFLAGS
    h-to-ffi.sh ${SDK}/System/Library/Frameworks/Player.framework/Headers/Player.h
else
    echo "Please   cd player/C   before running   sh ./populate.sh"
fi
