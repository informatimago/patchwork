
if [ "$(basename "$(pwd)")" = C -a "$(basename "$(dirname "$(pwd)")")" = player ] ; then
    rm -rf System Developer usr
    # if [ -x /Developer/SDKs/MacOSX10.6.sdk ] ; then
    #     SDK=/Developer/SDKs/MacOSX10.6.sdk
    # else
    #     SDK=/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.8.sdk
    # fi
    SDK=''
    CFLAGS="-m32 -msse2 -fobjc-abi-version=2 -isysroot ${SDK} -mmacosx-version-min=10.6"
    export CFLAGS
    h-to-ffi.sh ${SDK}/System/Library/Frameworks/Player.framework/Headers/Player.h
else
    echo "Please   cd player/C   before running   sh ./populate.sh"
fi
