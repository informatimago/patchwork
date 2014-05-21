
if [ "$(basename "$(pwd)")" = C -a "$(basename "$(dirname "$(pwd)")")" = coregraphics ] ; then
    rm -rf System Developer usr
    # if [ -x /Developer/SDKs/MacOSX10.6.sdk ] ; then
    #     SDK=/Developer/SDKs/MacOSX10.6.sdk
    # else
    #     SDK=/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.8.sdk
    # fi
    SDK=''
    CFLAGS="-m64 -fobjc-abi-version=2 -isysroot ${SDK} -mmacosx-version-min=10.6"
    export CFLAGS
    h-to-ffi.sh ${SDK}/System/Library/Frameworks/CoreGraphics.framework/Headers/CoreGraphics.h
else
    echo "Please   cd coregraphics/C   before running   sh ./populate.sh"
fi
