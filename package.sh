#!/usr/bin/env bash

set -euo pipefail

if [ $# -ne 1 ]; then
    echo "USAGE: $0 <package flavor>"
    exit 1
fi

export VERSION=${GITHUB_REF_NAME:-$(git describe --always --tags --dirty=+ --abbrev=6)}

function do_build () {
    CL_SOURCE_REGISTRY=$(pwd) sbcl --dynamic-space-size 4096 --disable-debugger --quit --load package/build.lisp
}

case $1 in
    linux)
        do_build
        linuxdeploy --appimage-extract-and-run --executable=bin/mana-break \
                    --custom-apprun=package/AppRun \
                    --icon-file=package/icon.png \
                    --desktop-file=package/mana-break.desktop \
                    --appdir=appimage $(find bin -name "lib*" -printf "-l%p ")
        cp bin/mana-break appimage/usr/bin
        cp -R Resources appimage/usr
        appimagetool --appimage-extract-and-run --comp xz -g appimage "mana-break-${VERSION}.AppImage"
        ;;

    windows)
        if ! command -v mingw-ldd > /dev/null 2>&1
        then
            echo "Missing mingw-ldd helper binary"
            exit 1
        fi
        do_build
        for binary in bin/*; do
            echo -n "${PATH}" | tr ';' '\0' | \
                xargs -t0 mingw-ldd "$binary" --disable-multiprocessing --dll-lookup-dirs | \
                { grep -v -e 'not found' -e 'system32' || test $? = 1; } | \
                awk -F '=> ' '{ print $2 }' | xargs -I deps cp deps bin/
        done
        makensis package/installer.nsi
        ;;

    macos)
        do_build
        bundle="Mana Break.app"
        contents=$bundle/Contents
        mkdir -p "$contents/MacOS"
        cp -r Resources "$contents"
        cp package/Info.plist "$contents"
        cp package/icon.png "$contents/Resources"
        for library in bin/*.dylib; do
            dylibbundler -of -cd -b -p '@loader_path' -x "$library" -d "$contents/MacOS"
            cp "$library" "$contents/MacOS"
        done

        # https://bugs.launchpad.net/sbcl/+bug/1869401
        replace_fr=$(echo -n  "/opt/local/lib/libzstd.1.dylib" | xxd -ps -c1 | tr -d '\n')
        replace_to=$(echo -en "@loader_path/libzstd.1.dylib\x00\x00" | xxd -ps -c1 | tr -d '\n')
        xxd -ps -c1 bin/mana-break | tr -d '\n' | sed "s/$replace_fr/$replace_to/" | fold -w 2 | xxd -r -p > "$contents/MacOS/mana-break"
        chmod +x "$contents/MacOS/mana-break"

        hdiutil create -quiet -srcfolder "$bundle" out.dmg
        # NOTE: ULMO = lzma compression = Catalina+ only
        hdiutil convert -quiet out.dmg -format ULMO -o "mana-break-${VERSION}.dmg"
        rm out.dmg
        ;;

    *)
        echo "Uknown package flavor: $1"
        exit 1
        ;;
esac
