#!/bin/sh
# Build rax-vr.apk: the native OpenXR app. NDK clang for the one C++ file, then the SDK's own tools for the apk.
#   rax/quest/native/build.sh [install]
# Needs under $ANDROID_HOME: build-tools, a platform, an ndk; and third/loader (the Khronos OpenXR loader's .aar, unzipped).
set -e
cd "$(dirname "$0")"
export JAVA_HOME="${JAVA_HOME:-/opt/homebrew/opt/openjdk}"
PATH="$JAVA_HOME/bin:$PATH"
SDK="${ANDROID_HOME:-/opt/homebrew/share/android-commandlinetools}"
BT="$(ls -d "$SDK"/build-tools/* | sort | tail -1)"
JAR="$(ls -d "$SDK"/platforms/android-* | sort | tail -1)/android.jar"
NDK="$(ls -d "$SDK"/ndk/* | sort | tail -1)"
CC="$(ls -d "$NDK"/toolchains/llvm/prebuilt/*/bin | head -1)"
GLUE="$NDK/sources/android/native_app_glue"
XR=third/loader
[ -f "$XR/jni/arm64-v8a/libopenxr_loader.so" ] || { echo "no OpenXR loader: unzip openxr_loader_for_android-*.aar (Maven Central, org.khronos.openxr) into $XR"; exit 1; }
rm -rf out && mkdir -p out/classes out/dex out/pkg/lib/arm64-v8a
"$CC/aarch64-linux-android29-clang" -c -O2 -fPIC -I "$GLUE" "$GLUE/android_native_app_glue.c" -o out/glue.o
"$CC/aarch64-linux-android29-clang++" -shared -fPIC -O2 -std=c++17 -Wall -Wno-unused-function \
  -I "$GLUE" -I "$XR/prefab/modules/headers/include" main.cpp out/glue.o \
  -L "$XR/jni/arm64-v8a" -lopenxr_loader -landroid -llog -lEGL -lGLESv3 -static-libstdc++ \
  -u ANativeActivity_onCreate -o out/pkg/lib/arm64-v8a/libraxnative.so
cp "$XR/jni/arm64-v8a/libopenxr_loader.so" out/pkg/lib/arm64-v8a/
"$BT/aapt2" compile --dir res -o out/res.zip
"$BT/aapt2" link -o out/base.apk --manifest AndroidManifest.xml -I "$JAR" out/res.zip
javac -source 11 -target 11 -nowarn -cp "$JAR" -d out/classes $(find src -name '*.java')
"$BT/d8" --release --lib "$JAR" --output out/dex $(find out/classes -name '*.class')
(cd out/dex && zip -q ../base.apk classes.dex)
(cd out/pkg && zip -q -r ../base.apk lib)
"$BT/zipalign" -f 4 out/base.apk out/aligned.apk
KEY="$HOME/.config/rax/quest-debug.keystore"
[ -f "$KEY" ] || { mkdir -p "$(dirname "$KEY")"; keytool -genkeypair -keystore "$KEY" -storepass android -keypass android -alias rax -keyalg RSA -keysize 2048 -validity 10000 -dname "CN=rax debug"; }
"$BT/apksigner" sign --ks "$KEY" --ks-pass pass:android --key-pass pass:android --out out/rax-vr.apk out/aligned.apk
ls -l out/rax-vr.apk
if [ "$1" = install ]; then
  adb install -r out/rax-vr.apk
  adb shell am start -n computer.aesthetic.raxvr/.RaxNativeActivity
fi
