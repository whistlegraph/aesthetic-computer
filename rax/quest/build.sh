#!/bin/sh
# Build rax-quest.apk with the Android SDK's own tools: no Gradle, no Studio.
#   rax/quest/build.sh            → out/rax-quest.apk (signed with a debug key made here)
#   rax/quest/build.sh install    → …and put it on the headset over adb, and start it
# Needs: a JDK (Homebrew's openjdk), and under $ANDROID_HOME build-tools and one platform
# (sdkmanager "build-tools;34.0.0" "platforms;android-34").
set -e
cd "$(dirname "$0")"
export JAVA_HOME="${JAVA_HOME:-/opt/homebrew/opt/openjdk}"
PATH="$JAVA_HOME/bin:$PATH"
SDK="${ANDROID_HOME:-/opt/homebrew/share/android-commandlinetools}"
BT="$(ls -d "$SDK"/build-tools/* | sort | tail -1)"
JAR="$(ls -d "$SDK"/platforms/android-* | sort | tail -1)/android.jar"
rm -rf out && mkdir -p out/classes out/dex
"$BT/aapt2" link -o out/base.apk --manifest AndroidManifest.xml -I "$JAR"
javac -source 11 -target 11 -nowarn -cp "$JAR" -d out/classes $(find src -name '*.java')
"$BT/d8" --release --lib "$JAR" --output out/dex $(find out/classes -name '*.class')
(cd out/dex && zip -q ../base.apk classes.dex)
"$BT/zipalign" -f 4 out/base.apk out/aligned.apk
KEY="$HOME/.config/rax/quest-debug.keystore"
[ -f "$KEY" ] || { mkdir -p "$(dirname "$KEY")"; keytool -genkeypair -keystore "$KEY" -storepass android -keypass android -alias rax -keyalg RSA -keysize 2048 -validity 10000 -dname "CN=rax debug"; }
"$BT/apksigner" sign --ks "$KEY" --ks-pass pass:android --key-pass pass:android --out out/rax-quest.apk out/aligned.apk
ls -l out/rax-quest.apk
if [ "$1" = install ]; then
  adb install -r out/rax-quest.apk
  adb shell am start -n computer.aesthetic.rax/.RaxActivity
fi
