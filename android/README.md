# Aesthetic Computer - Android App

Native Android wrapper for [aesthetic.computer](https://aesthetic.computer).

## Build Flavors

This project has two build flavors:

| Flavor | App ID | Purpose |
|--------|--------|---------|
| **consumer** | `computer.aesthetic` | Play Store distribution |
| **kiosk** | `computer.aesthetic.kiosk` | Dedicated device installations |

### Consumer Flavor
Simple WebView app that loads `https://aesthetic.computer`. For Google Play Store distribution.

Features:
- Clean WebView wrapper
- Camera/microphone permissions for pieces that need them
- Deep linking for `aesthetic.computer/*` URLs
- Splash screen with purple branding

### Kiosk Flavor  
Device owner mode app for dedicated installations (art exhibits, kiosks, etc).

Features:
- Local HTTPS server with self-signed SSL
- Device admin/owner mode for lock task
- Boot receiver for auto-start
- Blocks back/home buttons
- Hides system UI

## Building

### GitHub Actions (Recommended)

The easiest way to build is via GitHub Actions:

1. **Automatic builds**: Push changes to `android/` on `main` branch
2. **Manual builds**: Go to Actions → "Android Build" → Run workflow
   - Choose `debug` or `release`
   - Choose `consumer`, `kiosk`, or `all`
3. **Download APKs**: From the workflow run artifacts

APKs are available as downloadable artifacts for 30 days (debug) or 90 days (release).

### Local Build Prerequisites
- Java 17+ (`brew install openjdk@17` on Mac)
- Android SDK (via Android Studio or standalone)
- Set `ANDROID_HOME` environment variable
- **Note**: Building in the devcontainer won't work (ARM64 can't run x86_64 Android SDK tools)

### Build Commands

```bash
cd android

# Consumer (Play Store)
./gradlew assembleConsumerDebug      # Debug APK
./gradlew assembleConsumerRelease    # Release APK (needs signing)

# Kiosk (Device installations)
./gradlew assembleKioskDebug
./gradlew assembleKioskRelease

# Build both
./gradlew assembleDebug
./gradlew assembleRelease
```

### Output Locations
```
app/build/outputs/apk/consumer/debug/app-consumer-debug.apk
app/build/outputs/apk/consumer/release/app-consumer-release.apk
app/build/outputs/apk/kiosk/debug/app-kiosk-debug.apk
app/build/outputs/apk/kiosk/release/app-kiosk-release.apk
```

## Testing on Device

### Via USB (Mac → Android device)

1. Enable Developer Mode on Android device:
   - Settings → About Phone → Tap "Build Number" 7 times
   
2. Enable USB Debugging:
   - Settings → Developer Options → USB Debugging → On
   
3. Connect device and accept the debugging prompt

4. Install APK:
```bash
# Install consumer debug build
adb install app/build/outputs/apk/consumer/debug/app-consumer-debug.apk

# Or force reinstall
adb install -r app/build/outputs/apk/consumer/debug/app-consumer-debug.apk
```

### Via Wireless ADB (from devcontainer)

If your Mac host and Android device are on the same network:

1. On device: Settings → Developer Options → Wireless Debugging → Enable
2. Get pairing code from device
3. From Mac terminal:
```bash
adb pair <device-ip>:<pairing-port>  # Enter pairing code
adb connect <device-ip>:<debug-port>
adb install path/to/app.apk
```

## Project Structure

```
android/
├── app/
│   ├── build.gradle.kts      # Build config with product flavors
│   └── src/
│       ├── main/             # Shared code (theme, resources)
│       │   ├── AndroidManifest.xml
│       │   ├── java/computer/aesthetic/ui/theme/
│       │   └── res/          # Icons, colors, strings
│       │
│       ├── consumer/         # Play Store version
│       │   ├── AndroidManifest.xml
│       │   └── java/computer/aesthetic/
│       │       └── MainActivity.kt
│       │
│       └── kiosk/            # Device installation version
│           ├── AndroidManifest.xml
│           ├── java/computer/aesthetic/
│           │   ├── MainActivity.kt
│           │   ├── LocalHttpServer.kt
│           │   ├── BootReceiver.kt
│           │   └── MyDeviceAdminReceiver.kt
│           └── res/
│               ├── raw/keystore.bks
│               └── xml/device_admin.xml
│
├── gradle/                   # Gradle wrapper & version catalog
└── build.gradle.kts          # Root build config
```

## Play Store Release

### 1. Generate Upload Keystore
```bash
keytool -genkey -v -keystore aesthetic-upload.jks -keyalg RSA -keysize 2048 -validity 10000 -alias upload
```

### 2. Configure Signing
Add to `app/build.gradle.kts`:
```kotlin
signingConfigs {
    create("release") {
        storeFile = file("../aesthetic-upload.jks")
        storePassword = System.getenv("KEYSTORE_PASSWORD")
        keyAlias = "upload"
        keyPassword = System.getenv("KEY_PASSWORD")
    }
}

buildTypes {
    release {
        signingConfig = signingConfigs.getByName("release")
        // ...
    }
}
```

### 3. Build Release Bundle
```bash
./gradlew bundleConsumerRelease
# Output: app/build/outputs/bundle/consumerRelease/app-consumer-release.aab
```

### 4. Upload to Play Console
1. Create app in [Google Play Console](https://play.google.com/console)
2. Upload `.aab` file to Internal Testing track
3. Fill out store listing, content rating, privacy policy
4. Promote to Production when ready

## Kiosk Setup (Device Owner Mode)

For dedicated devices that boot directly into the app:

### 1. Factory Reset Device
Start with a fresh device.

### 2. Set Device Owner via ADB
```bash
adb shell dpm set-device-owner computer.aesthetic.kiosk/.MyDeviceAdminReceiver
```

### 3. Install Kiosk APK
```bash
adb install app-kiosk-release.apk
```

The device will now boot directly into Aesthetic Computer.

## Dependencies

- Kotlin 2.0.21
- Jetpack Compose (BOM 2024.12.01)
- AndroidX Core KTX 1.15.0
- AndroidX Splashscreen 1.0.1
- NanoHTTPD 2.3.1 (kiosk only)

## Version History

- **1.1.0** - Build flavor support (consumer vs kiosk)
- **1.0.0** - Initial kiosk-only release
