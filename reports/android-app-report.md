# Android App Report: Aesthetic Computer for Play Store

## Executive Summary

**Great news!** You already have a working Android app with an APK built and ready. The current implementation is designed as a **kiosk mode app** (for dedicated devices), but converting it to a simple consumer app for the Play Store is straightforward.

---

## Current State Analysis

### ✅ What You Already Have

| Component | Status | Location |
|-----------|--------|----------|
| Android project structure | ✅ Complete | [android/](android/) |
| Kotlin + Jetpack Compose | ✅ Configured | Modern Android stack |
| WebView-based app | ✅ Working | Loads web content |
| App icons | ✅ Present | All mipmap densities |
| Build scripts | ✅ Gradle 8.7.3 | Kotlin DSL |
| **Release APK** | ✅ Built | [android/app/release/app-release.apk](android/app/release/app-release.apk) (6.7MB) |
| App ID | ✅ Set | `computer.aesthetic` |

### 🏗️ Architecture Match

Your existing platforms all use the same pattern:

| Platform | Approach | URL |
|----------|----------|-----|
| **iOS App** | WKWebView → aesthetic.computer | Native Swift |
| **Electron (macOS)** | BrowserWindow → aesthetic.computer | Node.js |
| **Android (existing)** | WebView → localhost:8443 | Kotlin |

The Android app is nearly identical to iOS/Electron - just needs URL change!

---

## Current Android App Issues for Play Store

The existing app is configured for **kiosk mode** (dedicated display installations). For a consumer app:

### 1. **Remove Kiosk/Device Admin Features**

Current [MainActivity.kt](android/app/src/main/java/computer/aesthetic/MainActivity.kt) has:
- Device Owner/Admin mode (locks device to app)
- Lock task mode (prevents exiting)
- System UI hiding
- Back button blocking

**Fix:** Remove these for a normal user app.

### 2. **Point to Live URL**

Current: `https://localhost:8443` (local HTTPS server with self-signed cert)  
**Fix:** `https://aesthetic.computer`

### 3. **Remove Local HTTPS Server**

The [localHttpServer.kt](android/app/src/main/java/computer/aesthetic/localHttpServer.kt) runs NanoHTTPD with SSL for local content. Not needed for a web wrapper.

### 4. **Remove Unnecessary Permissions**

Current permissions in [AndroidManifest.xml](android/app/src/main/AndroidManifest.xml):
```xml
<uses-permission android:name="android.permission.RECEIVE_BOOT_COMPLETED" />
<uses-permission android:name="android.permission.BIND_DEVICE_ADMIN"/>
<uses-permission android:name="android.permission.KIOSK_MODE"/>
```

**For consumer app, only need:**
```xml
<uses-permission android:name="android.permission.INTERNET" />
```

### 5. **Fix Launcher Intent**

Current manifest sets app as device launcher (HOME category). Remove for normal app.

---

## Implementation Plan

### Phase 1: Simple WebView App (1-2 hours)

Create a clean consumer version:

1. **Simplify MainActivity.kt** - Just load `https://aesthetic.computer` in WebView
2. **Clean AndroidManifest.xml** - Remove kiosk intents, keep INTERNET permission
3. **Remove unused files:**
   - `localHttpServer.kt`
   - `BootReceiver.kt`  
   - `MyDeviceAdminReceiver.kt`
   - SSL keystore from `res/raw/`

### Phase 2: Play Store Prep (2-4 hours)

1. **App signing** - Generate upload keystore for Play Store
2. **Version bumping** - Increment `versionCode` and `versionName`
3. **Privacy policy** - Required for Play Store (you have one at [privacy-policy.html](system/public/privacy-policy.html))
4. **Screenshots** - Capture from Uniherz Jelly for store listing
5. **Play Console setup** - Create developer account ($25 one-time) if not already

### Phase 3: Enhancements (Optional)

- **Camera/Mic permissions** - For pieces that use them
- **JavaScript bridge** - Like iOS app has for notifications
- **Splash screen** - Match the purple aesthetic
- **Deep links** - Handle `aesthetic.computer/*` URLs

---

## Code Changes Summary

### Minimal MainActivity.kt

```kotlin
package computer.aesthetic

import android.os.Bundle
import android.webkit.WebView
import android.webkit.WebViewClient
import androidx.activity.ComponentActivity
import androidx.activity.compose.setContent
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.viewinterop.AndroidView
import computer.aesthetic.ui.theme.AestheticComputerTheme

class MainActivity : ComponentActivity() {
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContent {
            AestheticComputerTheme {
                WebViewPage(url = "https://aesthetic.computer")
            }
        }
    }
}

@Composable
fun WebViewPage(url: String) {
    AndroidView(
        factory = { context ->
            WebView(context).apply {
                settings.javaScriptEnabled = true
                settings.domStorageEnabled = true
                settings.mediaPlaybackRequiresUserGesture = false
                webViewClient = WebViewClient()
                loadUrl(url)
            }
        },
        modifier = Modifier.fillMaxSize()
    )
}
```

### Minimal AndroidManifest.xml

```xml
<?xml version="1.0" encoding="utf-8"?>
<manifest xmlns:android="http://schemas.android.com/apk/res/android">

    <uses-permission android:name="android.permission.INTERNET" />

    <application
        android:allowBackup="true"
        android:icon="@mipmap/ic_launcher"
        android:label="@string/app_name"
        android:roundIcon="@mipmap/ic_launcher_round"
        android:supportsRtl="true"
        android:theme="@style/Theme.AestheticComputer">

        <activity
            android:name=".MainActivity"
            android:exported="true"
            android:configChanges="orientation|screenSize">
            <intent-filter>
                <action android:name="android.intent.action.MAIN" />
                <category android:name="android.intent.category.LAUNCHER" />
            </intent-filter>
        </activity>

    </application>
</manifest>
```

---

## Building & Testing

### Build Debug APK (for testing on Uniherz Jelly)

```bash
cd android
./gradlew assembleDebug
# Output: app/build/outputs/apk/debug/app-debug.apk
```

### Install to Device via ADB

```bash
adb install app/build/outputs/apk/debug/app-debug.apk
```

### Build Release APK (for Play Store)

```bash
cd android
./gradlew assembleRelease
# Sign with your keystore
```

---

## Reusable Assets from Monorepo

| Asset | Source | Use |
|-------|--------|-----|
| App icons | Already in `android/app/src/main/res/mipmap-*/` | ✅ Ready |
| Privacy policy | [system/public/privacy-policy.html](system/public/privacy-policy.html) | Link in Play Store |
| Web app | `https://aesthetic.computer` | Main content |
| Branding | Purple theme already configured | ✅ Ready |

---

## Play Store Requirements Checklist

- [ ] Google Play Developer Account ($25 one-time fee)
- [ ] App signing keystore (generate new or use existing)
- [ ] Privacy policy URL
- [ ] App screenshots (phone + tablet if supporting)
- [ ] Feature graphic (1024x500)
- [ ] App icon (512x512) - you have `ic_launcher-playstore.png`
- [ ] Short description (80 chars)
- [ ] Full description (4000 chars)
- [ ] Content rating questionnaire
- [ ] Target audience declaration
- [ ] Data safety form

---

## Estimated Timeline

| Task | Time |
|------|------|
| Code cleanup (remove kiosk mode) | 1 hour |
| Test on Uniherz Jelly | 30 min |
| Play Console setup | 30 min |
| Store listing creation | 1-2 hours |
| Review & publish | 1-7 days (Google review) |

**Total: ~4 hours of work + Google review time**

---

## Next Steps

1. **Shall I create a clean consumer version** of the Android app now?
2. **Do you have a Google Play Developer account?** If not, you'll need to create one at https://play.google.com/console
3. **Connect Uniherz Jelly via USB** for testing - I can guide you through ADB setup

---

## Alternative: PWA Approach

The aesthetic.computer website already works as a PWA (Progressive Web App). Android users can "Add to Home Screen" from Chrome without needing a Play Store app. However, a native app:

- Shows up in Play Store searches
- Feels more "official"
- Gets automatic updates
- Can access native features (notifications, etc.)

Both approaches are valid - the Play Store app is better for discoverability.
