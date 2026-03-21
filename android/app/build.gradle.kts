plugins {
    alias(libs.plugins.android.application)
    alias(libs.plugins.kotlin.android)
    alias(libs.plugins.kotlin.compose)
}

android {
    namespace = "computer.aesthetic"
    compileSdk = 35

    defaultConfig {
        applicationId = "computer.aesthetic"
        minSdk = 24
        targetSdk = 35
        versionCode = 2
        versionName = "1.1.0"

        testInstrumentationRunner = "androidx.test.runner.AndroidJUnitRunner"
    }

    // Product flavors: consumer (Play Store) vs kiosk (device installations)
    flavorDimensions += "mode"
    productFlavors {
        create("consumer") {
            dimension = "mode"
            applicationIdSuffix = ""  // computer.aesthetic
            versionNameSuffix = ""
            // Consumer-specific build config
            buildConfigField("String", "BASE_URL", "\"https://aesthetic.computer\"")
            buildConfigField("Boolean", "KIOSK_MODE", "false")
        }
        create("kiosk") {
            dimension = "mode"
            applicationIdSuffix = ".kiosk"  // computer.aesthetic.kiosk
            versionNameSuffix = "-kiosk"
            // Kiosk-specific build config
            buildConfigField("String", "BASE_URL", "\"https://localhost:8443\"")
            buildConfigField("Boolean", "KIOSK_MODE", "true")
        }
    }

    buildTypes {
        release {
            isMinifyEnabled = false
            proguardFiles(
                getDefaultProguardFile("proguard-android-optimize.txt"),
                "proguard-rules.pro"
            )
        }
    }
    compileOptions {
        sourceCompatibility = JavaVersion.VERSION_17
        targetCompatibility = JavaVersion.VERSION_17
    }
    kotlinOptions {
        jvmTarget = "17"
    }
    buildFeatures {
        compose = true
        buildConfig = true
    }
}

dependencies {
    implementation(libs.androidx.core.ktx)
    implementation(libs.androidx.lifecycle.runtime.ktx)
    implementation(libs.androidx.activity.compose)
    implementation(platform(libs.androidx.compose.bom))
    implementation(libs.androidx.ui)
    implementation(libs.androidx.ui.graphics)
    implementation(libs.androidx.ui.tooling.preview)
    implementation(libs.androidx.material3)
    implementation(libs.appcompat)
    implementation(libs.androidx.splashscreen)
    
    // Kiosk-only: NanoHTTPD for local HTTPS server
    "kioskImplementation"(libs.nanohttpd)
    
    testImplementation(libs.junit)
    androidTestImplementation(libs.androidx.junit)
    androidTestImplementation(libs.androidx.espresso.core)
    androidTestImplementation(platform(libs.androidx.compose.bom))
    androidTestImplementation(libs.androidx.ui.test.junit4)
    debugImplementation(libs.androidx.ui.tooling)
    debugImplementation(libs.androidx.ui.test.manifest)
}