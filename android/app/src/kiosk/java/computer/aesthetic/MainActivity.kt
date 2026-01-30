package computer.aesthetic

import android.app.admin.DevicePolicyManager
import android.content.ComponentName
import android.content.Context
import android.net.http.SslError
import android.os.Bundle
import android.util.Log
import android.view.KeyEvent
import android.view.MotionEvent
import android.view.View
import android.webkit.SslErrorHandler
import android.webkit.WebView
import android.webkit.WebViewClient
import android.widget.FrameLayout
import androidx.activity.ComponentActivity
import androidx.activity.OnBackPressedCallback
import androidx.activity.compose.setContent
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.viewinterop.AndroidView
import androidx.core.view.WindowCompat
import androidx.core.view.WindowInsetsCompat
import androidx.core.view.WindowInsetsControllerCompat
import computer.aesthetic.ui.theme.AestheticComputerTheme
import android.view.WindowManager

/**
 * Kiosk version of Aesthetic Computer - for dedicated device installations
 * Enables device owner mode, lock task, and local HTTPS server
 */
class MainActivity : ComponentActivity() {
    private lateinit var localHttpServer: LocalHttpServer
    private lateinit var devicePolicyManager: DevicePolicyManager
    private lateinit var componentName: ComponentName

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        window.decorView.setBackgroundColor(android.graphics.Color.BLACK)

        // Start the local HTTP server
        localHttpServer = LocalHttpServer(this)
        localHttpServer.start()

        // Initialize Device Policy Manager
        devicePolicyManager = getSystemService(Context.DEVICE_POLICY_SERVICE) as DevicePolicyManager
        componentName = ComponentName(this, MyDeviceAdminReceiver::class.java)

        // Check if app is device owner
        if (devicePolicyManager.isDeviceOwnerApp(packageName)) {
            // Request full screen before the content view is set
            window.decorView.systemUiVisibility = View.SYSTEM_UI_FLAG_FULLSCREEN
            window.setFlags(
                WindowManager.LayoutParams.FLAG_FULLSCREEN,
                WindowManager.LayoutParams.FLAG_FULLSCREEN
            )

            // Block back gestures and edge swipe
            onBackPressedDispatcher.addCallback(this, object : OnBackPressedCallback(true) {
                override fun handleOnBackPressed() {
                    // Do nothing - blocks back gesture
                }
            })

            hideSystemUI() // Hide system bars and disable swipe gestures
            addGestureBlockingOverlay() // Add a gesture-blocking overlay

            // Disable the lock screen.
            val dpm = getSystemService(Context.DEVICE_POLICY_SERVICE) as DevicePolicyManager
            val adminComponent = ComponentName(this, MyDeviceAdminReceiver::class.java)
            dpm.setKeyguardDisabled(adminComponent, true)

            devicePolicyManager.setLockTaskPackages(componentName, arrayOf(packageName))
            startLockTask() // Enter Lock Task Mode
            Log.i("Aesthetic", "Task locked.")
        } else {
            try {
                throw IllegalStateException("App is not device owner - kiosk mode requires device owner!")
            } catch (e: IllegalStateException) {
                Log.e("Aesthetic", "Exception caught: ${e.message}")
            }
        }

        // Load the WebView into the screen
        setContent {
            AestheticComputerTheme {
                KioskWebView(url = BuildConfig.BASE_URL)
            }
        }
    }

    private fun hideSystemUI() {
        WindowCompat.setDecorFitsSystemWindows(window, false)
        val controller = WindowInsetsControllerCompat(window, window.decorView)
        controller.apply {
            hide(WindowInsetsCompat.Type.systemBars())
            systemBarsBehavior = WindowInsetsControllerCompat.BEHAVIOR_SHOW_TRANSIENT_BARS_BY_SWIPE
        }
    }

    private fun addGestureBlockingOverlay() {
        val overlay = FrameLayout(this)
        overlay.setOnTouchListener { _, _ -> true }
        overlay.setBackgroundColor(0x00000000) // Fully transparent
        addContentView(
            overlay, FrameLayout.LayoutParams(
                FrameLayout.LayoutParams.MATCH_PARENT,
                FrameLayout.LayoutParams.MATCH_PARENT
            )
        )
    }

    override fun onDestroy() {
        super.onDestroy()
        localHttpServer.stop()
    }

    // Block the Back and Home keys
    override fun onKeyDown(keyCode: Int, event: KeyEvent?): Boolean {
        return when (keyCode) {
            KeyEvent.KEYCODE_BACK, KeyEvent.KEYCODE_HOME -> true
            else -> super.onKeyDown(keyCode, event)
        }
    }

    override fun onTouchEvent(event: MotionEvent?): Boolean {
        return true
    }
}

@Composable
fun KioskWebView(url: String) {
    AndroidView(
        factory = { context ->
            WebView(context).apply {
                settings.javaScriptEnabled = true
                setBackgroundColor(android.graphics.Color.TRANSPARENT)
                setLayerType(View.LAYER_TYPE_SOFTWARE, null)
                webViewClient = KioskWebViewClient()
                loadUrl(url)
            }
        },
        modifier = Modifier.fillMaxSize()
    )
}

class KioskWebViewClient : WebViewClient() {
    override fun onReceivedSslError(view: WebView?, handler: SslErrorHandler?, error: SslError?) {
        handler?.proceed() // Accept self-signed certs for local server
    }

    override fun shouldOverrideUrlLoading(view: WebView?, request: android.webkit.WebResourceRequest?): Boolean {
        view?.loadUrl(request?.url.toString())
        return true
    }
}
