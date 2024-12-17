package computer.aesthetic

import android.os.Bundle
import android.view.KeyEvent
import android.view.View
import android.view.WindowManager
import android.webkit.WebView
import android.webkit.WebViewClient
import androidx.activity.ComponentActivity
import androidx.activity.compose.setContent
import androidx.activity.enableEdgeToEdge
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.material3.MaterialTheme
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.viewinterop.AndroidView
import computer.aesthetic.ui.theme.AestheticComputerTheme
import android.net.http.SslError
import android.webkit.SslErrorHandler
import android.webkit.WebResourceRequest
import android.app.admin.DevicePolicyManager
import android.content.ComponentName
import android.content.Context


class MainActivity : ComponentActivity() {
    private lateinit var localHttpServer: LocalHttpServer

    private lateinit var devicePolicyManager: DevicePolicyManager
    private lateinit var componentName: ComponentName


    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)

        devicePolicyManager = getSystemService(Context.DEVICE_POLICY_SERVICE) as DevicePolicyManager
        componentName = ComponentName(this, MyDeviceAdminReceiver::class.java)

        // Start the local HTTP server
        localHttpServer = LocalHttpServer(this)
        localHttpServer.start()

        // Initialize Device Policy Manager
        devicePolicyManager = getSystemService(Context.DEVICE_POLICY_SERVICE) as DevicePolicyManager
        componentName = ComponentName(this, MyDeviceAdminReceiver::class.java)

        // Check if app is device owner
        if (devicePolicyManager.isDeviceOwnerApp(packageName)) {
            startLockTask()
        } else {
            throw IllegalStateException("App is not the device owner!")
        }

        // Enable fullscreen immersive mode
        window.setFlags(
            WindowManager.LayoutParams.FLAG_FULLSCREEN,
            WindowManager.LayoutParams.FLAG_FULLSCREEN
        )
        window.decorView.systemUiVisibility = (
                View.SYSTEM_UI_FLAG_FULLSCREEN
                        or View.SYSTEM_UI_FLAG_HIDE_NAVIGATION
                        or View.SYSTEM_UI_FLAG_IMMERSIVE_STICKY
                )

        //enableEdgeToEdge()

        setContent {
            AestheticComputerTheme {
                // Load the WebView pointing to the local server
                WebViewPage(url = "https://localhost:8443")
            }
        }
    }

    override fun onDestroy() {
        super.onDestroy()
        // Stop the HTTP server when the activity is destroyed
        localHttpServer.stop()
    }

    // Override Back button behavior to disable it
    override fun onKeyDown(keyCode: Int, event: KeyEvent?): Boolean {
        return when (keyCode) {
            KeyEvent.KEYCODE_BACK, KeyEvent.KEYCODE_HOME -> true // Disable Back and Home buttons
            else -> super.onKeyDown(keyCode, event)
        }
    }

}

@Composable
fun WebViewPage(url: String) {
    // Integrate Android's WebView into Compose
    AndroidView(
        factory = { context ->
            WebView(context).apply {
                settings.javaScriptEnabled = true
                webViewClient = CustomWebViewClient() // Keeps navigation within the WebView
                loadUrl(url)
            }
        },
        modifier = Modifier.fillMaxSize()
    )
}

class CustomWebViewClient : WebViewClient() {

    // Ignore SSL errors for self-signed certificates
    override fun onReceivedSslError(
        view: WebView?,
        handler: SslErrorHandler?,
        error: SslError?
    ) {
        handler?.proceed() // Proceed despite the SSL error (development only)
    }

    // Handle navigation within the WebView
    override fun shouldOverrideUrlLoading(view: WebView?, request: WebResourceRequest?): Boolean {
        view?.loadUrl(request?.url.toString())
        return true
    }
}
