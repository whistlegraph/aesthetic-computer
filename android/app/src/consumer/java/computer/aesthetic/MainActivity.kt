package computer.aesthetic

import android.os.Bundle
import android.view.View
import android.webkit.WebChromeClient
import android.webkit.WebSettings
import android.webkit.WebView
import android.webkit.WebViewClient
import android.webkit.PermissionRequest
import androidx.activity.ComponentActivity
import androidx.activity.compose.setContent
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.viewinterop.AndroidView
import androidx.core.splashscreen.SplashScreen.Companion.installSplashScreen
import computer.aesthetic.ui.theme.AestheticComputerTheme

/**
 * Consumer version of Aesthetic Computer - simple WebView wrapper for Play Store
 */
class MainActivity : ComponentActivity() {
    
    override fun onCreate(savedInstanceState: Bundle?) {
        // Show splash screen
        installSplashScreen()
        
        super.onCreate(savedInstanceState)
        
        // Purple background while loading
        window.decorView.setBackgroundColor(0xFF6600FF.toInt())
        
        setContent {
            AestheticComputerTheme {
                AestheticWebView(url = BuildConfig.BASE_URL)
            }
        }
    }
}

@Composable
fun AestheticWebView(url: String) {
    AndroidView(
        factory = { context ->
            WebView(context).apply {
                // Background
                setBackgroundColor(0xFF6600FF.toInt()) // Purple while loading
                
                // WebView settings
                settings.apply {
                    javaScriptEnabled = true
                    domStorageEnabled = true
                    mediaPlaybackRequiresUserGesture = false
                    allowFileAccess = true
                    databaseEnabled = true
                    
                    // Allow mixed content (some assets may be http)
                    mixedContentMode = WebSettings.MIXED_CONTENT_COMPATIBILITY_MODE
                    
                    // Enable hardware acceleration
                    setLayerType(View.LAYER_TYPE_HARDWARE, null)
                }
                
                // Handle navigation within WebView
                webViewClient = object : WebViewClient() {
                    override fun shouldOverrideUrlLoading(
                        view: WebView?,
                        request: android.webkit.WebResourceRequest?
                    ): Boolean {
                        // Keep aesthetic.computer URLs in the WebView
                        val requestUrl = request?.url?.toString() ?: return false
                        return if (requestUrl.contains("aesthetic.computer")) {
                            view?.loadUrl(requestUrl)
                            true
                        } else {
                            // Open external URLs in browser
                            false
                        }
                    }
                    
                    override fun onPageFinished(view: WebView?, url: String?) {
                        super.onPageFinished(view, url)
                        // Make background transparent after page loads
                        view?.setBackgroundColor(android.graphics.Color.TRANSPARENT)
                    }
                }
                
                // Handle permissions (camera, microphone for pieces that need them)
                webChromeClient = object : WebChromeClient() {
                    override fun onPermissionRequest(request: PermissionRequest?) {
                        request?.grant(request.resources)
                    }
                }
                
                loadUrl(url)
            }
        },
        modifier = Modifier.fillMaxSize()
    )
}
