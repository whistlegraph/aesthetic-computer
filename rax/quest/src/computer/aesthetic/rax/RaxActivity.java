package computer.aesthetic.rax;

import android.app.Activity;
import android.content.Intent;
import android.graphics.Color;
import android.net.Uri;
import android.os.Bundle;
import android.view.View;
import android.webkit.JavascriptInterface;
import android.webkit.PermissionRequest;
import android.webkit.WebChromeClient;
import android.webkit.WebSettings;
import android.webkit.WebView;
import android.webkit.WebViewClient;

// rax, in a panel of its own. The page is the same one the browser shows
// (https://rax.ac/): a web view with scripts, storage and sound allowed, a
// game pad's keys left to the page, and the microphone given when asked.
// `adb shell am start -n computer.aesthetic.rax/.RaxActivity -e url <address>`
// opens another address (a preview, a room by its word).
public class RaxActivity extends Activity {
  private static final String IMMERSIVE = "https://aesthetic.computer/rax/"; // (where this headset is signed in, and where AC's pages can be read on the room's screens)
  private WebView web;

  @Override protected void onCreate(Bundle saved) {
    super.onCreate(saved);
    WebView.setWebContentsDebuggingEnabled(true); // (read from a desk: chrome://inspect, as the browser is)
    web = new WebView(this);
    web.setBackgroundColor(Color.BLACK);
    WebSettings s = web.getSettings();
    s.setJavaScriptEnabled(true);
    s.setDomStorageEnabled(true);
    s.setMediaPlaybackRequiresUserGesture(false);
    s.setUserAgentString(s.getUserAgentString() + " RaxQuest/0.1");
    // 🥽 The web view here has no WebXR, so the panel cannot go in to VR or mixed reality itself. Until this app has a
    // headset renderer of its own, a button on the page hands the same room to the Quest's browser, which can: one
    // press from the panel to where IMMERSE is offered. (`RaxQuest.immerse()` from the page does the same.)
    web.addJavascriptInterface(new Object() {
      @JavascriptInterface public void immerse() {
        runOnUiThread(() -> {
          try { startActivity(new Intent(Intent.ACTION_VIEW, Uri.parse(IMMERSIVE)).setPackage("com.oculus.browser")); }
          catch (Exception e) { startActivity(new Intent(Intent.ACTION_VIEW, Uri.parse(IMMERSIVE))); }
        });
      }
    }, "RaxQuest");
    web.setWebViewClient(new WebViewClient() {
      @Override public void onPageFinished(WebView view, String url) {
        view.evaluateJavascript("(function(){if(navigator.xr||document.getElementById('rax-quest-immerse'))return;"
          + "var b=document.createElement('button');b.id='rax-quest-immerse';b.textContent='immerse \u2192 browser';"
          + "b.style.cssText='position:fixed;right:16px;bottom:16px;z-index:99999;font:16px monospace;padding:12px 16px;"
          + "background:#00ffff;color:#10131c;border:2px solid #10131c;border-radius:0;cursor:pointer';"
          + "b.onclick=function(){RaxQuest.immerse()};document.body.appendChild(b)})()", null);
      }
    });
    web.setWebChromeClient(new WebChromeClient() {
      @Override public void onPermissionRequest(PermissionRequest request) { request.grant(request.getResources()); }
    });
    web.setSystemUiVisibility(View.SYSTEM_UI_FLAG_FULLSCREEN | View.SYSTEM_UI_FLAG_HIDE_NAVIGATION | View.SYSTEM_UI_FLAG_IMMERSIVE_STICKY);
    setContentView(web);
    String url = getIntent().getStringExtra("url");
    web.loadUrl(url != null && url.startsWith("https://") ? url : "https://rax.ac/");
  }

  @Override public void onBackPressed() { if (web.canGoBack()) web.goBack(); else super.onBackPressed(); }
  @Override protected void onPause() { super.onPause(); web.onPause(); }
  @Override protected void onResume() { super.onResume(); web.onResume(); }
  @Override protected void onDestroy() { web.destroy(); super.onDestroy(); }
}
