package computer.aesthetic.rax;

import android.app.Activity;
import android.graphics.Color;
import android.os.Bundle;
import android.view.View;
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
    web.setWebViewClient(new WebViewClient());
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
