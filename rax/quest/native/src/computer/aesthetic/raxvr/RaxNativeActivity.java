package computer.aesthetic.raxvr;

import android.app.NativeActivity;
import android.os.Bundle;
import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.net.HttpURLConnection;
import java.net.URL;

// The Java side of rax's native headset app. The drawing is main.cpp's; this
// keeps the room's scene file fresh: every few seconds it asks the rax server
// for the room as a list of boxes and, when the answer has changed, writes it
// where the native side looks (files/scene.txt, by a rename, so it is never
// read half written). So the room is changed on the server, as the website
// is, and shows in the headset without a new build.
//   adb shell am start -n computer.aesthetic.raxvr/.RaxNativeActivity -e scene <address>
public class RaxNativeActivity extends NativeActivity {
  private static final String SCENE = "https://aesthetic.computer/rax/native-scene.txt";
  private volatile boolean alive = true;

  @Override protected void onCreate(Bundle saved) {
    super.onCreate(saved);
    final String given = getIntent().getStringExtra("scene");
    final String address = given != null && given.startsWith("http") ? given : SCENE;
    final File out = new File(getFilesDir(), "scene.txt"), part = new File(getFilesDir(), "scene.part");
    new Thread(() -> {
      String last = null;
      while (alive) {
        try {
          HttpURLConnection c = (HttpURLConnection) new URL(address).openConnection();
          c.setConnectTimeout(5000); c.setReadTimeout(8000);
          if (c.getResponseCode() == 200) {
            InputStream in = c.getInputStream();
            java.io.ByteArrayOutputStream all = new java.io.ByteArrayOutputStream();
            byte[] buf = new byte[8192]; int n;
            while ((n = in.read(buf)) > 0 && all.size() < 4 * 1024 * 1024) all.write(buf, 0, n);
            in.close();
            String text = all.toString("UTF-8");
            if (!text.equals(last) && text.length() > 10) {
              FileOutputStream f = new FileOutputStream(part); f.write(all.toByteArray()); f.close();
              if (part.renameTo(out)) last = text;
            }
          }
          c.disconnect();
        } catch (Exception e) { android.util.Log.w("rax", "scene: " + e); }
        try { Thread.sleep(4000); } catch (InterruptedException e) { return; }
      }
    }, "rax-scene").start();
  }

  @Override protected void onDestroy() { alive = false; super.onDestroy(); }
}
