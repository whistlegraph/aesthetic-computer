package computer.aesthetic.raxvr;

import android.app.NativeActivity;
import android.os.Bundle;
import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.net.HttpURLConnection;
import java.net.URL;

// The Java side of rax's native headset app. The drawing is main.cpp's; this
// keeps two files fresh from the rax server, each a list of boxes, written
// where the native side looks (by a rename, so never read half written):
//   files/scene.txt  the room, asked for every few seconds;
//   files/live.txt   who is in it this instant (the people, the flying
//                    heads), asked for about ten times a second, one thread,
//                    one kept connection, short waits.
// So the room is changed on the server, as the website is, and shows in the
// headset without a new build; and the headset looks on at the world live.
//   adb shell am start -n computer.aesthetic.raxvr/.RaxNativeActivity -e scene <address> -e live <address>
// (With only `scene` given, live is asked for beside it: …/native-live.txt.)
public class RaxNativeActivity extends NativeActivity {
  private static final String SCENE = "https://aesthetic.computer/rax/native-scene.txt";
  private volatile boolean alive = true;

  // The text at an address, or null when it could not be had.
  private static byte[] fetch(String address, int connectMs, int readMs) throws Exception {
    HttpURLConnection c = (HttpURLConnection) new URL(address).openConnection();
    c.setConnectTimeout(connectMs); c.setReadTimeout(readMs); c.setUseCaches(false);
    try {
      if (c.getResponseCode() != 200) { InputStream e = c.getErrorStream(); if (e != null) e.close(); return null; }
      InputStream in = c.getInputStream();
      java.io.ByteArrayOutputStream all = new java.io.ByteArrayOutputStream();
      byte[] buf = new byte[8192]; int n;
      while ((n = in.read(buf)) > 0 && all.size() < 4 * 1024 * 1024) all.write(buf, 0, n);
      in.close(); // (read to its end and closed, not disconnected: the connection is kept for the next asking)
      return all.toByteArray();
    } catch (Exception e) { c.disconnect(); throw e; }
  }
  private static boolean put(File part, File out, byte[] bytes) throws Exception {
    FileOutputStream f = new FileOutputStream(part); f.write(bytes); f.close();
    return part.renameTo(out);
  }

  @Override protected void onCreate(Bundle saved) {
    super.onCreate(saved);
    final String given = getIntent().getStringExtra("scene"), givenLive = getIntent().getStringExtra("live");
    final String address = given != null && given.startsWith("http") ? given : SCENE;
    final String liveAddress = givenLive != null && givenLive.startsWith("http") ? givenLive : address.replace("native-scene.txt", "native-live.txt");
    final File out = new File(getFilesDir(), "scene.txt"), part = new File(getFilesDir(), "scene.part");
    final File live = new File(getFilesDir(), "live.txt"), livePart = new File(getFilesDir(), "live.part");
    live.delete(); // (who was there last time is not who is there now)
    android.util.Log.i("rax", "java: scene " + address + ", live " + liveAddress);
    new Thread(() -> {
      String last = null;
      while (alive) {
        try {
          byte[] bytes = fetch(address, 5000, 8000);
          if (bytes != null) {
            // (Its first line is a note with the time in it: left out of the comparing, so the same room is not made again every four seconds.)
            String all = new String(bytes, "UTF-8"), text = all.substring(Math.max(0, all.indexOf('\n') + 1));
            if (!text.equals(last) && text.length() > 10 && put(part, out, bytes)) last = text;
          }
        } catch (Exception e) { android.util.Log.w("rax", "scene: " + e); }
        try { Thread.sleep(4000); } catch (InterruptedException e) { return; }
      }
    }, "rax-scene").start();
    new Thread(() -> {
      String last = null;
      long got = 0, failed = 0, saidAt = 0, lastGood = System.currentTimeMillis();
      while (alive) {
        final long began = System.currentTimeMillis();
        try {
          byte[] bytes = fetch(liveAddress, 1500, 1500);
          if (bytes != null) {
            got++; lastGood = began;
            String text = new String(bytes, "UTF-8");
            // (The first line is a note with the server's tick in it: left out of the comparing, so a room where nothing moves is not written again and again.)
            String body = text.substring(Math.max(0, text.indexOf('\n') + 1));
            if (!body.equals(last) && put(livePart, live, bytes)) last = body;
          } else failed++;
        } catch (Exception e) { failed++; if (failed % 50 == 1) android.util.Log.w("rax", "live: " + e); }
        // Nothing heard for three seconds: nobody is drawn, sooner than people standing still who have gone.
        if (began - lastGood > 3000 && last != null) { try { if (put(livePart, live, new byte[0])) last = null; } catch (Exception e) { } }
        if (began - saidAt > 5000) { saidAt = began; android.util.Log.i("rax", "live: " + got + " got, " + failed + " failed, " + (last == null ? 0 : last.split("\n").length) + " lines now"); }
        // About ten a second when the server answers at once; every two seconds after a failure (a server that has no such address yet is not asked ten times a second).
        final long wait = (lastGood >= began ? 100 : 2000) - (System.currentTimeMillis() - began);
        try { if (wait > 0) Thread.sleep(wait); } catch (InterruptedException e) { return; }
      }
    }, "rax-live").start();
  }

  @Override protected void onDestroy() { alive = false; super.onDestroy(); }
}
