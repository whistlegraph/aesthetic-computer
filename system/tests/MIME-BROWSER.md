# MIME feed browser regression

Run against a dedicated Chrome on another machine through an SSH-forwarded
DevTools port, or use local Chrome:

```sh
MIME_CDP_URL=http://127.0.0.1:19339 MIME_SCREENSHOT_DIR=/tmp/mime-qa \
  node --test system/tests/mime-scroll-browser.test.mjs

PLAYWRIGHT_CHANNEL=chrome node --test system/tests/mime-scroll-browser.test.mjs
```

The remote run creates and closes its own browser context. It does not close
existing tabs. All requests use fixtures: no live comments, analytics, or
account changes. Use an isolated Chrome profile and a loopback/SSH-only port.

Checks cover center wheel scrolling over a scroll-capturing program, a document
and long text; explicit interaction and returning to scrolling; resetting an
interaction after scrolling away; touch scrolling at mobile width; natural
image/video proportions; deferred offscreen text; a single dominant playing
video; on-demand tape player creation and destruction; loading-state completion; and the three reviewed MIME action signals.
The tiny WebM fixture was generated with ffmpeg from a one-second solid color
frame (160×90 at 10 fps); it contains no external media. Inspect the saved mobile and
desktop screenshots as well as the assertions. Live media loading still needs
a separate marked browser check because fixtures do not measure network speed.
