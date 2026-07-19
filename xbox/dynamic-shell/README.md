# AC Xbox dynamic shell

Minimal x64 UWP WebView host for Xbox Developer Mode. It loads
`https://aesthetic.computer/fight` by default while keeping the existing
`../native-latency` probe as an independent package.

- Press **Menu** to edit the AC endpoint. The HTTP(S) URL persists in UWP local settings.
- Press **A** (or Enter) to save and load; **B** cancels the editor.
- A remote/test tool can launch `acfight:?endpoint=https%3A%2F%2Fdev.example%2Ffight`
  to switch and persist the endpoint without repackaging.
- Outside the editor, **B/back is captured** and dispatched to the page as an
  `ac-shell-back` event. It cannot close the app.
- Navigation and injected JavaScript console/error messages are written to the
  Visual Studio debug stream with `[AC NAV]` and `[AC WEB]` prefixes.

This is downloaded web content (HTML/JS/pieces), not downloaded native code.
The UWP package stays fixed while changes deployed to an AC endpoint load on refresh.

Build `DynamicShell.sln` as Release/x64, or use the `xbox-dynamic-shell-x64`
AppVeyor artifact. The generated CI certificate password is `aesthetic-xbox-ci`.
