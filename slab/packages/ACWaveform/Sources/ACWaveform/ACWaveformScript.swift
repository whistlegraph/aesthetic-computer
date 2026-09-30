import WebKit

/// The page half. It reads `window.AC.readOutputWaveform` — the runtime's lazy,
/// silent tap on the piece's speakers — and posts one min/max slice per read
/// to the `acWaveform` handler: `[low, high]` while sound plays, `0` for a
/// beat of silence after it stops so the strip keeps its tail.
///
/// Ported from the Electron desktop's preview-waveform.js (deleted in
/// f6574e12cc). Slices keep transients that a whole audio cycle aliased down to
/// one pixel would lose. Display gain follows peaks at once, releases gently,
/// and is capped so near-silence never fills the strip.
public enum ACWaveformScript {
    public static let handlerName = "acWaveform"

    public static let source = """
    (() => {
      if (window.__acWaveform) return;
      window.__acWaveform = true;
      let envelope = 0, previous = performance.now(), lastActive = -Infinity, tail = false;
      const post = value => { try { window.webkit.messageHandlers.\(handlerName).postMessage(value); } catch {} };
      const read = () => {
        let values = [];
        try { values = window.AC?.readOutputWaveform?.(512) || []; } catch { /* Audio can detach mid-reload. */ }
        let mean = 0;
        for (const v of values) mean += Number.isFinite(v) ? v : 0;
        mean /= values.length || 1;
        let low = 0, high = 0;
        for (const v of values) {
          const c = (Number.isFinite(v) ? Math.max(-1, Math.min(1, v)) : 0) - mean;
          if (c < low) low = c;
          if (c > high) high = c;
        }
        const now = performance.now(), peak = Math.max(-low, high), active = peak > 0.0005;
        if (active) lastActive = now;
        envelope = Math.max(peak, envelope * Math.exp(-(now - previous) / 180));
        previous = now;
        const listening = now - lastActive < 1500;
        if (active) {
          const gain = Math.min(32, 0.85 / (envelope || 1));
          post([low * gain, high * gain]);
          tail = true;
        } else if (tail) {
          post(0);
          tail = listening;
        }
        // Frame-ish reads while sound plays and for a beat after; silence backs
        // off to eight a second, which is the whole idle cost.
        setTimeout(read, listening ? 33 : 125);
      };
      read();
    })();
    """

    /// Injected at document end so the runtime's `window.AC` has been set up by
    /// the time the first read lands; the reads tolerate its absence anyway.
    public static func userScript() -> WKUserScript {
        WKUserScript(source: source, injectionTime: .atDocumentEnd, forMainFrameOnly: true)
    }
}
