(function initACMediaRecords(global) {
  if (global.ACMediaRecords) return;

  function normalizeLink(link) {
    if (!link || typeof link !== "string") return "";
    const trimmed = link.trim();
    if (!trimmed) return "";
    if (/^https?:\/\//i.test(trimmed)) return trimmed;
    if (/^at:\/\//i.test(trimmed)) return `https://pdsls.dev/${trimmed}`;
    if (/^[\w.-]+\.[a-z]{2,}([/?#].*)?$/i.test(trimmed)) return `https://${trimmed}`;
    return "";
  }

  function recordFallbackLink(item) {
    if (!item?.uri || typeof item.uri !== "string") return "";
    const atUri = item.uri.startsWith("at://")
      ? item.uri
      : `at://${item.uri.split("//")[1] || item.uri}`;
    return `https://pdsls.dev/${atUri}`;
  }

  function escapeHtml(value) {
    return String(value ?? "")
      .replace(/&/g, "&amp;")
      .replace(/</g, "&lt;")
      .replace(/>/g, "&gt;")
      .replace(/"/g, "&quot;")
      .replace(/'/g, "&#39;");
  }

  function buildFeedModalPayload(item, col, repoLabel, ago, title, primaryLink) {
    const recordLink = recordFallbackLink(item);
    const safePrimaryLink = normalizeLink(primaryLink);
    const safeCode = item?.code ? encodeURIComponent(String(item.code)) : "";
    let iframeUrl = "";
    let previewHtml = "";
    let extraHtml = "";

    switch (col.label) {
      case "painting":
        if (safeCode) {
          previewHtml = `<img src="https://aesthetic.computer/media/paintings/${safeCode}" alt="" style="width:100%;max-height:360px;object-fit:contain;border:1px solid rgba(205,92,155,0.2);border-radius:6px;background:#fff;">`;
        }
        break;
      case "kidlisp":
        if (safeCode) {
          previewHtml = `<img src="https://oven.aesthetic.computer/grab/webp/640/360/$${safeCode}?duration=2500&fps=8&quality=80&density=1&nowait=true" alt="" style="width:100%;max-height:360px;object-fit:cover;border:1px solid rgba(205,92,155,0.2);border-radius:6px;">`;
        }
        if (item?.source) {
          extraHtml = `<pre style="margin:0;padding:0.75em;background:rgba(205,92,155,0.08);border:1px solid rgba(205,92,155,0.2);border-radius:6px;white-space:pre-wrap;word-break:break-word;max-height:180px;overflow:auto;">${escapeHtml(String(item.source).slice(0, 1200))}</pre>`;
        }
        break;
      case "mood":
        if (item?.mood) {
          extraHtml = `<blockquote style="margin:0;padding:0.75em;border-left:3px solid rgba(205,92,155,0.6);background:rgba(205,92,155,0.08);border-radius:4px;">${escapeHtml(item.mood)}</blockquote>`;
        }
        break;
      case "news": {
        const newsUrl = safePrimaryLink || "https://news.aesthetic.computer";
        iframeUrl = newsUrl;
        if (item?.body) {
          extraHtml = `<div style="padding:0.75em;background:rgba(205,92,155,0.08);border:1px solid rgba(205,92,155,0.2);border-radius:6px;max-height:220px;overflow:auto;">${escapeHtml(String(item.body).slice(0, 1800))}</div>`;
        }
        break;
      }
      case "paper":
        iframeUrl = safePrimaryLink;
        break;
      default:
        iframeUrl = "";
    }

    const metadata = [
      ["Type", col.label],
      ["When", ago || "recent"],
      ["From", repoLabel || "unknown"],
      ["Rkey", item?.uri ? String(item.uri).split("/").pop() : ""],
    ]
      .filter((entry) => entry[1])
      .map((entry) => `<div><strong>${escapeHtml(entry[0])}:</strong> ${escapeHtml(entry[1])}</div>`)
      .join("");

    const rawRecord = escapeHtml(JSON.stringify(item, null, 2));
    const bodyHtml = `
        ${previewHtml}
        ${extraHtml}
        <div style="display:grid;gap:0.35em;font-size:0.82em;line-height:1.45;padding:0.65em 0.75em;border:1px solid rgba(205,92,155,0.2);border-radius:6px;background:rgba(205,92,155,0.06);">${metadata}</div>
        <details>
          <summary style="cursor:pointer;opacity:0.8;">Raw record</summary>
          <pre style="margin-top:0.5em;padding:0.65em;background:rgba(0,0,0,0.05);border-radius:6px;overflow:auto;font-size:0.75em;max-height:220px;">${rawRecord}</pre>
        </details>
      `;

    const actions = [];
    if (safePrimaryLink) actions.push({ label: "Open Link", url: safePrimaryLink });
    if (col.label === "news") actions.push({ label: "Open News Site", url: "https://news.aesthetic.computer" });
    if (recordLink) actions.push({ label: "View Record", url: recordLink });

    const seen = new Set();
    const dedupedActions = actions.filter((action) => {
      if (seen.has(action.url)) return false;
      seen.add(action.url);
      return true;
    });

    return {
      title: `${col.icon} ${title || col.label}`,
      subtitle: `${col.label} · ${ago || "recent"} · ${repoLabel || "unknown"}`,
      bodyHtml,
      iframeUrl,
      actions: dedupedActions,
    };
  }

  global.ACMediaRecords = {
    buildFeedModalPayload,
    escapeHtml,
    normalizeLink,
    recordFallbackLink,
  };
})(window);
