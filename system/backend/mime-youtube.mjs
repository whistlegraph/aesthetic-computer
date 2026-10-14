// Recognize public video links without fetching arbitrary user-supplied URLs.
export function youtubeEmbed(text) {
  for (const match of String(text || "").matchAll(/https?:\/\/[^\s<>"']+/gi)) {
    let url;
    try { url = new URL(match[0].replace(/[),.!?;]+$/, "")); } catch { continue; }
    if (url.username || url.password || url.port) continue;
    const host = url.hostname.toLowerCase();
    const parts = url.pathname.split("/").filter(Boolean);
    let id;
    if (host === "youtu.be" && parts.length === 1) id = parts[0];
    else if (["youtube.com", "www.youtube.com", "m.youtube.com", "music.youtube.com", "www.youtube-nocookie.com"].includes(host)) {
      if (url.pathname === "/watch") id = url.searchParams.get("v");
      else if (["shorts", "live", "embed"].includes(parts[0]) && parts.length === 2) id = parts[1];
    }
    if (!/^[\w-]{11}$/.test(id || "")) continue;
    const time = url.searchParams.get("t") || url.searchParams.get("start") || "0";
    const units = /^(?:(\d+)h)?(?:(\d+)m)?(?:(\d+)s)?$/.exec(time);
    const seconds = /^\d+$/.test(time) ? Number(time) : units ? Number(units[1] || 0) * 3600 + Number(units[2] || 0) * 60 + Number(units[3] || 0) : 0;
    const start = Math.min(86400, seconds);
    const caption = (String(text).slice(0, match.index) + String(text).slice(match.index + match[0].length)).trim();
    return { kind: "youtube", id, start, caption, url: `https://www.youtube.com/watch?v=${id}` + (start ? `&t=${start}s` : "") };
  }
  return null;
}
