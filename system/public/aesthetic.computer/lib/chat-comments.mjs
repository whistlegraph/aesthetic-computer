// MIME thread counts and a read-only comment sheet shared by both chat clients.
export function createCommentCounts(instance, changed, request = fetch) {
  const counts = new Map();
  let last = 0, signature = "", pending = false, disposed = false;
  const controller = new AbortController();
  return {
    counts,
    dispose() { disposed = true; controller.abort(); },
    async refresh(messages) {
      if (pending || disposed || Date.now() - last < 1000) return;
      const ids = [...new Set(messages.filter(m => !m.deleted && /^[a-f0-9]{24}$/.test(m.id)).map(m => m.id))];
      const key = ids.join(","), now = Date.now();
      if (now - last < 1000 || (key === signature && now - last < 30000)) return;
      last = now; signature = key; pending = true;
      try {
        for (let i = 0; i < ids.length; i += 100) {
          const batch = ids.slice(i, i + 100);
          const response = await request(`/api/mime?instance=${instance}&chatCounts=${batch.join(",")}`, { signal: controller.signal });
          if (!response.ok) throw new Error("Comments unavailable");
          const data = await response.json();
          if (disposed) return;
          let moved = false;
          for (const id of batch) {
            const count = Math.max(0, Math.floor(Number(data.counts?.[id]) || 0));
            if (counts.get(id) !== count) { counts.set(id, count); moved = true; }
          }
          if (moved) changed?.(counts);
        }
      } catch { /* Keep the previous counts and retry on the next cadence. */ }
      finally { pending = false; }
    },
  };
}

export function openChatComments(instance, id, onClose = () => {}) {
  if (!["clock", "system"].includes(instance) || !/^[a-f0-9]{24}$/.test(id)) return;
  const previous = document.activeElement;
  const overflow = document.documentElement.style.overflow;
  document.documentElement.style.overflow = "hidden";
  const dialog = document.createElement("dialog");
  dialog.className = "ac-chat-comments";
  dialog.setAttribute("aria-label", "MIME comments");
  dialog.innerHTML = `<style>
    .ac-chat-comments { box-sizing:border-box; inset:auto 0 0; margin:0 auto; width:min(560px,100%); max-width:100%; height:min(600px,75dvh); max-height:85dvh; padding:20px; border:1px solid #88788e; border-radius:18px 18px 0 0; background:#272237; color:#f5eefa; font:16px/1.5 system-ui,sans-serif; overflow:auto; overscroll-behavior:contain; }
    .ac-chat-comments[open] { animation:ac-comments-up 180ms ease-out; }
    .ac-chat-comments::backdrop { background:#14102488; }
    .ac-chat-comments h2 { font-size:20px; margin:0 0 16px; }
    .ac-chat-comments article { border-top:1px solid #625271; padding:14px 0; }
    .ac-chat-comments p { white-space:pre-wrap; overflow-wrap:anywhere; margin:6px 0; }
    .ac-chat-comments a { color:#7de3d7; }
    .ac-chat-comments button { font:inherit; padding:8px 16px; }
    @media(prefers-color-scheme:light) {
      .ac-chat-comments { color-scheme:light; background:#fff; color:#241b30; border-color:#806e8a; }
      .ac-chat-comments article { border-color:#806e8a; }
      .ac-chat-comments a { color:#006b72; }
    }
    @keyframes ac-comments-up { from { transform:translateY(100%); } to { transform:translateY(0); } }
    @media(prefers-reduced-motion:reduce) { .ac-chat-comments[open] { animation:none; } }
  </style><h2 tabindex="-1">MIME comments</h2><div class="comments-list"></div><p role="status">Loading…</p><button type="button" hidden>Retry</button>`;
  const controller = new AbortController();
  let closed = false;
  const close = () => {
    if (closed) return;
    closed = true; controller.abort(); dialog.close(); dialog.remove();
    document.documentElement.style.overflow = overflow;
    previous?.focus?.({ preventScroll: true }); onClose();
  };
  dialog.addEventListener("cancel", e => { e.preventDefault(); close(); });
  dialog.addEventListener("close", close);
  dialog.addEventListener("click", e => {
    const b = dialog.getBoundingClientRect();
    if (e.target === dialog && (e.clientX < b.left || e.clientX > b.right || e.clientY < b.top || e.clientY > b.bottom)) close();
  });
  const status = dialog.querySelector('[role="status"]'), retry = dialog.querySelector("button");
  async function load() {
    status.textContent = "Loading…"; retry.hidden = true;
    try {
      const response = await fetch(`/api/mime?thread=chat_${instance}_${id}`, { signal: controller.signal });
      if (!response.ok) throw new Error();
      const data = await response.json();
      if (closed) return;
      const rows = data.replies.map(reply => {
        const row = document.createElement("article"), author = document.createElement("strong"), text = document.createElement("p");
        author.textContent = reply.name || "Anonymous"; text.textContent = reply.text || "";
        row.append(author, text); return row;
      });
      dialog.querySelector(".comments-list").replaceChildren(...rows);
      status.textContent = rows.length ? "" : "No comments yet.";
    } catch { if (!closed) { status.textContent = "Could not load comments."; retry.hidden = false; } }
  }
  retry.onclick = load;
  document.body.append(dialog); dialog.showModal(); dialog.querySelector("h2").focus(); load();
  return close;
}
