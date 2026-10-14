import {
  noPaintUI,
  paintingLayout,
  decisionBar,
  menu,
  updateActions,
  applyPalette,
} from "/aesthetic.computer/lib/ui/web.mjs";
import * as images from "./images.mjs";
import { openStore } from "./storage.mjs";
import { Account } from "./account.mjs";
import { Game, paintCapacity } from "./game.mjs";
import { Companion } from "./companion.mjs";
import { publish } from "./publish.mjs";
import "/aesthetic.computer/lib/visit-tracker.mjs";
const $ = (id) => document.getElementById(id);
let game,
  desktop,
  store,
  paintImage = "",
  renderID = 0,
  peek = false,
  noHover = false,
  drag = null,
  mode = "zoom",
  brush = 16,
  locked = false,
  issue = "",
  lastPublication = "",
  historyData = [],
  workingLabel = "",
  upscaleRequested = null;
const account = new Account(() => {
  if (game) {
    game.prepare();
    render();
  }
});
const canvas = $("canvas"),
  ctx = canvas.getContext("2d"),
  overlay = $("overlay"),
  overlayCtx = overlay.getContext("2d");
let mask = new Uint8Array(256 * 256),
  blending = false;
const reduced = matchMedia("(prefers-reduced-motion: reduce)");
function open(id) {
  if (!$(id).open) $(id).showModal();
}
function notice(message, title = "No Paint", retry = null) {
  $("notice-title").textContent = title;
  $("notice-message").textContent = message;
  $("notice-retry").hidden = !retry;
  $("notice-retry").onclick = () => {
    $("notice").close();
    retry?.();
  };
  open("notice");
}
function resize() {
  const rect = $("studio").getBoundingClientRect(),
    layout = paintingLayout(rect.width, rect.height);
  $("studio").style.setProperty("--side", layout.side + "px");
  $("studio").style.setProperty("--controls", layout.controls + "px");
  $("studio").style.setProperty(
    "--button-font",
    Math.min(
      layout.side / 7,
      Math.max(
        rect.height < noPaintUI.layout.compactHeight ? 24 : 32,
        layout.controls * 0.42,
      ),
    ) + "px",
  );
}
function snapshot() {
  if (desktop?.state) {
    const s = desktop.state;
    return {
      ...s,
      accepted: desktop.accepted,
      before: desktop.before,
      preview: desktop.preview,
      error: desktop.error || s.error,
      canChange:
        !s.busy &&
        !s.upscale?.busy &&
        !s.publication?.busy &&
        !s.publication?.pending &&
        !desktop.sending,
      canPaint:
        !!s.quote &&
        !s.busy &&
        !s.upscale?.busy &&
        !s.error &&
        !s.publication?.pending &&
        !desktop.sending,
      canNo: s.can_reject && !s.publication?.pending && !desktop.sending,
      canDone:
        s.account.connected &&
        !s.account.stale &&
        !s.busy &&
        !s.publication?.busy,
      canFresh:
        !s.busy && !s.publication?.busy && !s.upscale?.busy && !desktop.sending,
      canUpscale: !s.busy && !s.publication?.pending && !s.upscale?.busy,
      ready: true,
      hasPublished: !!s.publication?.url,
      remote: true,
    };
  }
  if (!game?.track) return { models: [], account: {}, ready: false };
  return {
    accepted: game.accepted,
    before: game.before,
    preview: game.preview,
    busy: game.busy || game.publishing,
    engine: game.engine,
    selection: game.selection,
    models: game.models,
    account: account.value,
    quote: game.quote,
    publication: game.track.publication,
    strength: game.strength,
    error: game.error,
    canChange: game.canChange,
    canFresh: !game.busy && !game.publishing,
    canPaint: game.canPaint,
    canNo:
      !game.publishing &&
      !game.track.publication?.pending &&
      (game.busy || game.track.steps.length > 1),
    canDone:
      account.value.connected &&
      !account.value.stale &&
      !game.busy &&
      !game.publishing,
    canUpscale: false,
    ready: true,
    hasPublished: !!lastPublication,
    elapsed: game.job ? (Date.now() - game.job.started) / 1000 : null,
  };
}
function capabilities(s) {
  return {
    ...s,
    canPaint:
      s.canPaint &&
      !game?.finishPending &&
      paintImage === s.accepted &&
      !peek &&
      !blending &&
      !drag &&
      !locked,
    canNo: s.canNo && !game?.finishPending && !peek && !drag && !locked,
    canChange: s.canChange && !game?.finishPending && !drag && !locked,
    canFresh: s.canFresh && !game?.finishPending && !drag && !locked,
    canDone: s.canDone && !game?.finishPending && !locked,
  };
}
function status() {
  const s = snapshot();
  if (!s.ready) return;
  const model = s.models.find((m) => m.id === s.engine),
    price = s.quote?.braincells ?? model?.braincells,
    parts = [];
  if (locked) parts.push("Open in another tab");
  else if (issue || s.error) parts.push(issue || s.error);
  else if (workingLabel) parts.push(workingLabel);
  else {
    if (s.selection === "random") parts.push("Random");
    parts.push(model?.name || "Choose a model");
    if (s.busy) parts.push(`${(s.elapsed || 0).toFixed(1)}s`);
    if (price != null)
      parts.push(
        `${price.toLocaleString()} braincells`,
        `${paintCapacity(price, s.account)} paints left`,
      );
    if (model?.companion) parts.push("Connect Desktop");
    else if (!model?.available)
      parts.push(
        s.account.connected ? "Unavailable" : "Sign in for cloud moves",
      );
    parts.push(s.account.handle || "Sign in");
    if (s.account.stale) parts.push("last known balance");
    if (s.remote) parts.push("Desktop");
  }
  $("status").textContent = parts.join(" · ");
  $("status").title = parts.join(" · ");
  $("progress").hidden = !s.busy && !s.upscale?.busy;
}
function drawMask() {
  overlayCtx.clearRect(0, 0, 256, 256);
  if (mode === "inpaint" && !peek && !noHover) {
    const data = overlayCtx.createImageData(256, 256);
    mask.forEach((v, i) => {
      if (v) data.data.set([218, 61, 112, 100], i * 4);
    });
    overlayCtx.putImageData(data, 0, 0);
  }
  if (drag && mode === "zoom") {
    overlayCtx.strokeStyle = "#fff";
    overlayCtx.lineWidth = 1;
    overlayCtx.setLineDash([3, 3]);
    overlayCtx.strokeRect(
      drag.start[0],
      drag.start[1],
      drag.end[0] - drag.start[0],
      drag.end[1] - drag.start[1],
    );
    overlayCtx.setLineDash([]);
  }
}
async function render() {
  const s = snapshot();
  updateActions(document, capabilities(s));
  status();
  if (!s.ready) return;
  const hintable = s.engine?.startsWith("ac-");
  $("hint").disabled = !s.canChange || !hintable || locked;
  $("hint").placeholder = hintable ? "hint" : "hint · cloud only";
  $("strength").value = String(s.strength);
  $("before").disabled = !s.before;
  $("hint").hidden = false;
  const url =
    peek || noHover ? s.before : s.busy && s.preview ? s.preview : s.accepted;
  if (url && url !== paintImage) {
    const id = ++renderID;
    const next = await images.read(url).catch(() => null);
    if (!next || id !== renderID) return;
    const previous = images.canvas();
    previous.getContext("2d").drawImage(canvas, 0, 0);
    const blend = paintImage && !peek && !noHover && !reduced.matches;
    paintImage = url;
    blending = true;
    const start = performance.now(),
      duration = s.busy ? 220 : 400;
    const frame = (now) => {
      if (id !== renderID) return;
      const t = blend ? Math.min(1, (now - start) / duration) : 1;
      ctx.globalAlpha = 1;
      ctx.drawImage(previous, 0, 0);
      ctx.globalAlpha = t * t * (3 - 2 * t);
      ctx.drawImage(next, 0, 0);
      ctx.globalAlpha = 1;
      if (t < 1) requestAnimationFrame(frame);
      else {
        blending = false;
        updateActions(document, capabilities(snapshot()));
      }
    };
    if (blend) requestAnimationFrame(frame);
    else frame(performance.now());
  }
  drawMask();
  if ($("models").open) renderModels();
}
async function dispatch(id) {
  try {
    if (!game) return;
    if (
      ["models", "account", "companion", "settings", "history"].includes(id)
    ) {
      if (id === "models") {
        renderModels();
        $("model-list").scrollTop = 0;
        open("models");
      }
      if (id === "account") {
        renderAccount();
        open("account");
      }
      if (id === "companion") open("companion");
      if (id === "settings") {
        $("mode-label").textContent =
          mode === "zoom"
            ? "Drag the painting to crop."
            : "Brush the area to change. Hold Option/Alt to erase.";
        open("options");
      }
      if (id === "history") await showHistory();
      return;
    }
    if (id === "save") {
      images.download(
        snapshot().accepted,
        `nopaint-${new Date().toISOString().replace(/[:.]/g, "-")}.png`,
      );
      return;
    }
    if (id === "published") {
      const url = desktop?.state?.publication?.url || lastPublication;
      if (url && /^https:\/\/aesthetic\.computer\/#?[A-Za-z0-9]+$/.test(url))
        window.open(url, "_blank", "noopener");
      return;
    }
    if (locked)
      throw Error(
        "This painting is open in another tab. Close that tab and reload here.",
      );
    issue = "";
    if (id === "done") {
      if (desktop) {
        await desktop.receive(
          await desktop.request("/api/done", {
            revision: desktop.state.revision,
            accepted: desktop.state.accepted,
          }),
        );
        return;
      }
      game.finishPending = true;
      render();
      try {
        lastPublication = await publish(game, (text) => {
          workingLabel = text;
          status();
        });
        window.acVisits?.action("painting_saved");
        workingLabel = "";
        await game.fresh();
        mask.fill(0);
      } finally {
        game.finishPending = false;
        render();
      }
      return;
    }
    if (id.startsWith("upscale")) {
      if (!desktop) {
        open("companion");
        return;
      }
      const scale = Number(id.slice(-1));
      await desktop.receive(
        await desktop.request("/api/upscale", {
          revision: desktop.state.revision,
          accepted: desktop.state.accepted,
          scale,
        }),
      );
      upscaleRequested = desktop.state.upscale?.id;
      return;
    }
    if (id === "zoom" || id === "inpaint") {
      mode = id;
      drawMask();
      return;
    }
    if (id === "clearMask") {
      mask.fill(0);
      await commitMask();
      return;
    }
    if (id === "paint") {
      if (!capabilities(snapshot()).canPaint) return;
      if (desktop) await desktop.act("paint", { hint: $("hint").value });
      else {
        game.hint = $("hint").value;
        const before = game.accepted;
        await game.paint();
        if (game.accepted !== before)
          window.acVisits?.action("painting_edited");
      }
    } else if (id === "no" || id === "undo") {
      if (!capabilities(snapshot()).canNo) return;
      noHover = false;
      if (desktop) await desktop.act(id === "undo" ? "undo" : "no");
      else await game.no();
    } else if (id === "fresh") {
      if (desktop) await desktop.act("restart", { start: "noise" });
      else await game.fresh();
      mask.fill(0);
    }
    render();
  } catch (error) {
    workingLabel = "";
    issue = error.message;
    status();
    notice(
      error.message,
      id === "done" ? "Done failed" : "No Paint",
      id === "done" ? () => dispatch("done") : null,
    );
  }
}
function renderModels() {
  const s = snapshot(),
    query = $("search").value.toLowerCase();
  const noticeText = s.error || account.service?.detail || "";
  $("model-notice").replaceChildren();
  if (!s.account.connected) {
    const login = document.createElement("button");
    login.textContent = "Sign in with AC";
    login.onclick = () => {
      $("models").close();
      dispatch("account");
    };
    $("model-notice").append(login);
  } else if (noticeText) {
    $("model-notice").append(document.createTextNode(noticeText));
  }
  if (s.models.some((m) => m.id.startsWith("ac-"))) {
    const note = document.createElement("small");
    note.textContent =
      "Paint confirms the displayed price. Completed cloud moves cost Braincells even after No.";
    $("model-notice").append(note);
  }
  const choices = [
    {
      id: "random",
      name: "Random",
      location: "Available models",
      available: true,
    },
    ...s.models,
  ];
  $("model-list").replaceChildren(
    ...choices
      .filter((m) =>
        `${m.name} ${m.id} ${m.location}`.toLowerCase().includes(query),
      )
      .map((model) => {
        const row = document.createElement("button");
        row.type = "button";
        row.className = "model-row";
        row.setAttribute("aria-pressed", String(model.id === s.selection));
        const name = document.createElement("div");
        name.textContent = model.name;
        const detail = document.createElement("small");
        detail.textContent = model.location || "";
        name.append(document.createElement("br"), detail);
        const price = document.createElement("span");
        price.textContent = model.companion
          ? "Connect Desktop"
          : model.braincells === 0
            ? "0 · ∞ paints"
            : model.braincells > 0
              ? `${model.braincells.toLocaleString()} · ${paintCapacity(model.braincells, s.account)} paints`
              : model.id === "random"
                ? ""
                : "Not enabled on AC";
        if (!model.available && !model.companion)
          price.textContent += " · unavailable";
        row.append(name, price);
        row.disabled = !s.canChange;
        row.onclick = async () => {
          try {
            if (model.companion) {
              $("models").close();
              open("companion");
              return;
            }
            if (desktop) await desktop.act("engine", { engine: model.id });
            else await game.select(model.id);
            issue = "";
            $("models").close();
            render();
          } catch (error) {
            notice(error.message);
          }
        };
        return row;
      }),
  );
}
function renderAccount() {
  const s = snapshot(),
    a = s.account;
  $("signed-in").hidden = !a.connected;
  $("signin").hidden = !!a.connected;
  $("signout").disabled = s.busy || !!s.publication?.pending;
  $("balance").textContent =
    `${a.handle || ""} · ${((a.remaining || 0) + (a.purchased || 0)).toLocaleString()} Braincells${a.stale ? " (last known)" : ""}`;
  $("account-message").textContent =
    a.error || (desktop ? "Using the desktop’s AC account." : "");
}
async function commitMask() {
  if (desktop)
    await desktop.act("mask", {
      mask: images.maskBits(mask),
      accepted: desktop.state.accepted,
    });
  else {
    game.mask = mask.slice();
    game.track.mask = mask.slice();
    await game.persist();
    game.prepare();
  }
  drawMask();
  render();
}
const point = (e) => {
  const r = overlay.getBoundingClientRect();
  return [
    Math.max(
      0,
      Math.min(255, Math.floor(((e.clientX - r.left) * 256) / r.width)),
    ),
    Math.max(
      0,
      Math.min(255, Math.floor(((e.clientY - r.top) * 256) / r.height)),
    ),
  ];
};
function mark(p, erase) {
  const from = drag?.last || p,
    dist = Math.hypot(p[0] - from[0], p[1] - from[1]),
    steps = Math.max(1, Math.ceil(dist));
  for (let step = 0; step <= steps; step++) {
    const cx = from[0] + ((p[0] - from[0]) * step) / steps,
      cy = from[1] + ((p[1] - from[1]) * step) / steps,
      r = brush / 2;
    for (
      let y = Math.max(0, Math.floor(cy - r));
      y <= Math.min(255, cy + r);
      y++
    )
      for (
        let x = Math.max(0, Math.floor(cx - r));
        x <= Math.min(255, cx + r);
        x++
      )
        if ((x - cx) ** 2 + (y - cy) ** 2 <= r * r)
          mask[y * 256 + x] = erase ? 0 : 1;
  }
  drag.last = p;
  drawMask();
}
overlay.onpointerdown = (e) => {
  if (!capabilities(snapshot()).canChange) return;
  e.preventDefault();
  const p = point(e);
  drag = { start: p, end: p, last: p };
  overlay.setPointerCapture(e.pointerId);
  if (mode === "inpaint") mark(p, e.altKey);
  render();
};
overlay.onpointermove = (e) => {
  if (!drag) return;
  drag.end = point(e);
  if (mode === "inpaint") mark(drag.end, e.altKey);
  else drawMask();
};
overlay.onpointerup = async () => {
  if (!drag) return;
  const current = drag;
  drag = null;
  try {
    if (mode === "inpaint") await commitMask();
    else {
      const box = [
        Math.min(current.start[0], current.end[0]),
        Math.min(current.start[1], current.end[1]),
        Math.max(current.start[0], current.end[0]) + 1,
        Math.max(current.start[1], current.end[1]) + 1,
      ];
      if (box[2] - box[0] > 2 && box[3] - box[1] > 2) {
        if (desktop)
          await desktop.act("crop", { box, accepted: desktop.state.accepted });
        else await game.crop(box);
        mask.fill(0);
      }
    }
  } catch (error) {
    notice(error.message);
  }
  render();
};
overlay.onpointercancel = () => {
  drag = null;
  drawMask();
  render();
};
async function showHistory(trackID) {
  let rows;
  if (desktop) {
    if (trackID) {
      historyData = await desktop.request(
        "/api/track?id=" + encodeURIComponent(trackID),
      );
      rows = await Promise.all(
        historyData.steps.map(async (s) => ({
          ...s,
          image: await desktop.image(s.image),
        })),
      );
    } else {
      const result = await desktop.request("/api/tracks");
      historyData = result;
      rows = await Promise.all(
        result.paintings.map(async (t) => ({
          ...t,
          image: await desktop.image(t.image),
          label: t.started_at,
          trackID: t.id,
        })),
      );
    }
  } else if (trackID) {
    const track = await store.get(trackID);
    historyData = track;
    rows = [
      ...track.steps,
      ...track.events
        .filter((e) => e.step && !track.steps.some((s) => s.id === e.step.id))
        .map((e) => ({ ...e.step, rejected: true })),
    ].sort((a, b) => a.at - b.at);
  } else {
    const tracks = (await store.all()).sort(
      (a, b) => b.createdAt - a.createdAt,
    );
    historyData = tracks;
    rows = tracks.map((t) => ({
      image: t.steps.at(-1).image,
      label: new Date(t.createdAt).toLocaleString(),
      trackID: t.id,
    }));
  }
  $("history-list").replaceChildren(
    ...rows.map((step) => {
      const b = document.createElement("button"),
        im = document.createElement("img"),
        label = document.createElement("small");
      im.src = step.image;
      im.alt = step.rejected ? "Rejected move" : "Painting";
      label.textContent =
        step.label || step.model || step.kind || step.action || "Painting";
      b.append(im, label);
      b.classList.toggle("rejected", !!step.rejected);
      b.onclick = () =>
        step.trackID
          ? showHistory(step.trackID)
          : notice(
              step.receipt?.prompt ||
                JSON.stringify(
                  step.receipt || {
                    action: step.kind || step.action,
                    seed: step.seed,
                  },
                  null,
                  2,
                ),
              "Move",
            );
      return b;
    }),
  );
  open("history");
}
async function connect(token) {
  const next = new Companion(token, () => {
    if (!drag) mask = next.mask || mask;
    render();
  });
  await next.poll();
  if (!next.state)
    throw Error(
      "Could not connect to No Paint. Keep the desktop app running and allow local network access in your browser.",
    );
  desktop = next;
  sessionStorage.setItem("nopaint-companion", token);
  $("companion-disconnect").hidden = false;
  $("companion-message").textContent = "Connected to the desktop painting.";
  mask = next.mask;
  render();
}
applyPalette();
$("decisions").append(decisionBar(dispatch));
$("band").prepend($("hint"));
for (const spec of noPaintUI.menus) {
  const item = menu(spec, dispatch);
  if (spec.id === "file") $("menus").prepend(item);
  else $("menus").append(item);
}
for (const { label, value } of noPaintUI.strengths)
  $("strength").add(new Option(label, String(value)));
for (const size of noPaintUI.brushes)
  $("brush").add(new Option(`${size} px`, String(size)));
$("brush").value = "16";
$("hint").maxLength = noPaintUI.components.find((c) => c.kind === "hint").limit;
$("hint").onkeydown = (e) => {
  if (e.key === "Enter") {
    e.preventDefault();
    dispatch("paint");
    $("hint").blur();
  }
};
document
  .querySelectorAll("[data-close]")
  .forEach((b) => (b.onclick = () => b.closest("dialog").close()));
$("search").oninput = renderModels;
$("status").onclick = () => {
  const error = issue || snapshot().error;
  if (error)
    notice(error, "No Paint", () => {
      issue = "";
      game.retry();
      if (desktop) desktop.act("retry").catch((e) => notice(e.message));
    });
  else dispatch("models");
};
$("account-open").onclick = () => dispatch("account");
$("companion-open").onclick = () => dispatch("companion");
$("clear-mask").onclick = () => dispatch("clearMask");
$("strength").onchange = async () => {
  try {
    if (desktop)
      await desktop.act("strength", { strength: Number($("strength").value) });
    else await game.changeStrength(Number($("strength").value));
  } catch (error) {
    notice(error.message);
  }
};
$("brush").onchange = () => {
  brush = Number($("brush").value);
};
$("signout").onclick = async () => {
  try {
    if (desktop)
      await desktop.receive(
        await desktop.request("/api/account", { action: "disconnect" }),
      );
    else await account.logout();
    renderAccount();
    render();
  } catch (error) {
    $("account-message").textContent = error.message;
  }
};
$("redirect-login").onclick = async () => {
  try {
    if (desktop)
      await desktop.receive(
        await desktop.request("/api/account", { action: "connect" }),
      );
    else {
      await game.persist();
      await account.login();
    }
  } catch (error) {
    $("account-message").textContent = error.message;
  }
};
$("signin").onsubmit = async (e) => {
  e.preventDefault();
  $("sign-submit").disabled = true;
  try {
    if (desktop) {
      await desktop.receive(
        await desktop.request("/api/account", { action: "connect" }),
      );
    } else if ($("code-label").hidden) {
      await account.otp.sendCode($("email").value);
      $("code-label").hidden = false;
      $("code").required = true;
      $("email").readOnly = true;
      $("sign-submit").textContent = "Sign in";
      $("code").focus();
      $("account-message").textContent =
        "Check your email for the six-digit code.";
    } else {
      await account.otp.verify($("email").value, $("code").value);
      await account.refresh();
      renderAccount();
      if (account.value.connected) $("account").close();
    }
  } catch (error) {
    $("account-message").textContent = error.hint || error.message;
  } finally {
    $("sign-submit").disabled = false;
  }
};
$("catalog").onclick = async () => {
  try {
    let rows;
    if (desktop) rows = (await desktop.request("/api/models")).models;
    else {
      const response = await fetch(
        "https://openrouter.ai/api/v1/images/models",
        { signal: AbortSignal.timeout(15000) },
      );
      if (!response.ok) throw Error("OpenRouter catalog unavailable.");
      const data = await response.json();
      rows = (data.data || []).filter(
        (m) =>
          m.architecture?.input_modalities?.includes("image") &&
          m.architecture?.output_modalities?.includes("image") &&
          (m.supported_parameters?.input_references?.max || 0) >= 1 &&
          (m.supported_parameters?.input_references?.min || 0) <= 1 &&
          JSON.stringify(m.supported_parameters?.output_format?.values) !==
            '["svg"]',
      );
      for (const row of rows)
        if (!account.models.some((m) => m.id === "ac-openrouter:" + row.id))
          account.models.push({
            id: "ac-openrouter:" + row.id,
            model: row.id,
            name: row.name,
            location: "AC cloud · OpenRouter",
            available: false,
          });
    }
    if (desktop) await desktop.poll();
    renderModels();
  } catch (error) {
    $("model-notice").textContent = error.message;
  }
};
$("history-export").onclick = () =>
  images.download(
    new Blob([JSON.stringify(historyData, null, 2)], {
      type: "application/json",
    }),
    "nopaint-history.json",
  );
$("companion-disconnect").onclick = () => {
  desktop = null;
  sessionStorage.removeItem("nopaint-companion");
  mask = game.mask?.slice() || new Uint8Array(256 * 256);
  $("companion-disconnect").hidden = true;
  $("companion-message").textContent = "Browser painting restored.";
  render();
};
function before(value) {
  peek = value;
  render();
}
$("before").onpointerdown = (e) => {
  e.preventDefault();
  $("before").setPointerCapture(e.pointerId);
  before(true);
};
$("before").onpointerup = $("before").onpointercancel = () => before(false);
const no = document.querySelector(".decisions [data-action=no]");
no.onpointerenter = (e) => {
  if (e.pointerType === "mouse" && !no.disabled) {
    noHover = true;
    render();
  }
};
no.onpointerleave = () => {
  noHover = false;
  render();
};
document.addEventListener("keydown", (e) => {
  if (
    e.target.matches("input,textarea,select") ||
    document.querySelector("dialog[open]")
  )
    return;
  if (e.code === "Space") {
    e.preventDefault();
    if (!e.repeat) before(true);
    return;
  }
  if (e.repeat) return;
  const key = e.key.toLowerCase();
  if (e.metaKey || e.ctrlKey) {
    const action = { n: "fresh", s: "save", d: "done" }[key];
    if (action) {
      e.preventDefault();
      dispatch(action);
    }
    return;
  }
  const action =
    key === "u"
      ? "undo"
      : noPaintUI.components
          .find((c) => c.kind === "decisions")
          .actions.find((a) => a.key === key)?.id;
  if (action) {
    e.preventDefault();
    dispatch(action);
  }
});
document.addEventListener("keyup", (e) => {
  if (e.code === "Space") before(false);
});
window.addEventListener("blur", () => {
  peek = noHover = false;
  render();
});
new ResizeObserver(resize).observe($("studio"));
resize();
async function boot() {
  store = await openStore();
  game = new Game({ store, images, account, onchange: () => render() });
  await game.boot();
  mask = game.mask?.slice() || mask;
  lastPublication = (await store.setting("published")) || "";
  const params = new URLSearchParams(location.hash.slice(1)),
    token =
      params.get("companion") || sessionStorage.getItem("nopaint-companion");
  if (params.has("companion"))
    history.replaceState(null, "", location.pathname + location.search);
  if (token)
    await connect(token).catch((error) => {
      sessionStorage.removeItem("nopaint-companion");
      notice(error.message);
    });
  account.boot().catch((error) => {
    issue = error.message;
    status();
  });
  if (navigator.locks)
    navigator.locks.request(
      "nopaint-ai-editor",
      { ifAvailable: true },
      async (lock) => {
        if (!lock) {
          locked = true;
          render();
          return;
        }
        await new Promise(() => {});
      },
    );
  setInterval(async () => {
    if (desktop) {
      await desktop.poll();
      const upscale = desktop.state?.upscale;
      if (
        upscaleRequested &&
        upscale?.id === upscaleRequested &&
        !upscale.busy
      ) {
        upscaleRequested = null;
        if (upscale.url)
          images.download(
            await desktop.image(upscale.url),
            `nopaint-${Date.now()}-${upscale.scale}x.png`,
          );
        else if (upscale.error) notice(upscale.error, "Upscale failed");
      }
    }
    status();
  }, 250);
  setInterval(() => {
    if (!desktop) account.refresh();
  }, 30000);
  render();
}
boot().catch((error) =>
  notice(`Your painting could not be opened: ${error.message}`),
);
