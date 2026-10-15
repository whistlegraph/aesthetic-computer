import {
  noPaintUI,
  paintingLayout,
  decisionBar,
  actionButton,
  updateActions,
  applyPalette,
} from "/aesthetic.computer/lib/ui/web.mjs";
import * as images from "./images.mjs";
import { openStore } from "./storage.mjs";
import { Account } from "./account.mjs";
import { Game, paintCapacity } from "./game.mjs";
import { publish } from "./publish.mjs";
import "/aesthetic.computer/lib/visit-tracker.mjs";
const $ = (id) => document.getElementById(id);
let game,
  store,
  paintImage = "",
  renderID = 0,
  peek = false,
  noHover = false,
  drag = null,
  mode = "zoom",
  pendingModel = null,
  locked = false,
  issue = "",
  lastPublication = "",
  historyData = [],
  workingLabel = "";
const account = new Account(() => {
  if (game) {
    game.prepare();
    render();
    if (account.value.connected)
      finishModelSignIn().catch((error) => notice(error.message));
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
    parts.push(model?.name || "Choose a model");
    if (s.busy) parts.push(`${(s.elapsed || 0).toFixed(1)}s`);
    if (price > 0)
      parts.push(
        `${price.toLocaleString()} braincells`,
        `${paintCapacity(price, s.account)} paints left`,
      );
    if (!model?.available)
      parts.push(
        s.account.connected ? "Unavailable" : "Sign in for cloud moves",
      );
    if (s.account.handle) parts.push(s.account.handle);
    if (s.account.stale) parts.push("last known balance");
  }
  $("status").textContent = parts.join(" · ");
  $("status").title = parts.join(" · ");
  $("progress").hidden = !s.busy;
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
  $("hint").hidden = !hintable;
  $("strength").value = String(s.strength);
  $("mode").value = mode;
  $("mode-label").textContent =
    mode === "zoom"
      ? "Drag the painting to crop."
      : "Brush the area to change. Hold Option/Alt to erase.";
  for (const id of ["strength", "mode"])
    $(id).disabled = !capabilities(s).canChange;
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
    if (["models", "account", "settings", "history"].includes(id)) {
      if (id === "models") {
        $("search").value = "";
        renderModels();
        open("models");
        $("model-list").scrollTop = 0;
      }
      if (id === "account") {
        renderAccount();
        open("account");
      }
      if (id === "settings") {
        $("mode-label").textContent =
          mode === "zoom"
            ? "Drag the painting to crop."
            : "Brush the area to change. Hold Option/Alt to erase.";
        open("brush-settings");
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
      const url = lastPublication;
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
      game.hint = $("hint").value;
      const before = game.accepted;
      await game.paint();
      if (game.accepted !== before) window.acVisits?.action("painting_edited");
    } else if (id === "no" || id === "undo") {
      if (!capabilities(snapshot()).canNo) return;
      noHover = false;
      await game.no();
    } else if (id === "fresh") {
      await game.fresh();
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
  const noticeText = issue || s.error || account.service?.detail || "";
  $("model-notice").replaceChildren();
  if (noticeText) {
    $("model-notice").append(document.createTextNode(noticeText));
    const retry = document.createElement("button");
    retry.textContent = "Try again";
    retry.onclick = () => {
      issue = "";
      game.retry();
      account.refresh();
    };
    $("model-notice").append(retry);
  }
  const choices = s.models;
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
        price.textContent =
          model.braincells === 0
            ? ""
            : model.braincells > 0
              ? `${model.braincells.toLocaleString()} Braincells`
              : "Not enabled on AC";
        if (!model.available && s.account.connected)
          price.textContent += " · " + modelAvailability(model, s.account);
        row.append(name, price);
        row.disabled = !s.canChange;
        row.onclick = async () => {
          try {
            if (model.id.startsWith("ac-") && !account.value.connected) {
              pendingModel = model.id;
              renderAccount();
              open("account");
              return;
            }
            await game.select(model.id);
            issue = "";
            $("models").close();
            render();
          } catch (error) {
            notice(error.message);
          }
        };
        const choice = document.createElement("div"),
          info = document.createElement("button");
        choice.className = "model-choice";
        info.type = "button";
        info.className = "model-info";
        info.textContent = "ⓘ";
        info.title = "Model details";
        info.setAttribute("aria-label", `Details about ${model.name}`);
        info.onclick = () => showModelDetails(model);
        choice.append(row, info);
        return choice;
      }),
  );
}
function modelAvailability(model, value) {
  if (!model.id.startsWith("ac-")) return "Ready";
  if (!model.serviceAvailable) return "Unavailable on AC";
  if (!value.connected) return "Sign in with AC";
  if (value.stale) return "Refresh your balance";
  if ((value.remaining || 0) + (value.purchased || 0) < model.braincells)
    return "Not enough Braincells";
  return "Ready";
}
function showModelDetails(model) {
  $("model-details-title").textContent = model.name;
  const cloud = model.id.startsWith("ac-"),
    settings = model.generation,
    rows = [["Runs", model.location || "AC cloud"]];
  if (cloud) {
    rows.push(["Model", model.model || model.id.slice(3)]);
    rows.push([
      "Per Paint",
      Number.isFinite(model.braincells)
        ? `${model.braincells.toLocaleString()} Braincells`
        : "Not configured",
    ]);
    if (Number.isFinite(model.braincells) && account.value.connected)
      rows.push([
        "Paints left",
        paintCapacity(model.braincells, account.value),
      ]);
    rows.push(["Status", modelAvailability(model, account.value)]);
    if (settings) {
      rows.push([
        "Generation",
        [
          settings.resolution ||
            settings.size?.join(" × ") ||
            "Provider default size",
          settings.aspect_ratio,
        ]
          .filter(Boolean)
          .join(" · "),
      ]);
      if (settings.quality) rows.push(["Quality", settings.quality]);
      if (settings.steps) rows.push(["Steps", settings.steps]);
      rows.push([
        "Seed",
        settings.seed ? "Sent to the model" : "Not sent to the model",
      ]);
    }
    rows.push(["Painting", `${(model.size || [256, 256]).join(" × ")} pixels`]);
    rows.push(["Hint", "Steers the next AI move"]);
    rows.push(["Preview", model.previews ? "While painting" : "Final result"]);
    rows.push([
      "Billing",
      "Paint confirms the price. Completed moves cost Braincells even after No.",
    ]);
  } else {
    const labels = {
      slice: "strip shifts",
      turn: "tile turns",
      sort: "pixel sorting",
      mosaic: "mosaic",
      hue: "channel rotation",
      tint: "tint",
      contrast: "contrast",
      posterize: "posterize",
      stroke: "curved strokes",
      shapes: "shapes",
      stipple: "stipple",
      hatch: "hatching",
      diffuse: "pixel diffusion",
    };
    const operations = images.FAMILIES[model.id];
    rows.push(["Operations", operations.map((op) => labels[op]).join(", ")]);
    rows.push(["Per Paint", "One operation, chosen from this palette"]);
    rows.push(["Change", "Small: 3 passes · Medium: 5 · Large: 8"]);
    rows.push(["Painting", "256 × 256 pixels"]);
  }
  $("model-details-list").replaceChildren(
    ...rows.flatMap(([label, value]) => {
      const term = document.createElement("dt"),
        detail = document.createElement("dd");
      term.textContent = label;
      detail.textContent = String(value);
      return [term, detail];
    }),
  );
  const link = $("model-source");
  link.hidden = !cloud;
  if (cloud) {
    link.href =
      model.provider === "openrouter" || model.id.startsWith("ac-openrouter:")
        ? `https://openrouter.ai/${model.model || model.id.slice("ac-openrouter:".length)}`
        : "https://fal.ai/models/fal-ai/flux-2/klein/4b/edit";
    link.textContent = "Provider details ↗";
  }
  open("model-details");
}
function renderAccount() {
  const s = snapshot(),
    a = s.account;
  $("account-title").textContent = a.connected
    ? "AC account"
    : "Sign in with AC";
  const model = s.models.find((m) => m.id === pendingModel);
  $("account-model").hidden = !model || a.connected;
  $("account-model").textContent = model ? `To use ${model.name}.` : "";
  $("signed-in").hidden = !a.connected;
  $("signin").hidden = !!a.connected;
  $("signout").disabled = s.busy || !!s.publication?.pending;
  $("balance").textContent =
    `${a.handle || ""} · ${((a.remaining || 0) + (a.purchased || 0)).toLocaleString()} Braincells${a.stale ? " (last known)" : ""}`;
  $("account-message").textContent = a.error || "";
}
async function finishModelSignIn() {
  const id = pendingModel || sessionStorage.getItem("nopaint-signin-model");
  if (!id || !account.value.connected || !game.canChange || locked) return;
  pendingModel = null;
  sessionStorage.removeItem("nopaint-signin-model");
  await game.select(id);
  issue = "";
  $("account").close();
  $("models").close();
  render();
}
async function commitMask() {
  if (!capabilities(snapshot()).canChange) return;
  game.mask = mask.slice();
  game.track.mask = mask.slice();
  await game.persist();
  game.prepare();
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
      r = 8;
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
        await game.crop(box);
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
  if (trackID) {
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
applyPalette();
$("decisions").append(decisionBar(dispatch));
$("band").prepend($("hint"));
const webActions = [
  "fresh",
  "save",
  "done",
  "history",
  "settings",
  "account",
  "published",
];
const actions = noPaintUI.menus.flatMap((m) => m.actions);
for (const id of webActions) {
  const spec = actions.find((a) => a.id === id);
  const label =
    { save: "Save", settings: "Brush", published: "Last painting" }[id] ||
    spec.label;
  $("studio-actions").append(
    actionButton({ ...spec, label }, (action) => {
      $("painting-actions").close();
      $("models").close();
      dispatch(action);
    }),
  );
}
for (const { label, value } of noPaintUI.strengths)
  $("strength").add(new Option(label, String(value)));
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
$("status").onclick = () => dispatch("models");
$("more-actions").onclick = () => open("painting-actions");
$("account").onclose = () => {
  pendingModel = null;
};
$("clear-mask").onclick = () => dispatch("clearMask");
$("strength").onchange = async () => {
  try {
    await game.changeStrength(Number($("strength").value));
  } catch (error) {
    notice(error.message);
  }
};
$("mode").onchange = () => {
  mode = $("mode").value;
  render();
};
$("signout").onclick = async () => {
  try {
    await account.logout();
    renderAccount();
    render();
  } catch (error) {
    $("account-message").textContent = error.message;
  }
};
$("redirect-login").onclick = async () => {
  try {
    await game.persist();
    if (pendingModel)
      sessionStorage.setItem("nopaint-signin-model", pendingModel);
    await account.login();
  } catch (error) {
    sessionStorage.removeItem("nopaint-signin-model");
    $("account-message").textContent = error.message;
  }
};
$("signin").onsubmit = async (e) => {
  e.preventDefault();
  $("sign-submit").disabled = true;
  try {
    if ($("code-label").hidden) {
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
$("history-export").onclick = () =>
  images.download(
    new Blob([JSON.stringify(historyData, null, 2)], {
      type: "application/json",
    }),
    "nopaint-history.json",
  );
function before(value) {
  peek = value;
  render();
}
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
  // Claim the painting before opening storage: a second tab must never recover
  // the first tab's pending request as if it had been interrupted.
  if (navigator.locks) {
    await new Promise((resolve, reject) => {
      navigator.locks
        .request("nopaint-ai-editor", { ifAvailable: true }, async (lock) => {
          locked = !lock;
          resolve();
          if (lock) await new Promise(() => {});
        })
        .catch(reject);
    });
  }
  store = await openStore();
  game = new Game({ store, images, account, onchange: () => render() });
  await game.boot({ readOnly: locked });
  mask = game.mask?.slice() || mask;
  lastPublication = (await store.setting("published")) || "";
  // Discard old pairing links without ever contacting a desktop backend.
  if (new URLSearchParams(location.hash.slice(1)).has("companion"))
    history.replaceState(null, "", location.pathname + location.search);
  sessionStorage.removeItem("nopaint-companion");
  account.boot().catch((error) => {
    issue = error.message;
    status();
  });
  setInterval(status, 250);
  setInterval(() => account.refresh(), 30000);
  render();
}
boot().catch((error) =>
  notice(`Your painting could not be opened: ${error.message}`),
);
