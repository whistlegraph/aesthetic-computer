// cal — calendar, 2026.06.15
// View, add, edit, and delete appointments. Ships MONTH, WEEK, and DAY views.
// The DateWizard is the primary pop-up: it opens on the focused WEEK as a
// readout, and folds out to an add/edit form. All date-math (grid + week/day
// layout + recurrence expansion + event bucketing) lives in the shared engine
// lib/cal.mjs so every view reuses it. Events come from /api/cal when signed in,
// or from a device-local store when not.

import {
  WEEKDAYS,
  MONTHS,
  MONTHS_SHORT,
  buildMonthGrid,
  layoutEvents,
  monthRange,
  addMonths,
  addWeeks,
  weekStart,
  weekLayout,
  dayLayout,
  expandRecurrences,
  isoOf,
  isoDate,
  eventTimeLabel,
} from "../lib/cal.mjs";

/* #region 📚 README
  Colon params → view + date:
    cal              → month view, today
    cal:2026:6       → month view, June 2026  (year:month, month 1-indexed)
    cal:add          → DateWizard add-form open on today
    cal:wizard       → DateWizard WEEK readout open (the primary pop-up)
    cal:week         → top-level WEEK view (focused week, hour-less day columns)
    cal:day          → top-level DAY view (single day, hour rows)

  Controls (calendar views):
    Tap a day        — focus it (month: its events list at the bottom)
    ← / →            — page (month pages months; week pages weeks; day pages days)
    "wizard" button  — open the DateWizard on the focused week
    "add" button     — open the DateWizard add-form on the focused day
    Tap an event     — (focused-day list / week / day) open it to EDIT or DELETE
    Esc / `          — back to prompt

  DateWizard (the primary pop-up, two modes):
    WEEK readout — 7 day rows for the focused week (today highlighted) listing
      each day's events (time + title). Tap a day row's "+" to start an ADD on
      that day; tap an event to open the EDIT/DELETE form; ‹ / › page weeks;
      "Add" jumps to a blank add-form on the focused day; "Close" dismisses.
    ADD / EDIT form — fields stack: Title, Start, End, Note, plus a private/
      public Visibility toggle. ↑/↓ (or tap a field) move the active field;
      typing edits Title/Note; Start/End are time steppers (−/+ or ←/→). On a
      new event "Add" POSTs; on an existing event "Save" PUTs and a "Delete"
      button DELETEs. "Back" returns to the week readout. Writes go through the
      same auth-vs-local path the calendar fetch uses.
#endregion */

const FONT = "MatrixChunky8";

const COLORS = {
  bg: [12, 14, 22],
  header: [150, 160, 200],
  weekend: [110, 120, 160],
  grid: [38, 42, 60],
  cellIn: [22, 26, 38],
  cellOut: [14, 16, 24],
  dayNum: [170, 180, 210],
  dayNumOut: [70, 78, 100],
  today: [255, 210, 110],
  todayFill: [60, 48, 18],
  focus: [120, 200, 255],
  focusFill: [22, 40, 60],
  event: [120, 230, 160],
  eventDot: [120, 230, 160],
  recur: [200, 170, 255],
  title: [220, 226, 245],
  dim: [90, 100, 130],
  error: [255, 120, 120],
  delete: [255, 120, 120],
};

// 🌐 State (module-level; reset in boot since this module can survive nav).
let view = "month"; // "month" | "week" | "day"
let cursor = { year: 2026, month: 6 }; // visible month (month 1-indexed)
let focusDate = new Date(); // anchor Date for week/day views + week paging
let today = { iso: "", year: 0, month: 0, day: 0 };
let weeks = []; // buildMonthGrid result, with events bucketed in (month view)
let weekModel = null; // weekLayout() result (week view)
let dayModel = null; // dayLayout() result (day view)
let events = []; // raw events for the visible range (pre-expansion)
let focusIso = null; // YYYY-MM-DD of the focused day cell
let loading = true;
let error = null;
let signedIn = false; // drives net.userRequest vs. local store
let simClock = 0; // frame counter for the ~1/min "now" refresh
let frameCount = 0;

// Captured AC handles (so async fetch + button callbacks can reach them).
let $net = null;
let $store = null;
let $send = null; // for keyboard:open / keyboard:close around the wizard

// UI buttons (built in boot, painted in paint, handled in act).
let prevBtn = null;
let nextBtn = null;
let todayBtn = null;
let addBtn = null;
let wizardBtn = null;

// 🧙 DateWizard — the primary pop-up. `wizard` is null when closed. It has two
// modes:
//   mode:"week"  → the week readout (rows of days + their events). Carries
//                  `hitboxes` (day "+" boxes, event rows, pager) rebuilt each
//                  paint so act() can tap without re-deriving geometry.
//   mode:"form"  → the add/edit form. `formMode` is "add" or "edit". Form state:
//                  iso, title, note, startMin, endMin, visibility, field,
//                  editUid (the event being edited, or null), saving, hitboxes.
let wizard = null;
const WIZ_FIELDS = ["title", "start", "end", "note", "visibility"];
let cancelBtn = null; // form: "Back" → week readout
let confirmBtn = null; // form: "Add"/"Save"
let deleteBtn = null; // form (edit): "Delete"
let weekCloseBtn = null; // week readout: "Close"
let weekAddBtn = null; // week readout: "Add" (blank form on focused day)
let weekPrevBtn = null; // week readout: "‹"
let weekNextBtn = null; // week readout: "›"

// 🗝️ Local-calendar storage key (device-local fallback when signed out).
const LOCAL_KEY = "cal:events:local";

// ──────────────────────────────────────────────────────────────────────────

function boot({ colon, store, net, handle, user, ui, screen, send }) {
  $net = net;
  $store = store;
  $send = send;

  // Reset per-entry state (module is a session singleton).
  events = [];
  weeks = [];
  weekModel = null;
  dayModel = null;
  focusIso = null;
  loading = true;
  error = null;
  simClock = 0;
  frameCount = 0;
  wizard = null;

  signedIn = !!(handle?.() || user);

  const now = new Date();
  today = {
    iso: isoOf(now),
    year: now.getFullYear(),
    month: now.getMonth() + 1,
    day: now.getDate(),
  };
  focusDate = now;

  // Parse view + date from the colon params.
  //   cal:week / cal:day → those top-level views.
  //   cal:YYYY:MM        → month view on that month.
  //   cal:add            → open the DateWizard add-form on today.
  //   cal:wizard         → open the DateWizard week readout (the primary pop-up).
  const first = colon?.[0];
  const openAddOnBoot = first === "add";
  const openWeekWizardOnBoot = first === "wizard";
  if (first === "week") view = "week";
  else if (first === "day") view = "day";
  else view = "month";

  if (first && /^\d{4}$/.test(first)) {
    cursor.year = parseInt(first, 10);
    cursor.month = colon[1] ? clampMonth(parseInt(colon[1], 10)) : today.month;
  } else {
    cursor.year = today.year;
    cursor.month = today.month;
  }

  // Restore the last view (so reopening `cal` lands where you left off), unless
  // the colon explicitly asked for one.
  if (!first || first === "add" || first === "wizard") {
    store.retrieve("cal:view", "local").then((saved) => {
      if (saved === "week" || saved === "day" || saved === "month") view = saved;
    });
  } else {
    store.persist("cal:view", view, "local");
  }

  // Focus today if it's in the visible month, else the 1st.
  focusIso =
    cursor.year === today.year && cursor.month === today.month
      ? today.iso
      : isoDate(cursor.year, cursor.month, 1);

  buildButtons({ ui, screen });
  rebuildModels();
  fetchRange();

  // cal:add opens the add-form straight away; cal:wizard opens the week readout.
  if (openAddOnBoot) openAddForm(focusIso);
  else if (openWeekWizardOnBoot) openWeekWizard();
}

function clampMonth(m) {
  if (isNaN(m) || m < 1) return 1;
  if (m > 12) return 12;
  return m;
}

// 🧩 Expand recurrences over the visible range, then rebuild whichever view
// model is active. Called after every fetch / paging / mutation so all views
// (and the wizard) re-layout from one place.
function rebuildModels() {
  if (view === "week") {
    const { from, to } = weekLayout(focusDate, []); // cheap range probe
    const expanded = expandRecurrences(events, from, to);
    weekModel = weekLayout(focusDate, expanded);
  } else if (view === "day") {
    const { from, to } = dayLayout(focusDate, []);
    const expanded = expandRecurrences(events, from, to);
    dayModel = dayLayout(focusDate, expanded);
  } else {
    weeks = buildMonthGrid(cursor.year, cursor.month);
    const { from, to } = monthRange(cursor.year, cursor.month);
    const expanded = expandRecurrences(events, from, to);
    layoutEvents(weeks, expanded);
  }
  // The wizard's week readout always re-layouts its own week from the raw set.
  if (wizard && wizard.mode === "week") rebuildWizardWeek();
}

// The wizard week readout uses the wizard's own `weekDate` anchor (it pages
// independently of the underlying calendar view), expanding recurrences first.
function rebuildWizardWeek() {
  const { from, to } = weekLayout(wizard.weekDate, []);
  const expanded = expandRecurrences(events, from, to);
  wizard.week = weekLayout(wizard.weekDate, expanded);
}

// 📡 Fetch the visible range's events (auth → /api/cal; else local store). The
// range covers whatever the active view needs (month grid / week / day).
async function fetchRange() {
  loading = true;
  error = null;
  const { from, to } = currentRange();
  try {
    if (signedIn && $net?.userRequest) {
      const res = await $net.userRequest(
        "GET",
        `/api/cal?from=${encodeURIComponent(from)}&to=${encodeURIComponent(to)}`,
      );
      if (res?.message === "unauthorized") {
        // Token went stale → fall back to the local calendar.
        signedIn = false;
        await loadLocal();
      } else {
        events = res?.events || [];
      }
    } else {
      await loadLocal();
    }
  } catch (err) {
    error = err.message || "fetch failed";
    events = [];
  }
  loading = false;
  rebuildModels();
}

// The [from,to] ISO range the active view needs. We always over-fetch a little
// (month for month view; the focused week/day for those) so recurrence
// expansion has the master events to work from.
function currentRange() {
  if (view === "week") {
    const { from, to } = weekLayout(focusDate, []);
    return { from, to };
  }
  if (view === "day") {
    const { from, to } = dayLayout(focusDate, []);
    return { from, to };
  }
  return monthRange(cursor.year, cursor.month);
}

// Device-local calendar (signed-out fallback). Pulls the whole local store; we
// keep recurring masters (their rrule may project an instance into range) and
// any concrete event overlapping the range. expandRecurrences runs downstream.
async function loadLocal() {
  const all = (await $store?.retrieve(LOCAL_KEY, "local")) || [];
  const { from, to } = currentRange();
  const lo = new Date(from);
  const hi = new Date(to);
  events = all.filter((e) => {
    if (e.rrule) return true; // let expansion decide.
    const s = new Date(e.start);
    const en = e.end ? new Date(e.end) : s;
    return !isNaN(s) && en >= lo && s <= hi;
  });
}

async function persistLocal(event) {
  const all = (await $store?.retrieve(LOCAL_KEY, "local")) || [];
  all.push(event);
  $store?.persist(LOCAL_KEY, all, "local");
}

async function updateLocal(uid, fields) {
  const all = (await $store?.retrieve(LOCAL_KEY, "local")) || [];
  const idx = all.findIndex((e) => e.uid === uid);
  if (idx >= 0) all[idx] = { ...all[idx], ...fields };
  $store?.persist(LOCAL_KEY, all, "local");
}

async function deleteLocal(uid) {
  const all = (await $store?.retrieve(LOCAL_KEY, "local")) || [];
  $store?.persist(LOCAL_KEY, all.filter((e) => e.uid !== uid), "local");
}

// ✍️ Create an event through the auth-vs-local path (POST /api/cal, else local).
async function commitEvent(body) {
  if (signedIn && $net?.userRequest) {
    const res = await $net.userRequest("POST", "/api/cal", body);
    if (res?.event) {
      events = [...events, res.event];
    } else if (res?.message === "unauthorized") {
      signedIn = false;
      await persistLocal({ uid: localUid(), ...body });
      await loadLocal();
    } else {
      error = res?.message || "add failed";
    }
  } else {
    await persistLocal({ uid: localUid(), ...body });
    await loadLocal();
  }
  rebuildModels();
}

// ✏️ Update an existing event (PUT /api/cal {uid,...}, else local). Mirrors the
// commitEvent local-store fallback so unauthenticated edits persist too.
async function updateEvent(uid, fields) {
  if (signedIn && $net?.userRequest) {
    const res = await $net.userRequest("PUT", "/api/cal", { uid, ...fields });
    if (res?.event) {
      events = events.map((e) => (e.uid === uid ? res.event : e));
    } else if (res?.message === "unauthorized") {
      signedIn = false;
      await updateLocal(uid, fields);
      await loadLocal();
    } else {
      error = res?.message || "save failed";
    }
  } else {
    await updateLocal(uid, fields);
    await loadLocal();
  }
  rebuildModels();
}

// 🗑️ Delete an event (DELETE /api/cal?uid=, else local).
async function deleteEvent(uid) {
  if (signedIn && $net?.userRequest) {
    const res = await $net.userRequest(
      "DELETE",
      `/api/cal?uid=${encodeURIComponent(uid)}`,
    );
    if (res?.ok) {
      events = events.filter((e) => e.uid !== uid);
    } else if (res?.message === "unauthorized") {
      signedIn = false;
      await deleteLocal(uid);
      await loadLocal();
    } else {
      error = res?.message || "delete failed";
    }
  } else {
    await deleteLocal(uid);
    await loadLocal();
  }
  rebuildModels();
}

function localUid() {
  return "local-" + Date.now().toString(36) + Math.random().toString(36).slice(2, 6);
}

// 🧙 DateWizard ────────────────────────────────────────────────────────────────

// Open the wizard's WEEK readout on the focused day's week (the primary pop-up).
function openWeekWizard() {
  const anchor = isoToDate(focusIso) || focusDate || new Date();
  wizard = {
    mode: "week",
    weekDate: weekStart(anchor),
    week: null,
    hitboxes: null,
  };
  rebuildWizardWeek();
}

// Page the wizard's week readout by `delta` weeks.
function wizardPageWeek(delta) {
  if (!wizard || wizard.mode !== "week") return;
  wizard.weekDate = addWeeks(wizard.weekDate, delta);
  rebuildWizardWeek();
}

// Open the ADD form on an ISO day, defaulting 12:00→13:00. Sends keyboard:open
// so touch devices raise a soft keyboard for the Title field.
function openAddForm(iso) {
  wizard = {
    mode: "form",
    formMode: "add",
    iso: iso || focusIso || today.iso,
    editUid: null,
    title: "",
    note: "",
    startMin: 12 * 60, // 12:00
    endMin: 13 * 60, // 13:00
    visibility: "private",
    field: 0,
    hitboxes: null,
    saving: false,
  };
  $send?.({ type: "keyboard:open" });
}

// Open the EDIT form prefilled from an existing event. Recurring instances edit
// their master document (masterUid), so the whole series moves consistently.
function openEditForm(ev) {
  const start = ev.start ? new Date(ev.start) : new Date();
  const end = ev.end ? new Date(ev.end) : start;
  wizard = {
    mode: "form",
    formMode: "edit",
    iso: isoOf(start),
    editUid: ev.masterUid || ev.uid,
    title: ev.title || "",
    note: ev.note || "",
    startMin: start.getHours() * 60 + start.getMinutes(),
    endMin: end.getHours() * 60 + end.getMinutes(),
    visibility: ev.visibility === "public" ? "public" : "private",
    field: 0,
    hitboxes: null,
    saving: false,
  };
  $send?.({ type: "keyboard:open" });
}

function closeWizard() {
  wizard = null;
  $send?.({ type: "keyboard:close" });
}

// Form → back to the week readout (anchored on the form's day).
function formBackToWeek() {
  const anchor = isoToDate(wizard?.iso) || focusDate || new Date();
  wizard = { mode: "week", weekDate: weekStart(anchor), week: null, hitboxes: null };
  rebuildWizardWeek();
  $send?.({ type: "keyboard:close" });
}

// Move the active form field, wrapping. Used by ↑/↓ and tapping a row.
function wizardMoveField(delta) {
  if (!wizard || wizard.mode !== "form") return;
  const n = WIZ_FIELDS.length;
  wizard.field = (wizard.field + delta + n) % n;
}

// Nudge the active time field by `mins`, clamped to the day. The end is kept
// after the start (and the start kept before the end) so the range stays valid.
function wizardNudgeTime(mins) {
  if (!wizard || wizard.mode !== "form") return;
  if (wizard.field === 1) {
    wizard.startMin = clampMin(wizard.startMin + mins);
    if (wizard.endMin <= wizard.startMin)
      wizard.endMin = clampMin(wizard.startMin + 30);
  } else if (wizard.field === 2) {
    wizard.endMin = clampMin(wizard.endMin + mins);
    if (wizard.endMin <= wizard.startMin)
      wizard.startMin = clampMin(wizard.endMin - 30);
  }
}

function wizardToggleVisibility() {
  if (!wizard || wizard.mode !== "form") return;
  wizard.visibility = wizard.visibility === "public" ? "private" : "public";
}

function clampMin(m) {
  if (m < 0) return 0;
  if (m > 23 * 60 + 59) return 23 * 60 + 59;
  return m;
}

// Append a printable character to the active text field (Title / Note).
function wizardType(ch) {
  if (!wizard || wizard.mode !== "form") return;
  const key = WIZ_FIELDS[wizard.field];
  if (key !== "title" && key !== "note") return;
  if (wizard[key].length >= 64) return;
  wizard[key] += ch;
}

function wizardBackspace() {
  if (!wizard || wizard.mode !== "form") return;
  const key = WIZ_FIELDS[wizard.field];
  if (key !== "title" && key !== "note") return;
  wizard[key] = wizard[key].slice(0, -1);
}

// Build the event body from the form and POST (add) or PUT (edit), then return
// to the week readout + refresh.
async function confirmWizard() {
  if (!wizard || wizard.mode !== "form" || wizard.saving) return;
  wizard.saving = true;
  const { iso, startMin, endMin, note, visibility, formMode, editUid } = wizard;
  const [y, m, d] = iso.split("-").map((n) => parseInt(n, 10));
  const title = wizard.title.trim() || "New appointment";
  const start = new Date(y, m - 1, d, Math.floor(startMin / 60), startMin % 60, 0).toISOString();
  const end = new Date(y, m - 1, d, Math.floor(endMin / 60), endMin % 60, 0).toISOString();
  const body = {
    title,
    start,
    end,
    note: note.trim() || undefined,
    allDay: false,
    visibility,
  };
  focusIso = iso; // keep the touched day focused for the bottom panel
  if (formMode === "edit" && editUid) {
    await updateEvent(editUid, body);
  } else {
    await commitEvent(body);
  }
  // Land back on the week readout so the user sees the result in context.
  formBackToWeek();
}

// Delete the event being edited, then return to the week readout.
async function deleteFromWizard() {
  if (!wizard || wizard.mode !== "form" || !wizard.editUid || wizard.saving) return;
  wizard.saving = true;
  const uid = wizard.editUid;
  await deleteEvent(uid);
  formBackToWeek();
}

// "1:05 PM" style label for a minutes-since-midnight value.
function timeLabel(mins) {
  let h = Math.floor(mins / 60);
  const mm = String(mins % 60).padStart(2, "0");
  const ap = h < 12 ? "AM" : "PM";
  h = h % 12;
  if (h === 0) h = 12;
  return `${h}:${mm} ${ap}`;
}

// 🧭 Paging — month view pages months; week pages weeks; day pages days.
function pageView(delta) {
  if (view === "week") {
    focusDate = addWeeks(focusDate, delta);
    syncCursorToFocus();
  } else if (view === "day") {
    focusDate = addDays(focusDate, delta);
    syncCursorToFocus();
  } else {
    const next = addMonths(cursor.year, cursor.month, delta);
    cursor.year = next.year;
    cursor.month = next.month;
    focusIso =
      cursor.year === today.year && cursor.month === today.month
        ? today.iso
        : isoDate(cursor.year, cursor.month, 1);
  }
  rebuildModels();
  fetchRange();
}

function goToday() {
  const now = new Date();
  focusDate = now;
  cursor.year = today.year;
  cursor.month = today.month;
  focusIso = today.iso;
  rebuildModels();
  fetchRange();
}

// Keep cursor/focusIso aligned with focusDate so switching views is coherent.
function syncCursorToFocus() {
  cursor.year = focusDate.getFullYear();
  cursor.month = focusDate.getMonth() + 1;
  focusIso = isoOf(focusDate);
}

function addDays(date, delta) {
  const d = new Date(date.getFullYear(), date.getMonth(), date.getDate());
  d.setDate(d.getDate() + delta);
  return d;
}

function isoToDate(iso) {
  if (!iso) return null;
  const [y, m, d] = iso.split("-").map((n) => parseInt(n, 10));
  if (!y || !m || !d) return null;
  return new Date(y, m - 1, d);
}

// 🔘 Buttons ──────────────────────────────────────────────────────────────────
function buildButtons({ ui, screen }) {
  prevBtn = new ui.TextButton("<", { left: 4, top: 4, screen });
  nextBtn = new ui.TextButton(">", { right: 4, top: 4, screen });
  todayBtn = new ui.TextButton("today", { left: 4, bottom: 4, screen });
  addBtn = new ui.TextButton("add", { right: 4, bottom: 4, screen });
  wizardBtn = new ui.TextButton("wizard", { center: "x", bottom: 4, screen });
}

function syncButtons({ screen }) {
  if (!prevBtn) return;
  prevBtn.reposition({ left: 4, top: 4, screen });
  nextBtn.reposition({ right: 4, top: 4, screen });
  todayBtn.reposition({ left: 4, bottom: 4, screen });
  addBtn.reposition({ right: 4, bottom: 4, screen });
  wizardBtn.reposition({ center: "x", bottom: 4, screen });
}

const BTN_SCHEME = [[20, 24, 34], [80, 90, 120], [180, 190, 220]];
const BTN_HOVER = [[30, 35, 50], [140, 160, 210], [220, 230, 255]];
const ADD_SCHEME = [[14, 28, 20], [60, 160, 100], [140, 230, 170]];
const ADD_HOVER = [[20, 40, 30], [110, 220, 150], [200, 255, 220]];
const WIZ_SCHEME = [[24, 18, 34], [120, 90, 170], [200, 170, 255]];
const WIZ_HOVER = [[34, 24, 50], [160, 130, 220], [225, 205, 255]];
const DEL_SCHEME = [[34, 16, 18], [160, 70, 70], [230, 140, 140]];
const DEL_HOVER = [[50, 22, 24], [220, 110, 110], [255, 200, 200]];

// 🎨 Paint ────────────────────────────────────────────────────────────────────
function paint($) {
  const { wipe, ink, screen, text, ui } = $;
  const { width: w, height: h } = screen;
  frameCount++;

  wipe(...COLORS.bg);

  if (!prevBtn) buildButtons({ ui, screen });
  syncButtons({ screen });

  if (view === "week") paintWeekView($);
  else if (view === "day") paintDayView($);
  else paintMonthView($);

  // ── Buttons on top (shared across views).
  prevBtn.paint($, BTN_SCHEME, BTN_HOVER);
  nextBtn.paint($, BTN_SCHEME, BTN_HOVER);
  todayBtn.paint($, BTN_SCHEME, BTN_HOVER);
  addBtn.paint($, ADD_SCHEME, ADD_HOVER);
  wizardBtn.paint($, WIZ_SCHEME, WIZ_HOVER);

  // Local-mode hint near the bottom buttons.
  if (!signedIn) {
    const hint = "local";
    const hw = text.width(hint, FONT);
    const botBar = 4 + todayBtn.height + 4;
    ink(...COLORS.dim).write(
      hint, { x: Math.floor(w / 2 - hw / 2), y: h - botBar - 9 },
      false, undefined, false, FONT,
    );
  }

  // 🧙 DateWizard overlay sits on top of everything when open.
  if (wizard) {
    if (wizard.mode === "week") paintWizardWeek($);
    else paintWizardForm($);
  }
}

// 📅 MONTH view (the original grid + focused-day panel).
function paintMonthView($) {
  const { ink, screen, text, needsPaint } = $;
  const { width: w, height: h } = screen;

  const topBar = 4 + prevBtn.height + 4;
  const botBar = 4 + todayBtn.height + 4;

  // ── Title: "June 2026" centered between the < > buttons.
  const titleStr = `${MONTHS[cursor.month - 1]} ${cursor.year}`;
  const titleW = text.width(titleStr, FONT);
  ink(...COLORS.title).write(
    titleStr, { x: Math.floor(w / 2 - titleW / 2), y: 8 },
    false, undefined, false, FONT,
  );

  const panelH = 56;
  const headerH = 12;
  const gridTop = topBar + 2 + headerH;
  const gridBottom = h - botBar - panelH;
  const gridH = Math.max(28, gridBottom - gridTop);
  const cols = 7;
  const rows = weeks.length || 6;
  const cellW = w / cols;
  const cellH = gridH / rows;

  // ── Weekday header.
  for (let c = 0; c < cols; c++) {
    const cx = Math.floor(c * cellW);
    const weekend = c === 0 || c === 6;
    const lbl = WEEKDAYS[c];
    const lw = text.width(lbl, FONT);
    ink(...(weekend ? COLORS.weekend : COLORS.header)).write(
      lbl, { x: Math.floor(cx + cellW / 2 - lw / 2), y: gridTop - headerH + 2 },
      false, undefined, false, FONT,
    );
  }

  // ── Day cells.
  for (let r = 0; r < weeks.length; r++) {
    for (let c = 0; c < 7; c++) {
      const cell = weeks[r][c];
      const x = Math.floor(c * cellW);
      const y = Math.floor(gridTop + r * cellH);
      const cw = Math.floor((c + 1) * cellW) - x;
      const ch = Math.floor(gridTop + (r + 1) * cellH) - y;

      const isToday = cell.iso === today.iso;
      const isFocus = cell.iso === focusIso;
      const hasEvents = cell.events.length > 0;

      let fill = cell.inMonth ? COLORS.cellIn : COLORS.cellOut;
      if (isToday) fill = COLORS.todayFill;
      if (isFocus) fill = COLORS.focusFill;
      ink(...fill).box(x, y, cw, ch);
      ink(...COLORS.grid).box(x, y, cw, ch, "outline");

      const numColor = cell.inMonth ? COLORS.dayNum : COLORS.dayNumOut;
      ink(...(isToday ? COLORS.today : numColor)).write(
        String(cell.day), { x: x + 2, y: y + 2 },
        false, undefined, false, FONT,
      );

      if (hasEvents) {
        const dotX = x + cw - 5;
        ink(...COLORS.eventDot).box(dotX, y + 3, 2, 2);
        if (cell.events.length > 1) {
          const cnt = String(cell.events.length);
          const cw2 = text.width(cnt, FONT);
          ink(...COLORS.event).write(
            cnt, { x: dotX - cw2 - 2, y: y + 2 },
            false, undefined, false, FONT,
          );
        }
        if (ch > 18) {
          const maxChars = Math.max(1, Math.floor((cw - 4) / 4));
          const t = cell.events[0].title || "";
          const shown = t.length > maxChars ? t.slice(0, maxChars - 1) + "…" : t;
          ink(...COLORS.event).write(
            shown, { x: x + 2, y: y + 11 },
            false, undefined, false, FONT,
          );
        }
      }

      if (isFocus) ink(...COLORS.focus).box(x, y, cw, ch, "outline");
      else if (isToday) ink(...COLORS.today, 160).box(x, y, cw, ch, "outline");
    }
  }

  // ── Focused-day event panel (taps open EDIT). Stash row hitboxes for act().
  const panelTop = h - botBar - panelH;
  ink(0, 0, 0, 120).box(0, panelTop, w, panelH);
  ink(...COLORS.grid).line(0, panelTop, w, panelTop);

  const focusCell = findCell(focusIso);
  const headLabel = focusIso ? humanDay(focusIso) : "";
  ink(...COLORS.focus).write(
    headLabel, { x: 4, y: panelTop + 3 }, false, undefined, false, FONT,
  );

  monthPanelHits = []; // reset hit list each paint
  if (loading) {
    const dots = ".".repeat(Math.floor(frameCount / 12) % 4);
    ink(...COLORS.dim).write(
      `loading${dots}`, { x: 4, y: panelTop + 14 }, false, undefined, false, FONT,
    );
    needsPaint();
  } else if (error) {
    ink(...COLORS.error).write(
      error.slice(0, 40), { x: 4, y: panelTop + 14 }, false, undefined, false, FONT,
    );
  } else if (focusCell && focusCell.events.length) {
    let ey = panelTop + 14;
    const maxRows = Math.floor((panelH - 14) / 9);
    focusCell.events.slice(0, maxRows).forEach((ev) => {
      const t = eventTimeLabel(ev);
      ink(...COLORS.dim).write(t, { x: 4, y: ey }, false, undefined, false, FONT);
      const tx = 4 + text.width(t + " ", FONT) + 2;
      const maxChars = Math.max(1, Math.floor((w - tx - 4) / 4));
      const title = ev.title || "(untitled)";
      const shown = title.length > maxChars ? title.slice(0, maxChars - 1) + "…" : title;
      ink(...(ev.recurring ? COLORS.recur : COLORS.title)).write(
        shown, { x: tx, y: ey }, false, undefined, false, FONT,
      );
      monthPanelHits.push({ y: ey - 1, h: 9, ev }); // tap → edit
      ey += 9;
    });
    if (focusCell.events.length > maxRows) {
      ink(...COLORS.dim).write(
        `+${focusCell.events.length - maxRows} more`,
        { x: 4, y: ey }, false, undefined, false, FONT,
      );
    }
  } else {
    ink(...COLORS.dim).write(
      signedIn ? "no events — tap add" : "no events (local) — tap add",
      { x: 4, y: panelTop + 14 }, false, undefined, false, FONT,
    );
  }
}

// 📆 WEEK view (top-level): 7 day columns, each listing the day's events. We use
// day columns (not an hour grid) so it stays readable on a phone; today's column
// is highlighted. Tapping an event opens EDIT; tapping a column header focuses
// that day. Reuses lib/cal.mjs's weekLayout via rebuildModels().
function paintWeekView($) {
  const { ink, screen, text } = $;
  const { width: w, height: h } = screen;
  const days = weekModel?.days || [];

  const topBar = 4 + prevBtn.height + 4;
  const botBar = 4 + todayBtn.height + 4;

  // Title: the week's span, e.g. "Jun 15 – 21".
  const first = days[0];
  const last = days[6];
  const titleStr = first && last
    ? `${MONTHS_SHORT[first.month - 1]} ${first.day} – ${
        first.month === last.month ? "" : MONTHS_SHORT[last.month - 1] + " "
      }${last.day}`
    : "Week";
  const titleW = text.width(titleStr, FONT);
  ink(...COLORS.title).write(
    titleStr, { x: Math.floor(w / 2 - titleW / 2), y: 8 },
    false, undefined, false, FONT,
  );

  const gridTop = topBar + 14;
  const gridBottom = h - botBar - 2;
  const colW = w / 7;
  const colH = Math.max(40, gridBottom - gridTop);

  weekViewHits = [];
  for (let c = 0; c < 7; c++) {
    const cell = days[c];
    if (!cell) continue;
    const x = Math.floor(c * colW);
    const cw = Math.floor((c + 1) * colW) - x;
    const isToday = cell.iso === today.iso;
    const isFocus = cell.iso === focusIso;

    let fill = COLORS.cellIn;
    if (isToday) fill = COLORS.todayFill;
    if (isFocus && !isToday) fill = COLORS.focusFill;
    ink(...fill).box(x, gridTop, cw, colH);
    ink(...COLORS.grid).box(x, gridTop, cw, colH, "outline");

    // Header: weekday + day number.
    const wd = WEEKDAYS[c];
    ink(...(isToday ? COLORS.today : COLORS.header)).write(
      wd, { x: x + 2, y: gridTop + 2 }, false, undefined, false, FONT,
    );
    const dn = String(cell.day);
    const dnw = text.width(dn, FONT);
    ink(...(isToday ? COLORS.today : COLORS.dayNum)).write(
      dn, { x: x + cw - dnw - 2, y: gridTop + 2 }, false, undefined, false, FONT,
    );
    ink(...COLORS.grid).line(x + 1, gridTop + 11, x + cw - 1, gridTop + 11);
    weekViewHits.push({ x, y: gridTop, w: cw, h: 11, iso: cell.iso }); // header → focus day

    // Event list (time + truncated title), tappable for EDIT.
    let ey = gridTop + 13;
    const maxChars = Math.max(1, Math.floor((cw - 3) / 4));
    const maxRows = Math.floor((colH - 14) / 9);
    cell.events.slice(0, maxRows).forEach((ev) => {
      const tl = eventTimeLabel(ev);
      const ttl = ev.title || "(untitled)";
      const line = `${tl} ${ttl}`;
      const shown = line.length > maxChars ? line.slice(0, maxChars - 1) + "…" : line;
      ink(...(ev.recurring ? COLORS.recur : COLORS.event)).write(
        shown, { x: x + 2, y: ey }, false, undefined, false, FONT,
      );
      weekViewHits.push({ x, y: ey - 1, w: cw, h: 9, ev });
      ey += 9;
    });
    if (cell.events.length > maxRows) {
      ink(...COLORS.dim).write(
        `+${cell.events.length - maxRows}`, { x: x + 2, y: ey },
        false, undefined, false, FONT,
      );
    }
    if (isFocus) ink(...COLORS.focus).box(x, gridTop, cw, colH, "outline");
  }

  if (loading) {
    ink(...COLORS.dim).write("loading…", { x: 4, y: gridTop - 12 }, false, undefined, false, FONT);
    $.needsPaint();
  } else if (error) {
    ink(...COLORS.error).write(error.slice(0, 40), { x: 4, y: gridTop - 12 }, false, undefined, false, FONT);
  }
}

// 🕐 DAY view (top-level): a single day with hour rows; events placed by their
// start/end span. Reuses lib/cal.mjs's dayLayout via rebuildModels().
function paintDayView($) {
  const { ink, screen, text } = $;
  const { width: w, height: h } = screen;
  const cell = dayModel?.day;

  const topBar = 4 + prevBtn.height + 4;
  const botBar = 4 + todayBtn.height + 4;

  // Title: "Mon Jun 15".
  const titleStr = cell ? humanDay(cell.iso) : "Day";
  const titleW = text.width(titleStr, FONT);
  const isTodayDay = cell?.iso === today.iso;
  ink(...(isTodayDay ? COLORS.today : COLORS.title)).write(
    titleStr, { x: Math.floor(w / 2 - titleW / 2), y: 8 },
    false, undefined, false, FONT,
  );

  const gridTop = topBar + 14;
  const gridBottom = h - botBar - 2;
  const gridH = Math.max(48, gridBottom - gridTop);
  const axisW = 22; // hour-label gutter
  const startHour = 6; // show 6:00 → 23:00 (18 rows) to keep rows tall enough
  const endHour = 24;
  const hours = endHour - startHour;
  const rowH = gridH / hours;
  const colX = axisW;
  const colW = w - axisW - 2;

  // Hour rows + labels.
  for (let i = 0; i <= hours; i++) {
    const y = Math.floor(gridTop + i * rowH);
    ink(...COLORS.grid).line(colX, y, colX + colW, y);
    if (i < hours) {
      let hr = startHour + i;
      const ap = hr < 12 ? "a" : "p";
      let hh = hr % 12;
      if (hh === 0) hh = 12;
      ink(...COLORS.dim).write(
        `${hh}${ap}`, { x: 2, y: y + 1 }, false, undefined, false, FONT,
      );
    }
  }
  ink(...COLORS.grid).box(colX, gridTop, colW, Math.floor(hours * rowH), "outline");

  // "Now" line if viewing today.
  if (isTodayDay) {
    const now = new Date();
    const nowMin = now.getHours() * 60 + now.getMinutes();
    const ny = gridTop + ((nowMin / 60) - startHour) * rowH;
    if (ny >= gridTop && ny <= gridTop + hours * rowH) {
      ink(...COLORS.today, 200).line(colX, Math.floor(ny), colX + colW, Math.floor(ny));
    }
  }

  // Place events as boxes spanning their [start,end] within the visible hours.
  dayViewHits = [];
  const evs = cell?.events || [];
  evs.forEach((ev, idx) => {
    const s = ev.start ? new Date(ev.start) : null;
    if (!s) return;
    const e = ev.end ? new Date(ev.end) : s;
    let startMin = s.getHours() * 60 + s.getMinutes();
    let endMin = e.getHours() * 60 + e.getMinutes();
    if (ev.allDay) { startMin = startHour * 60; endMin = startHour * 60 + 30; }
    if (endMin <= startMin) endMin = startMin + 30;
    const y0 = gridTop + (startMin / 60 - startHour) * rowH;
    const y1 = gridTop + (endMin / 60 - startHour) * rowH;
    const top = Math.max(gridTop, Math.floor(y0));
    const bot = Math.min(gridTop + hours * rowH, Math.ceil(y1));
    if (bot <= gridTop || top >= gridTop + hours * rowH) return; // off-screen
    const bh = Math.max(8, bot - top);
    // Slight horizontal stagger so overlapping events don't fully hide.
    const inset = (idx % 3) * 4;
    const bx = colX + 2 + inset;
    const bw = colW - 4 - inset;
    ink(...(ev.recurring ? COLORS.recur : COLORS.event), 60).box(bx, top, bw, bh);
    ink(...(ev.recurring ? COLORS.recur : COLORS.event)).box(bx, top, bw, bh, "outline");
    const tl = eventTimeLabel(ev);
    const ttl = ev.title || "(untitled)";
    const label = `${tl} ${ttl}`;
    const maxChars = Math.max(1, Math.floor((bw - 4) / 4));
    const shown = label.length > maxChars ? label.slice(0, maxChars - 1) + "…" : label;
    ink(...COLORS.title).write(shown, { x: bx + 2, y: top + 2 }, false, undefined, false, FONT);
    dayViewHits.push({ x: bx, y: top, w: bw, h: bh, ev });
  });

  if (loading) {
    ink(...COLORS.dim).write("loading…", { x: 4, y: gridTop - 12 }, false, undefined, false, FONT);
    $.needsPaint();
  } else if (error) {
    ink(...COLORS.error).write(error.slice(0, 40), { x: 4, y: gridTop - 12 }, false, undefined, false, FONT);
  } else if (!evs.length) {
    ink(...COLORS.dim).write("no events — tap add", { x: colX + 4, y: gridTop + 4 }, false, undefined, false, FONT);
  }
}

// Hit lists rebuilt each paint (event taps → edit; week headers → focus).
let monthPanelHits = [];
let weekViewHits = [];
let dayViewHits = [];

// 🧙 Draw the DateWizard WEEK readout: a stacked list of the 7 days for the
// wizard's focused week. Each day row shows weekday+date and its events; a "+"
// at the row's right starts an ADD on that day, and tapping an event opens
// EDIT. Header carries ‹ / › week pagers; footer has Add / Close.
function paintWizardWeek($) {
  const { ink, screen, text, ui } = $;
  const { width: w, height: h } = screen;
  const week = wizard.week;
  const days = week?.days || [];

  // Dim everything behind the modal.
  ink(0, 0, 0, 180).box(0, 0, w, h);

  const pw = Math.min(w - 12, 260);
  const px = Math.floor(w / 2 - pw / 2);
  const headH = 16;
  const footH = 18;
  const rowH = 22;
  const ph = Math.min(h - 12, headH + rowH * 7 + footH + 8);
  const py = Math.max(6, Math.floor(h / 2 - ph / 2));

  ink(...COLORS.cellIn).box(px, py, pw, ph);
  ink(...COLORS.recur).box(px, py, pw, ph, "outline");

  // Header: "Week of Jun 15" + ‹ › pagers.
  const first = days[0];
  const headStr = first
    ? `Week of ${MONTHS_SHORT[first.month - 1]} ${first.day}`
    : "Week";
  ink(...COLORS.recur).write(
    headStr, { x: px + 4, y: py + 4 }, false, undefined, false, FONT,
  );

  if (!weekPrevBtn) weekPrevBtn = new ui.TextButton("‹", { x: 0, y: 0, screen });
  if (!weekNextBtn) weekNextBtn = new ui.TextButton("›", { x: 0, y: 0, screen });
  weekNextBtn.reposition({ right: w - (px + pw - 4), y: py + 2, screen });
  weekPrevBtn.reposition({ right: w - (px + pw - 4 - weekNextBtn.width - 4), y: py + 2, screen });
  weekPrevBtn.paint($, WIZ_SCHEME, WIZ_HOVER);
  weekNextBtn.paint($, WIZ_SCHEME, WIZ_HOVER);

  // Day rows.
  const rows = [];
  const listTop = py + headH;
  const availH = ph - headH - footH - 4;
  const useRowH = Math.min(rowH, availH / 7);
  for (let i = 0; i < 7; i++) {
    const cell = days[i];
    if (!cell) continue;
    const ry = listTop + i * useRowH;
    const isToday = cell.iso === today.iso;

    if (isToday) ink(...COLORS.todayFill).box(px + 2, ry, pw - 4, useRowH - 1);
    // Date label (left).
    const lbl = `${WEEKDAYS[i]} ${cell.day}`;
    ink(...(isToday ? COLORS.today : COLORS.header)).write(
      lbl, { x: px + 4, y: ry + 2 }, false, undefined, false, FONT,
    );

    // "+" add box at the row's right.
    const s = 11;
    const plus = { x: px + pw - 4 - s, y: ry + 2, w: s, h: s };
    ink(...COLORS.grid).box(plus.x, plus.y, plus.w, plus.h);
    ink(...COLORS.event).box(plus.x, plus.y, plus.w, plus.h, "outline");
    const pwd = text.width("+", FONT);
    ink(...COLORS.event).write(
      "+", { x: plus.x + Math.floor(plus.w / 2 - pwd / 2), y: plus.y + 2 },
      false, undefined, false, FONT,
    );

    // Events on a second line (compact); each is its own tappable hit.
    const evHits = [];
    let ex = px + 4 + text.width(lbl + "  ", FONT);
    const evBaseY = ry + 2;
    const lineMax = plus.x - 4;
    if (cell.events.length === 0) {
      ink(...COLORS.dim).write("—", { x: ex, y: evBaseY }, false, undefined, false, FONT);
    } else {
      cell.events.forEach((ev) => {
        if (ex >= lineMax) return; // ran out of room on this row
        const tl = eventTimeLabel(ev);
        const ttl = ev.title || "(untitled)";
        const chip = `${tl} ${ttl}`;
        const cwid = text.width(chip + " ", FONT);
        const shown =
          ex + cwid > lineMax
            ? clipToWidth(chip, lineMax - ex, text) + "…"
            : chip;
        ink(...(ev.recurring ? COLORS.recur : COLORS.event)).write(
          shown, { x: ex, y: evBaseY }, false, undefined, false, FONT,
        );
        const sw = text.width(shown + " ", FONT);
        evHits.push({ x: ex, y: ry, w: sw, h: useRowH, ev });
        ex += sw + 2;
      });
    }
    rows.push({ y: ry, h: useRowH, iso: cell.iso, plus, evHits });
  }

  // Footer: Add (blank form on focused day) / Close.
  const btnY = py + ph - footH;
  if (!weekAddBtn) weekAddBtn = new ui.TextButton("Add", { x: 0, y: 0, screen });
  if (!weekCloseBtn) weekCloseBtn = new ui.TextButton("Close", { x: 0, y: 0, screen });
  weekAddBtn.reposition({ x: px + 4, y: btnY, screen });
  weekCloseBtn.reposition({ right: w - (px + pw - 4), y: btnY, screen });
  weekAddBtn.paint($, ADD_SCHEME, ADD_HOVER);
  weekCloseBtn.paint($, BTN_SCHEME, BTN_HOVER);

  wizard.hitboxes = { rows, panel: { x: px, y: py, w: pw, h: ph } };
}

// Truncate a string to fit `maxW` pixels (rough, char-stepping). Helper for the
// week readout's per-event chips.
function clipToWidth(str, maxW, text) {
  let out = "";
  for (const ch of str) {
    if (text.width(out + ch, FONT) > maxW) break;
    out += ch;
  }
  return out || str[0] || "";
}

// 🧙 Draw the DateWizard ADD/EDIT form. Fields: Title, Start, End, Note,
// Visibility. Edit mode shows a Delete button + a "Save" confirm label.
function paintWizardForm($) {
  const { ink, screen, text, ui } = $;
  const { width: w, height: h } = screen;

  ink(0, 0, 0, 170).box(0, 0, w, h);

  const pw = Math.min(w - 16, 220);
  const rowH = 16;
  const headH = 16;
  const footH = 18;
  const ph = headH + rowH * 5 + footH + 8; // 5 fields now (+visibility)
  const px = Math.floor(w / 2 - pw / 2);
  const py = Math.max(8, Math.floor(h / 2 - ph / 2));

  ink(...COLORS.cellIn).box(px, py, pw, ph);
  ink(...COLORS.focus).box(px, py, pw, ph, "outline");

  // Header: "Add · Mon Jun 15" or "Edit · …".
  const verb = wizard.formMode === "edit" ? "Edit" : "Add";
  ink(...COLORS.focus).write(
    `${verb} · ${humanDay(wizard.iso)}`,
    { x: px + 4, y: py + 4 }, false, undefined, false, FONT,
  );

  const rows = [];
  const fieldY = py + headH;
  const fieldValues = [
    wizard.title || "(title)",
    timeLabel(wizard.startMin),
    timeLabel(wizard.endMin),
    wizard.note || "(optional)",
    wizard.visibility === "public" ? "public 🌍" : "private 🔒",
  ];
  const labels = ["Title", "Start", "End", "Note", "Vis"];

  for (let i = 0; i < 5; i++) {
    const ry = fieldY + i * rowH;
    const active = wizard.field === i;
    if (active) ink(...COLORS.focusFill).box(px + 2, ry, pw - 4, rowH - 1);
    ink(...(active ? COLORS.focus : COLORS.dim)).write(
      labels[i], { x: px + 4, y: ry + 4 }, false, undefined, false, FONT,
    );
    const valX = px + 4 + 34;
    const isEmpty = (i === 0 && !wizard.title) || (i === 3 && !wizard.note);
    ink(...(isEmpty ? COLORS.dim : COLORS.title)).write(
      fieldValues[i], { x: valX, y: ry + 4 }, false, undefined, false, FONT,
    );

    if (active && (i === 0 || i === 3) && Math.floor(frameCount / 30) % 2 === 0) {
      const shown = i === 0 ? wizard.title : wizard.note;
      const cx = valX + (shown ? text.width(shown, FONT) : 0) + 1;
      ink(...COLORS.focus).box(cx, ry + 3, 1, 9);
    }

    // Time steppers on Start / End rows.
    let minus = null, plus = null, toggle = null;
    if (i === 1 || i === 2) {
      const s = 11;
      plus = { x: px + pw - 4 - s, y: ry + 2, w: s, h: s };
      minus = { x: plus.x - s - 2, y: ry + 2, w: s, h: s };
      for (const [b, sym] of [[minus, "-"], [plus, "+"]]) {
        ink(...COLORS.grid).box(b.x, b.y, b.w, b.h);
        ink(...COLORS.focus).box(b.x, b.y, b.w, b.h, "outline");
        const sw = text.width(sym, FONT);
        ink(...COLORS.focus).write(
          sym, { x: b.x + Math.floor(b.w / 2 - sw / 2), y: b.y + 2 },
          false, undefined, false, FONT,
        );
      }
    }
    // The whole Visibility row is its toggle target.
    if (i === 4) toggle = { x: px + 2, y: ry, w: pw - 4, h: rowH - 1 };
    rows.push({ y: ry, h: rowH, minus, plus, toggle });
  }

  // Footer: Back · (Delete on edit) · Add/Save.
  const btnY = fieldY + rowH * 5 + 3;
  if (!cancelBtn) cancelBtn = new ui.TextButton("Back", { x: 0, y: 0, screen });
  if (!confirmBtn) confirmBtn = new ui.TextButton("Add", { x: 0, y: 0, screen });
  cancelBtn.reposition({ x: px + 4, y: btnY, screen });
  confirmBtn.reposition(
    { right: w - (px + pw - 4), y: btnY, screen },
    wizard.formMode === "edit" ? "Save" : "Add",
  );
  cancelBtn.paint($, BTN_SCHEME, BTN_HOVER);
  confirmBtn.paint($, ADD_SCHEME, ADD_HOVER);

  if (wizard.formMode === "edit") {
    if (!deleteBtn) deleteBtn = new ui.TextButton("Delete", { x: 0, y: 0, screen });
    deleteBtn.reposition({ center: "x", y: btnY, screen });
    deleteBtn.paint($, DEL_SCHEME, DEL_HOVER);
  }

  wizard.hitboxes = { rows, panel: { x: px, y: py, w: pw, h: ph } };
}

// Find the day cell matching an ISO key in the current month grid.
function findCell(iso) {
  if (!iso) return null;
  for (const week of weeks) for (const cell of week) if (cell.iso === iso) return cell;
  return null;
}

// "Mon Jun 15" style label.
function humanDay(iso) {
  const [y, m, d] = iso.split("-").map((n) => parseInt(n, 10));
  const date = new Date(y, m - 1, d);
  return `${WEEKDAYS[date.getDay()]} ${MONTHS_SHORT[m - 1]} ${d}`;
}

// 🖱️ Act ──────────────────────────────────────────────────────────────────────
function act({ event: e, screen, jump, ui }) {
  if (!prevBtn) buildButtons({ ui, screen });

  // 🧙 While the wizard is open it owns all input.
  if (wizard) {
    if (wizard.mode === "week") actWizardWeek(e);
    else actWizardForm(e);
    if (e.is("reframed")) syncButtons({ screen });
    return;
  }

  prevBtn.act(e, () => pageView(-1));
  nextBtn.act(e, () => pageView(1));
  todayBtn.act(e, () => goToday());
  addBtn.act(e, () => openAddForm(focusIso));
  wizardBtn.act(e, () => openWeekWizard());

  // Tap an event row in any view → open it for EDIT. Tap a bare day → focus.
  if (e.is("touch")) {
    if (view === "month") {
      const hit = monthPanelHits.find((b) => e.y >= b.y && e.y < b.y + b.h);
      if (hit) { openEditForm(hit.ev); return; }
      focusAt(e, screen);
    } else if (view === "week") {
      const hit = weekViewHits.find(
        (b) => e.x >= b.x && e.x < b.x + b.w && e.y >= b.y && e.y < b.y + b.h,
      );
      if (hit) {
        if (hit.ev) { openEditForm(hit.ev); return; }
        if (hit.iso) { focusIso = hit.iso; focusDate = isoToDate(hit.iso); rebuildModels(); }
      }
    } else if (view === "day") {
      const hit = dayViewHits.find(
        (b) => e.x >= b.x && e.x < b.x + b.w && e.y >= b.y && e.y < b.y + b.h,
      );
      if (hit?.ev) { openEditForm(hit.ev); return; }
    }
  }

  // Swipe pages the active unit (month/week/day).
  if (e.is("swipe:left")) pageView(1);
  if (e.is("swipe:right")) pageView(-1);

  // Keyboard paging.
  if (e.is("keyboard:down:arrowleft")) pageView(-1);
  if (e.is("keyboard:down:arrowright")) pageView(1);
  if (e.is("keyboard:down:arrowup")) pageView(-1);
  if (e.is("keyboard:down:arrowdown")) pageView(1);
  if (e.is("keyboard:down:t")) goToday();
  // View switches by key: m / w / d.
  if (e.is("keyboard:down:m")) switchView("month");
  if (e.is("keyboard:down:w")) switchView("week");
  if (e.is("keyboard:down:d")) switchView("day");

  if (e.is("reframed")) syncButtons({ screen });

  if (
    e.is("keyboard:down:escape") ||
    e.is("keyboard:down:`") ||
    e.is("keyboard:down:backspace")
  ) {
    jump("prompt");
  }
}

// Switch the active view, syncing the cursor + refetching the new range.
function switchView(next) {
  if (next === view) return;
  view = next;
  if (next === "week" || next === "day") {
    focusDate = isoToDate(focusIso) || focusDate || new Date();
    syncCursorToFocus();
  }
  $store?.persist("cal:view", view, "local");
  rebuildModels();
  fetchRange();
}

// 🧙 Route input while the wizard WEEK readout is open.
function actWizardWeek(e) {
  weekPrevBtn?.act(e, () => wizardPageWeek(-1));
  weekNextBtn?.act(e, () => wizardPageWeek(1));
  weekAddBtn?.act(e, () => openAddForm(focusIso));
  weekCloseBtn?.act(e, () => closeWizard());

  const hb = wizard.hitboxes;
  if (e.is("touch") && hb) {
    for (const r of hb.rows) {
      if (inBox(e, r.plus)) { openAddForm(r.iso); return; } // "+" → add on this day
      for (const evh of r.evHits) {
        if (e.x >= evh.x && e.x < evh.x + evh.w && e.y >= evh.y && e.y < evh.y + evh.h) {
          openEditForm(evh.ev); // tap an event chip → edit
          return;
        }
      }
    }
  }

  // ‹ / › arrows page weeks; Enter opens add; Esc closes.
  if (e.is("keyboard:down:arrowleft")) wizardPageWeek(-1);
  if (e.is("keyboard:down:arrowright")) wizardPageWeek(1);
  if (e.is("keyboard:down:enter")) { openAddForm(focusIso); return; }
  if (e.is("keyboard:down:escape") || e.is("keyboard:down:`")) { closeWizard(); return; }
}

// 🧙 Route input while the wizard ADD/EDIT form is open.
function actWizardForm(e) {
  cancelBtn?.act(e, () => formBackToWeek());
  confirmBtn?.act(e, () => confirmWizard());
  if (wizard.formMode === "edit") deleteBtn?.act(e, () => deleteFromWizard());

  const hb = wizard.hitboxes;
  if (e.is("touch") && hb) {
    const { x: px, w: pw } = hb.panel;
    for (let i = 0; i < hb.rows.length; i++) {
      const r = hb.rows[i];
      if (inBox(e, r.minus)) { wizard.field = i; wizardNudgeTime(-30); return; }
      if (inBox(e, r.plus)) { wizard.field = i; wizardNudgeTime(30); return; }
      if (inBox(e, r.toggle)) { wizard.field = i; wizardToggleVisibility(); return; }
      if (e.y >= r.y && e.y < r.y + r.h && e.x >= px && e.x < px + pw) {
        wizard.field = i;
        return;
      }
    }
  }

  if (e.is("keyboard:down:arrowup")) wizardMoveField(-1);
  if (e.is("keyboard:down:arrowdown")) wizardMoveField(1);
  if (e.is("keyboard:down:tab")) wizardMoveField(1);
  // On the Visibility row, ← / → (and Space) toggle; elsewhere nudge time.
  if (WIZ_FIELDS[wizard.field] === "visibility") {
    if (e.is("keyboard:down:arrowleft") || e.is("keyboard:down:arrowright") ||
        e.is("keyboard:down:space")) {
      wizardToggleVisibility();
      return;
    }
  } else {
    if (e.is("keyboard:down:arrowleft")) wizardNudgeTime(-30);
    if (e.is("keyboard:down:arrowright")) wizardNudgeTime(30);
  }

  if (e.is("keyboard:down:backspace")) { wizardBackspace(); return; }
  if (e.is("keyboard:down:enter")) { confirmWizard(); return; }
  if (e.is("keyboard:down:escape") || e.is("keyboard:down:`")) { formBackToWeek(); return; }

  if (
    typeof e.name === "string" &&
    e.name.startsWith("keyboard:down:") &&
    typeof e.key === "string" &&
    e.key.length === 1 &&
    !e.ctrl &&
    !e.alt
  ) {
    wizardType(e.key);
  }
}

// Point-in-box test (box may be null on rows that lack that target).
function inBox(e, b) {
  return b && e.x >= b.x && e.x < b.x + b.w && e.y >= b.y && e.y < b.y + b.h;
}

// Hit-test the MONTH grid for a touch and focus the cell under it.
function focusAt(e, screen) {
  if (!prevBtn) return;
  const { width: w, height: h } = screen;
  const topBar = 4 + prevBtn.height + 4;
  const botBar = 4 + todayBtn.height + 4;
  const panelH = 56;
  const headerH = 12;
  const gridTop = topBar + 2 + headerH;
  const gridBottom = h - botBar - panelH;
  const gridH = Math.max(28, gridBottom - gridTop);
  const rows = weeks.length || 6;
  const cellW = w / 7;
  const cellH = gridH / rows;

  if (e.y < gridTop || e.y >= gridTop + gridH) return;
  const c = Math.floor(e.x / cellW);
  const r = Math.floor((e.y - gridTop) / cellH);
  if (c < 0 || c > 6 || r < 0 || r >= weeks.length) return;
  const cell = weeks[r][c];
  if (cell) focusIso = cell.iso;
}

// 🧮 Sim — keep "now" current so today's highlight survives a midnight rollover.
function sim() {
  simClock++;
  if (simClock >= 60 * 60) {
    simClock = 0;
    const now = new Date();
    const iso = isoOf(now);
    if (iso !== today.iso) {
      today = {
        iso,
        year: now.getFullYear(),
        month: now.getMonth() + 1,
        day: now.getDate(),
      };
    }
  }
}

function meta() {
  return {
    title: "Calendar",
    desc: "View, add, edit, and delete appointments.",
  };
}

export { boot, paint, act, sim, meta };
