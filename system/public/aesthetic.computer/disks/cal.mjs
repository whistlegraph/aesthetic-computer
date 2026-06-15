// cal — calendar, 2026.06.15
// Editing/scheduling lives in the native DateWizard app; this piece is the web
// calendar view. It ships MONTH, WEEK, and DAY browsing surfaces. All date-math
// (grid + week/day layout + recurrence expansion + event bucketing) lives in the
// shared engine lib/cal.mjs so every view reuses it. Events come from /api/cal
// when signed in, or from a device-local store when not.

import {
  WEEKDAYS,
  MONTHS,
  MONTHS_SHORT,
  buildMonthGrid,
  layoutEvents,
  monthRange,
  addMonths,
  addWeeks,
  dayLayout,
  weekLayout,
  expandRecurrences,
  isoOf,
  isoDate,
  eventTimeLabel,
} from "../lib/cal.mjs";

/* #region 📚 README
  Colon params → view + date:
    cal              → month view, today
    cal:2026:6       → month view, June 2026  (year:month, month 1-indexed)
    cal:week         → top-level WEEK view (focused week, hour-less day columns)
    cal:day          → top-level DAY view (single day, hour rows)

  Controls (read-only viewer):
    Tap a day        — focus it (month: its events list at the bottom)
    ← / →            — page (month pages months; week pages weeks; day pages days)
    m / w / d        — switch view; t — today
    Esc / `          — back to prompt
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

// Captured AC handles (so async fetch can reach them).
let $net = null;
let $store = null;

// UI buttons (built in boot, painted in paint, handled in act).
let prevBtn = null;
let nextBtn = null;
let todayBtn = null;

// 🗝️ Local-calendar storage key (device-local fallback when signed out).
const LOCAL_KEY = "cal:events:local";

// ──────────────────────────────────────────────────────────────────────────

function boot({ colon, store, net, handle, user, ui, screen }) {
  $net = net;
  $store = store;

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
  const first = colon?.[0];
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
  if (!first) {
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
}

function clampMonth(m) {
  if (isNaN(m) || m < 1) return 1;
  if (m > 12) return 12;
  return m;
}

// 🧩 Expand recurrences over the visible range, then rebuild whichever view
// model is active. Called after every fetch / paging so all views re-layout
// from one place.
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
}

function syncButtons({ screen }) {
  if (!prevBtn) return;
  prevBtn.reposition({ left: 4, top: 4, screen });
  nextBtn.reposition({ right: 4, top: 4, screen });
  todayBtn.reposition({ left: 4, bottom: 4, screen });
}

const BTN_SCHEME = [[20, 24, 34], [80, 90, 120], [180, 190, 220]];
const BTN_HOVER = [[30, 35, 50], [140, 160, 210], [220, 230, 255]];

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
}

// 📅 MONTH view (the grid + focused-day panel).
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

  // ── Focused-day event panel (read-only list).
  const panelTop = h - botBar - panelH;
  ink(0, 0, 0, 120).box(0, panelTop, w, panelH);
  ink(...COLORS.grid).line(0, panelTop, w, panelTop);

  const focusCell = findCell(focusIso);
  const headLabel = focusIso ? humanDay(focusIso) : "";
  ink(...COLORS.focus).write(
    headLabel, { x: 4, y: panelTop + 3 }, false, undefined, false, FONT,
  );

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
      signedIn ? "no events" : "no events (local)",
      { x: 4, y: panelTop + 14 }, false, undefined, false, FONT,
    );
  }
}

// 📆 WEEK view (top-level): 7 day columns, each listing the day's events. We use
// day columns (not an hour grid) so it stays readable on a phone; today's column
// is highlighted. Tapping a column header focuses that day. Reuses lib/cal.mjs's
// weekLayout via rebuildModels().
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

    // Event list (time + truncated title).
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
  });

  if (loading) {
    ink(...COLORS.dim).write("loading…", { x: 4, y: gridTop - 12 }, false, undefined, false, FONT);
    $.needsPaint();
  } else if (error) {
    ink(...COLORS.error).write(error.slice(0, 40), { x: 4, y: gridTop - 12 }, false, undefined, false, FONT);
  } else if (!evs.length) {
    ink(...COLORS.dim).write("no events", { x: colX + 4, y: gridTop + 4 }, false, undefined, false, FONT);
  }
}

// Hit list rebuilt each paint (week headers → focus day).
let weekViewHits = [];

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

  prevBtn.act(e, () => pageView(-1));
  nextBtn.act(e, () => pageView(1));
  todayBtn.act(e, () => goToday());

  // Tap a day → focus it (month grid hit-test; week column headers).
  if (e.is("touch")) {
    if (view === "month") {
      focusAt(e, screen);
    } else if (view === "week") {
      const hit = weekViewHits.find(
        (b) => e.x >= b.x && e.x < b.x + b.w && e.y >= b.y && e.y < b.y + b.h,
      );
      if (hit?.iso) { focusIso = hit.iso; focusDate = isoToDate(hit.iso); rebuildModels(); }
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
    desc: "View your appointments. Month, week, and day views.",
  };
}

export { boot, paint, act, sim, meta };
