// cal — pure date-math engine for the calendar piece(s), 2026.06.15
//
// No rendering, no AC API, no dependencies — just the grid + bucketing math
// the calendar views need. The `cal` piece (disks/cal.mjs) renders MONTH view
// on top of this; future week / day / year views reuse the same helpers so the
// date arithmetic lives in exactly one place.
//
// Conventions throughout:
//   - `month` is 1-indexed (1 = January … 12 = December) at the public API edge,
//     matching how humans (and the `cal:2026:6` colon param) talk about months.
//     Internally we convert to JS's 0-indexed Date months at the boundary.
//   - "weekday" is 0 = Sunday … 6 = Saturday (JS Date.getDay() convention).
//   - Day cells are plain { year, month, day } objects (month 1-indexed) so a
//     cell is comparable + serializable without carrying a live Date around.
//   - Events arrive with ISO `start` / `end` strings (see the backend contract
//     in disks/cal.mjs) and get bucketed onto the day cells they touch.

/* #region 🏁 TODO
  - [x] weekGrid()/weekLayout() for the dedicated week view + wizard week readout.
  - [x] dayLayout() helper that buckets a single day's events for the day view.
  - [x] expandRecurrences() — expand FREQ=DAILY|WEEKLY|MONTHLY rrules over a range.
#endregion */

export const WEEKDAYS = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"];
export const WEEKDAYS_FULL = [
  "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday",
];
export const MONTHS = [
  "January", "February", "March", "April", "May", "June",
  "July", "August", "September", "October", "November", "December",
];
export const MONTHS_SHORT = [
  "Jan", "Feb", "Mar", "Apr", "May", "Jun",
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
];

// 📅 Basic month math ─────────────────────────────────────────────────────────

// Days in a given month (month 1-indexed). Leap years handled by Date rollover:
// day 0 of the *next* month is the last day of *this* month.
export function daysInMonth(year, month) {
  return new Date(year, month, 0).getDate();
}

// Weekday (0=Sun … 6=Sat) of the 1st of the month.
export function firstWeekdayOf(year, month) {
  return new Date(year, month - 1, 1).getDay();
}

// True if `year` is a leap year.
export function isLeapYear(year) {
  return (year % 4 === 0 && year % 100 !== 0) || year % 400 === 0;
}

// Step a {year, month} forward/back by `delta` months, wrapping the year.
// month stays 1-indexed. e.g. addMonths(2026, 12, 1) → {year:2027, month:1}.
export function addMonths(year, month, delta) {
  // Convert to a 0-indexed absolute month count, shift, convert back.
  const total = (year * 12 + (month - 1)) + delta;
  return { year: Math.floor(total / 12), month: (total % 12) + 1 };
}

// 🧱 Day-cell construction ────────────────────────────────────────────────────

// A day cell. `inMonth` marks whether it belongs to the month being displayed
// (vs. a leading/trailing spillover day from an adjacent month). `iso` is the
// YYYY-MM-DD key used to bucket events and to compare against "today".
export function dayCell(year, month, day, inMonth) {
  return {
    year,
    month, // 1-indexed
    day,
    inMonth,
    iso: isoDate(year, month, day),
    events: [], // filled by layoutEvents()
  };
}

// YYYY-MM-DD for a (year, 1-indexed month, day). Local-calendar, zero-padded.
export function isoDate(year, month, day) {
  const mm = String(month).padStart(2, "0");
  const dd = String(day).padStart(2, "0");
  return `${year}-${mm}-${dd}`;
}

// The YYYY-MM-DD of a Date in *local* time (Date.toISOString() is UTC and would
// shift the day across timezone boundaries, which we never want for a calendar).
export function isoOf(date) {
  return isoDate(date.getFullYear(), date.getMonth() + 1, date.getDate());
}

// 🗓️ Month grid ───────────────────────────────────────────────────────────────

// Build a month as an array of weeks, each week an array of 7 day cells. The
// grid always starts on Sunday and is padded with the trailing days of the
// previous month and leading days of the next so every row is full — the shape
// a month-view renderer wants. Weeks vary 4–6 depending on the month's layout.
export function buildMonthGrid(year, month) {
  const firstDay = firstWeekdayOf(year, month); // 0..6, Sun-based
  const totalDays = daysInMonth(year, month);
  const prev = addMonths(year, month, -1);
  const prevDays = daysInMonth(prev.year, prev.month);
  const next = addMonths(year, month, 1);

  const cells = [];

  // Leading spillover: the last `firstDay` days of the previous month.
  for (let i = firstDay - 1; i >= 0; i -= 1) {
    cells.push(dayCell(prev.year, prev.month, prevDays - i, false));
  }
  // The actual month.
  for (let d = 1; d <= totalDays; d += 1) {
    cells.push(dayCell(year, month, d, true));
  }
  // Trailing spillover: fill out the final week to a multiple of 7.
  let nextDay = 1;
  while (cells.length % 7 !== 0) {
    cells.push(dayCell(next.year, next.month, nextDay, false));
    nextDay += 1;
  }

  // Chunk the flat run into weeks of 7.
  const weeks = [];
  for (let i = 0; i < cells.length; i += 7) weeks.push(cells.slice(i, i + 7));
  return weeks;
}

// All the day cells of the week that contains `date` (Sunday-based), as a flat
// array of 7 cells. Used by the (future) week view; also handy for "this week"
// highlighting in the month view.
export function weekOf(date) {
  const start = new Date(date.getFullYear(), date.getMonth(), date.getDate());
  start.setDate(start.getDate() - start.getDay()); // back up to Sunday
  const cells = [];
  for (let i = 0; i < 7; i += 1) {
    const d = new Date(start.getFullYear(), start.getMonth(), start.getDate() + i);
    cells.push(dayCell(d.getFullYear(), d.getMonth() + 1, d.getDate(), true));
  }
  return cells;
}

// Start-of-week Date (local midnight, Sunday-based) for the day `date` lands in.
export function weekStart(date) {
  const start = new Date(date.getFullYear(), date.getMonth(), date.getDate());
  start.setDate(start.getDate() - start.getDay()); // back up to Sunday
  return start;
}

// Step a week-start Date forward/back by `delta` weeks (returns a new Date).
export function addWeeks(date, delta) {
  const d = weekStart(date);
  d.setDate(d.getDate() + delta * 7);
  return d;
}

// 📐 weekLayout(date, events) — the shared week-view model used by BOTH the
// top-level `cal:week` piece AND the DateWizard's week readout, so a single
// helper owns the week's shape. Returns the 7 day cells (Sunday→Saturday) with
// their events bucketed + sorted, plus the inclusive ISO range the week covers
// (handy for fetching). Events should be pre-expanded (see expandRecurrences).
export function weekLayout(date, events) {
  const cells = weekOf(date); // 7 plain day cells, all inMonth:true
  layoutEvents(cells, events); // bucket + sort into each cell's events[]
  const first = cells[0];
  const last = cells[6];
  return {
    days: cells,
    from: new Date(first.year, first.month - 1, first.day, 0, 0, 0).toISOString(),
    to: new Date(last.year, last.month - 1, last.day, 23, 59, 59).toISOString(),
  };
}

// 📐 dayLayout(date, events) — single-day model for `cal:day`. Returns one cell
// (with events bucketed + sorted) and the day's inclusive ISO range. Mirrors
// weekLayout so the day view shares the same bucketing path.
export function dayLayout(date, events) {
  const cell = dayCell(date.getFullYear(), date.getMonth() + 1, date.getDate(), true);
  layoutEvents([cell], events);
  return {
    day: cell,
    from: new Date(cell.year, cell.month - 1, cell.day, 0, 0, 0).toISOString(),
    to: new Date(cell.year, cell.month - 1, cell.day, 23, 59, 59).toISOString(),
  };
}

// ISO week number (1..53) of a date, ISO-8601 (weeks start Monday, week 1 holds
// the year's first Thursday). Useful as a row label in week/day views.
export function isoWeekNumber(date) {
  const d = new Date(Date.UTC(date.getFullYear(), date.getMonth(), date.getDate()));
  const dayNum = (d.getUTCDay() + 6) % 7; // Mon=0 … Sun=6
  d.setUTCDate(d.getUTCDate() - dayNum + 3); // nearest Thursday
  const firstThursday = new Date(Date.UTC(d.getUTCFullYear(), 0, 4));
  const firstDayNum = (firstThursday.getUTCDay() + 6) % 7;
  firstThursday.setUTCDate(firstThursday.getUTCDate() - firstDayNum + 3);
  return 1 + Math.round((d - firstThursday) / (7 * 24 * 3600 * 1000));
}

// 📌 Event layout ─────────────────────────────────────────────────────────────

// Span of YYYY-MM-DD keys an event covers, inclusive of both ends. An all-day
// or single-instant event covers one day; a multi-day event covers each day it
// touches. End is treated as inclusive of its calendar day (an event ending at
// midnight of the next day still belongs to the day it visibly occupies — but
// we keep it simple in v1: bucket by the calendar days start..end land on).
export function eventDayKeys(event) {
  const start = parseEventDate(event.start);
  if (!start) return [];
  const endRaw = event.end ? parseEventDate(event.end) : null;
  const end = endRaw && endRaw >= start ? endRaw : start;

  const keys = [];
  const cursor = new Date(start.getFullYear(), start.getMonth(), start.getDate());
  const last = new Date(end.getFullYear(), end.getMonth(), end.getDate());
  // Guard against pathological ranges (corrupt data) so we never loop forever.
  let guard = 0;
  while (cursor <= last && guard < 400) {
    keys.push(isoOf(cursor));
    cursor.setDate(cursor.getDate() + 1);
    guard += 1;
  }
  return keys;
}

// Lenient ISO/date parse → Date (or null). Accepts full ISO timestamps and bare
// YYYY-MM-DD (which `new Date` would otherwise parse as UTC midnight — we parse
// it as *local* midnight so day bucketing stays in the user's calendar).
export function parseEventDate(value) {
  if (!value) return null;
  if (value instanceof Date) return isNaN(value) ? null : value;
  const dateOnly = /^(\d{4})-(\d{2})-(\d{2})$/.exec(value);
  if (dateOnly) {
    return new Date(+dateOnly[1], +dateOnly[2] - 1, +dateOnly[3]);
  }
  const d = new Date(value);
  return isNaN(d) ? null : d;
}

// Bucket a list of events onto the day cells of a month grid (mutates each
// cell's `events` array and returns the same grid for chaining). Events are
// sorted within a day by start time so the renderer can list them top-down.
// `grid` is the weeks array from buildMonthGrid(); pass any cells with an `iso`
// + `events` field and it works for week/day grids too.
export function layoutEvents(grid, events) {
  // Map each day key → its cell(s). A flat grid (week) and a nested grid
  // (month weeks) both flatten cleanly here.
  const cells = grid.flat ? flattenCells(grid) : grid;
  const byKey = new Map();
  for (const cell of cells) {
    cell.events = []; // reset so re-layout is idempotent
    byKey.set(cell.iso, cell);
  }
  for (const event of events || []) {
    for (const key of eventDayKeys(event)) {
      const cell = byKey.get(key);
      if (cell) cell.events.push(event);
    }
  }
  for (const cell of cells) cell.events.sort(compareByStart);
  return grid;
}

// Flatten either a weeks-of-cells month grid or a flat cell array to one list.
function flattenCells(grid) {
  if (!Array.isArray(grid)) return [];
  // weeks-of-cells → each element is itself an array of cells.
  if (grid.length && Array.isArray(grid[0])) return grid.flat();
  return grid;
}

// Sort comparator: earlier start first; all-day events float to the top.
export function compareByStart(a, b) {
  if (a.allDay && !b.allDay) return -1;
  if (!a.allDay && b.allDay) return 1;
  const da = parseEventDate(a.start);
  const db = parseEventDate(b.start);
  if (!da) return 1;
  if (!db) return -1;
  return da - db;
}

// 🔁 Recurrence expansion ─────────────────────────────────────────────────────
//
// Expand events carrying an `rrule` into concrete dated instances that fall
// within [rangeStartISO, rangeEndISO]. Non-recurring events (no `rrule`) pass
// through untouched. The result is a flat event list safe to hand to
// layoutEvents() — every entry has real `start`/`end` ISO strings.
//
// Supported RFC 5545 subset (dependency-free, deliberately practical):
//   FREQ=DAILY | WEEKLY | MONTHLY   (required)
//   INTERVAL=<n>                    (optional, default 1)
//   COUNT=<n>                       (optional — stop after n instances)
//   UNTIL=<ISO-ish>                 (optional — stop after this instant)
// Anything fancier (BYDAY, BYMONTHDAY, FREQ=YEARLY, etc.) is ignored: the
// series simply steps by its FREQ from the master start. Each instance keeps
// the event's duration (end − start) and is tagged { recurring, masterUid }
// so the renderer can show it and edits can find the master document.
//
// Hard-capped at MAX_INSTANCES per series so a malformed rule (or an enormous
// range) can never spin forever.
const MAX_INSTANCES = 366;

export function expandRecurrences(events, rangeStartISO, rangeEndISO) {
  const rangeStart = parseEventDate(rangeStartISO);
  const rangeEnd = parseEventDate(rangeEndISO);
  const out = [];

  for (const ev of events || []) {
    if (!ev?.rrule) {
      out.push(ev); // plain event → straight through.
      continue;
    }
    const rule = parseRRule(ev.rrule);
    const start = parseEventDate(ev.start);
    if (!rule || !start || !rangeStart || !rangeEnd) {
      out.push(ev); // unparseable → fall back to the master as-is.
      continue;
    }

    const end = ev.end ? parseEventDate(ev.end) : null;
    const durMs = end && end > start ? end - start : 0;
    const interval = rule.interval > 0 ? rule.interval : 1;
    const until = rule.until || null;

    let occ = new Date(start.getTime());
    let made = 0;
    for (let i = 0; i < MAX_INSTANCES; i += 1) {
      if (rule.count && made >= rule.count) break;
      if (until && occ > until) break;
      if (occ > rangeEnd) break; // stepping only forward → done once past range.

      const occEnd = new Date(occ.getTime() + durMs);
      // In-range if the instance overlaps [rangeStart, rangeEnd].
      if (occEnd >= rangeStart && occ <= rangeEnd) {
        out.push({
          ...ev,
          start: occ.toISOString(),
          end: durMs ? occEnd.toISOString() : ev.end,
          recurring: true,
          masterUid: ev.uid, // the document to PUT/DELETE when editing this series.
        });
      }
      made += 1;
      occ = stepDate(occ, rule.freq, interval);
    }
  }
  return out;
}

// Parse "FREQ=WEEKLY;INTERVAL=2;COUNT=10" → { freq, interval, count, until }.
function parseRRule(rrule) {
  const parts = {};
  for (const piece of String(rrule).split(";")) {
    const [k, v] = piece.split("=");
    if (k && v !== undefined) parts[k.trim().toUpperCase()] = v.trim();
  }
  const freq = (parts.FREQ || "").toUpperCase();
  if (freq !== "DAILY" && freq !== "WEEKLY" && freq !== "MONTHLY") return null;
  return {
    freq,
    interval: parts.INTERVAL ? parseInt(parts.INTERVAL, 10) : 1,
    count: parts.COUNT ? parseInt(parts.COUNT, 10) : null,
    until: parts.UNTIL ? parseUntil(parts.UNTIL) : null,
  };
}

// RFC 5545 UNTIL is usually a compact stamp (20261231T235959Z); also accept a
// plain ISO string or bare date. Returns a Date or null.
function parseUntil(value) {
  const compact = /^(\d{4})(\d{2})(\d{2})(?:T(\d{2})(\d{2})(\d{2})Z?)?$/.exec(value);
  if (compact) {
    const [, y, mo, d, h = "0", mi = "0", s = "0"] = compact;
    return new Date(+y, +mo - 1, +d, +h, +mi, +s);
  }
  return parseEventDate(value);
}

// Advance a Date by one FREQ step × interval (new Date; mutates nothing shared).
function stepDate(date, freq, interval) {
  const d = new Date(date.getTime());
  if (freq === "DAILY") d.setDate(d.getDate() + interval);
  else if (freq === "WEEKLY") d.setDate(d.getDate() + 7 * interval);
  else if (freq === "MONTHLY") d.setMonth(d.getMonth() + interval);
  return d;
}

// ⏱️ Range helpers ────────────────────────────────────────────────────────────

// [from, to] ISO timestamps spanning the full visible month grid (including the
// spillover days), so a single GET /api/cal?from&to fetches everything painted.
export function monthRange(year, month) {
  const weeks = buildMonthGrid(year, month);
  const cells = weeks.flat();
  const first = cells[0];
  const last = cells[cells.length - 1];
  return {
    from: new Date(first.year, first.month - 1, first.day, 0, 0, 0).toISOString(),
    to: new Date(last.year, last.month - 1, last.day, 23, 59, 59).toISOString(),
  };
}

// Short, human time label for an event ("9:30a", "All day", "→" for multi-day
// continuations). Pure formatting; safe to call from a renderer.
export function eventTimeLabel(event) {
  if (event.allDay) return "all day";
  const d = parseEventDate(event.start);
  if (!d) return "";
  let h = d.getHours();
  const m = d.getMinutes();
  const ampm = h >= 12 ? "p" : "a";
  h = h % 12;
  if (h === 0) h = 12;
  return m === 0 ? `${h}${ampm}` : `${h}:${String(m).padStart(2, "0")}${ampm}`;
}
