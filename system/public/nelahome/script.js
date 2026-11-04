const EVENTS = [
  "2026-06-09",
  "2026-06-23",
  "2026-07-07",
  "2026-07-21",
  "2026-08-04",
  "2026-08-18",
  "2026-09-01",
  "2026-09-15",
  "2026-09-29",
  "2026-10-13",
  "2026-10-27",
  "2026-11-10",
  "2026-11-24",
  "2026-12-08",
  "2026-12-22",
  "2027-01-05",
  "2027-01-19",
  "2027-02-02",
  "2027-02-16",
];

function formatNextMeeting(dateStr) {
  const [year, month, day] = dateStr.split("-").map(Number);
  const d = new Date(year, month - 1, day);
  const dayName = d.toLocaleDateString("en-US", { weekday: "long" });
  const monthName = d.toLocaleDateString("en-US", { month: "long" });
  return `${dayName}, ${monthName} ${day} at 7pm`;
}

function formatListDate(dateStr) {
  const [year, month, day] = dateStr.split("-").map(Number);
  const d = new Date(year, month - 1, day);
  return d.toLocaleDateString("en-US", { month: "short", day: "numeric", year: "numeric" });
}

// Uses viewer's local timezone; meetings happen Tuesdays at 7pm in Los Angeles (PT)
function render() {
  const today = new Date();
  today.setHours(0, 0, 0, 0);

  const upcoming = EVENTS.filter(e => {
    const [y, m, d] = e.split("-").map(Number);
    return new Date(y, m - 1, d) >= today;
  });

  const nextEl = document.getElementById("nextMeeting");
  const listEl = document.getElementById("meetingList");

  if (upcoming.length === 0) {
    nextEl.textContent = "No upcoming meetings scheduled";
    listEl.innerHTML = "";
    return;
  }

  nextEl.textContent = formatNextMeeting(upcoming[0]);

  const rest = upcoming.slice(1);
  listEl.innerHTML = rest.map(e => `<li>${formatListDate(e)}</li>`).join("");
}

document.addEventListener("DOMContentLoaded", render);

if (new URLSearchParams(location.search).get("thanks") === "1") {
  document.getElementById("ncc-thanks").hidden = false;
}
