(() => {
  if (!document.body.classList.contains("page-id-1147")) return;

  const normalize = (value) => value
    .replace(/\u200b/g, "")
    .toLowerCase()
    .replace(/[^a-z0-9]+/g, " ")
    .trim();
  const venues = {
    "art school": "CalArts, Valencia, California",
    "dissent": "LACE, Los Angeles",
    "the experimental impulse": "REDCAT, Los Angeles",
    "hot coffee": "Artists Space, New York",
    "shimmer": "Municipal Art Gallery at Barnsdall Park, Los Angeles",
    "the british art show": "Manchester · Edinburgh · Cardiff",
    "nostalgia as resistance": "P.S.1 and The Clocktower, New York",
    "livin in the usa": "Damon Brandt Gallery, New York",
    "critical perspectives": "P.S.1, New York",
    "reallife whitecolumns": "White Columns, New York",
    "reallife magazine presents": "Nigel Greenwood Gallery, London",
    "pat douthwaite": "St Andrews Festival, St Andrews",
  };

  document.querySelectorAll(".tl-ac-grid > .elementor-container > .elementor-column").forEach((card) => {
    const headings = card.querySelectorAll(".elementor-widget-heading");
    const titleNode = headings[0]?.querySelector(".elementor-heading-title");
    if (!titleNode || titleNode.dataset.tlContextApplied) return;

    const merged = titleNode.textContent.trim();
    const yearNode = headings[1]?.querySelector(".elementor-heading-title");
    const year = yearNode?.textContent.trim() || merged.match(/(?:,\s*)(\d{4}(?:\s*[-–—]\s*(?:\d{4}|present))?)$/i)?.[1] || "";
    const title = year ? merged.replace(new RegExp(`,\\s*${year.replace(/[.*+?^${}()|[\]\\]/g, "\\$&")}\\s*$`), "") : merged;
    const venue = venues[normalize(title)];

    titleNode.textContent = title;
    titleNode.dataset.tlContextApplied = "1";
    card.classList.add("tl-context-card");

    const context = [venue, year].filter(Boolean).join(" · ");
    if (context) {
      const meta = document.createElement("span");
      meta.className = "tl-context-venue-year";
      meta.textContent = context;
      titleNode.appendChild(meta);
    }
  });
})();
