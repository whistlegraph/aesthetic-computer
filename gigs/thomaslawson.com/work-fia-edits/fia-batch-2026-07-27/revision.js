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

(() => {
  const path = location.pathname.replace(/\/+$/, "");
  const bookshelfDetails = [
    "/bookshelf_artforum", "/bookshelf_afterall", "/bookshelf_eastofborneo",
    "/bookshelf-reallife", "/elementor-1796", "/bookshelf_writingsabouttl",
    "/elementor-395", "/bookshelf-anthologies",
  ];
  if (bookshelfDetails.includes(path)) document.body.classList.add("tl-bookshelf-detail");
  if (path.startsWith("/art-in-context-")) document.body.classList.add("tl-exhibition-detail");

  if (document.body.classList.contains("tl-bookshelf-detail")) {
    document.querySelectorAll("h1,h2").forEach((heading) => {
      if (heading.textContent.replace(/\s+/g, " ").trim() === "PUBLICATIONS") heading.textContent = "Publications";
    });
  }

  if (document.body.classList.contains("page-id-808")) {
    document.querySelectorAll(".tl-shelf-strip").forEach((strip) => {
      const section = strip.closest(".elementor-top-section");
      const heading = section?.querySelector(".elementor-heading-title");
      const headingLink = heading?.querySelector("a[href]");
      const href = headingLink?.href;
      const name = heading?.textContent.replace(/\s+/g, " ").trim() || "publication section";
      if (href && !heading.querySelector(".tl-shelf-more")) {
        const more = document.createElement("a");
        more.className = "tl-shelf-more";
        more.href = href;
        more.textContent = "More →";
        more.setAttribute("aria-label", `More from ${name}`);
        heading.appendChild(more);
      }
      if (!href) return;
      [...strip.children].forEach((child, index) => {
        if (child.matches("a.tl-shelf-cover")) return;
        const image = child.matches("img") ? child : child.querySelector("img");
        if (!image) return;
        image.alt = `${name} preview ${index + 1}`;
        const cover = document.createElement("a");
        cover.className = "tl-shelf-cover";
        cover.href = href;
        cover.setAttribute("aria-label", `Open ${name}`);
        strip.insertBefore(cover, child);
        cover.appendChild(child);
      });
    });
  }

  if (document.body.classList.contains("page-id-68")) {
    const cards = {
      "GOLD DOG":["Gold Dog","Artwork"], "REALLIFE COVER":["REALLIFE Cover","Magazine cover"],
      "FORMANESQUE DRAWING WALL":["Formanesque Drawing Wall","Installation"], "RED SHOE":["Red Shoe","Painting"],
      "LAST EXIT":["Last Exit","Artforum essay"], "SHOT FOR A BIKE":["Shot for a Bike","Painting"],
      "THE BRITISH ART SHOW":["The British Art Show","Exhibition"], "BROAD STUDIOS":["Broad Studios","Art-school project"],
      "LOS ANGELES PAINTING":["Los Angeles Painting","Painting"], "SUBURBAN INSTALL":["Suburban Install","Installation"],
      "PATRICK CAUFIELD ESSAY":["Patrick Caulfield Essay","Essay"], "PARTICIPANT INSTALL":["Participant Install","Installation"],
      "THE JOURNEY WEST":["The Journey West","East of Borneo essay"], "PATTERN OF THOUGHT":["Pattern of Thought","Painting"],
      "DREAMS OF THE ARROGANT PRINCE":["Dreams of the Arrogant Prince","Painting"], "STUDIO":["Studio","Studio view"],
      "MICHAEL ASHER":["Michael Asher","East of Borneo essay"], "DISCARDED PROTECTION":["Discarded Protection","Artwork"],
    };
    document.querySelectorAll("h5.elementor-heading-title").forEach((title) => {
      const key = title.textContent.replace(/[\u200B-\u200D\uFEFF]/g, "").replace(/\s+/g, " ").trim().toUpperCase();
      const card = cards[key];
      if (!card) return;
      const meta = title.closest(".elementor-column")?.querySelector("h6.elementor-heading-title");
      const year = meta?.textContent.replace(/\s+/g, " ").trim() || "";
      title.textContent = card[0];
      title.classList.add("tl-about-card-title");
      if (meta) {
        meta.textContent = card[1] + (year && year !== "-" ? ` · ${year}` : "");
        meta.classList.add("tl-about-card-meta");
      }
    });
  }
})();
