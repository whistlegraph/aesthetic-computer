#!/usr/bin/env python3
"""
Extract structured artwork data from thomaslawson.com Elementor pages
via authenticated WordPress AJAX API.

Outputs data/elementor-raw.json — consumed by scrape.mjs for enrichment.
"""

import requests, json, re, sys, os

BASE = "https://www.thomaslawson.com"
CREDS_PATH = os.path.join(
    os.path.dirname(__file__),
    "..", "aesthetic-computer-vault", "gigs", "thomaslawson.com", "credentials.json"
)

STUDIO_PAGES = [
    (347, "1977-1979"),
    (401, "1980-1982"),
    (428, "1983-1987"),
    (457, "1987-1990"),
    (476, "1991-1993"),
    (486, "1994-1998"),
    (504, "1999-2006"),
    (524, "2006-2010"),
    (1998, "2010-2015"),
    (544, "2015-2016"),
    (560, "2017-2020"),
]

BEYOND_PAGES = [
    (1552, "painted installations"),
    (1608, "early new york"),
    (1580, "dark installations"),
    (1622, "temporary murals"),
    (1646, "glasgow projects"),
    (1660, "theatre dance fashion"),
    (1689, "los angeles"),
    (1711, "the scottish project"),
]

EXHIBITION_PAGES = [
    (1205, "Pat Douthwaite", 1973),
    (1421, "REALLIFE Magazine Presents", 1981),
    (1233, "Critical Perspectives", 1982),
    (1252, "REALLIFE | Whitecolumns", 1982),
    (1280, "Livin' in the USA", 1984),
    (1292, "Nostalgia as Resistance", 1988),
    (1435, "Familie Beck", 1990),
    (1312, "The British Art Show", 1994),
    (1378, "Hot Coffee", 1997),
    (1458, "Shimmer", 1997),
    (1878, "Art School", 1997),
    (1343, "The Experimental Impulse", 2011),
    (1445, "Dissent", 2016),
]

BOOKSHELF_PAGES = [
    (873, "afterall"),
    (898, "artforum"),
    (981, "east-of-borneo"),
    (1819, "reallife"),
    (2007, "anthologies"),
    (1796, "interviews"),
    (395, "considering-other-artists"),
    (1099, "writings-about-tl"),
]


def login(session):
    creds = json.load(open(CREDS_PATH))
    wp = creds["wordpress"]
    session.post(f"{BASE}/wp-login.php", data={
        "log": wp["username"],
        "pwd": wp["password"],
        "wp-submit": "Log In",
        "redirect_to": "/wp-admin/",
        "testcookie": "1"
    }, cookies={"wordpress_test_cookie": "WP Cookie check"}, allow_redirects=True)


def get_elementor_data(session, page_id):
    """Fetch Elementor widget tree via authenticated AJAX."""
    editor = session.get(f"{BASE}/wp-admin/post.php?post={page_id}&action=elementor")
    nonces = set(re.findall(r'"nonce":"([a-f0-9]+)"', editor.text))

    for nonce in nonces:
        actions = json.dumps({"fetch_data": {"action": "get_document_config", "data": {"id": page_id}}})
        resp = session.post(f"{BASE}/wp-admin/admin-ajax.php", data={
            "action": "elementor_ajax",
            "editor_post_id": str(page_id),
            "_nonce": nonce,
            "actions": actions
        })
        d = resp.json()
        if d.get("success"):
            responses = d.get("data", {}).get("responses", {})
            for key, val in responses.items():
                if isinstance(val, dict) and val.get("success") and "elements" in val.get("data", {}):
                    return val["data"]["elements"]
    return None


def flatten_columns(elements):
    """Walk the Elementor tree and group widgets by column."""
    columns = []

    def walk(els, current_col=None):
        for el in els:
            el_type = el.get("elType", "")
            wtype = el.get("widgetType", "")

            if el_type == "column":
                col = []
                columns.append(col)
                if "elements" in el:
                    walk(el["elements"], col)
            elif wtype and current_col is not None:
                current_col.append(el)
            elif "elements" in el:
                walk(el["elements"], current_col)

    walk(elements)
    return columns


def parse_h5_caption(html_title):
    """Parse an h5 title that may contain <br>-separated lines."""
    result = {"title": None, "year": None, "medium": None, "dimensions_text": None}

    lines = re.sub(r"<br\s*/?>", "\n", html_title)
    lines = re.sub(r"<[^>]+>", "", lines)
    lines = lines.replace("&amp;", "&").replace("&#8217;", "'").replace("&nbsp;", " ")
    lines = [l.strip() for l in lines.split("\n") if l.strip()]

    if not lines:
        return result

    title_line = lines[0]
    year_match = re.search(r",?\s*\(?(\d{4})\)?\s*$", title_line)
    if year_match:
        result["year"] = int(year_match.group(1))
        result["title"] = title_line[:year_match.start()].rstrip(", ")
    else:
        result["title"] = title_line

    if len(lines) >= 3:
        result["medium"] = lines[1]
        result["dimensions_text"] = lines[2]
    elif len(lines) == 2:
        if re.search(r"\d+\s*[xX×]\s*\d+", lines[1]):
            result["dimensions_text"] = lines[1]
        else:
            result["medium"] = lines[1]

    return result


def extract_artworks_from_columns(columns):
    """Build artwork records from grouped Elementor columns."""
    artworks = []

    for col in columns:
        img_widget = None
        h5_title = None
        h5_raw = None
        h6_year = None

        for widget in col:
            wtype = widget.get("widgetType", "")
            settings = widget.get("settings", {})

            if wtype == "image":
                img_data = settings.get("image", {})
                if img_data.get("url"):
                    img_widget = img_data

            elif wtype == "heading":
                tag = settings.get("header_size", "h2")
                title = settings.get("title", "").strip()
                if not title:
                    continue
                if tag == "h5":
                    h5_raw = title
                    # Strip HTML for clean text
                    h5_title = re.sub(r"<[^>]+>", "", title).strip()
                elif tag == "h6":
                    h6_year = title.strip()

        if not img_widget or not img_widget.get("url"):
            continue

        url = img_widget["url"]
        lower = url.lower()
        if any(x in lower for x in ["logo", "footer", "header", "thomas-lawson-1.png", "beyond-the-studio"]):
            continue

        artwork = {
            "image_url": url,
            "image_id": img_widget.get("id"),
            "image_alt": img_widget.get("alt", ""),
        }

        # Parse h5 — could be multi-line with <br> or single line
        if h5_raw and "<br" in h5_raw:
            parsed = parse_h5_caption(h5_raw)
            artwork["title"] = parsed["title"]
            artwork["year"] = parsed.get("year")
            artwork["medium"] = parsed.get("medium")
            artwork["dimensions_text"] = parsed.get("dimensions_text")
        else:
            artwork["title"] = h5_title
            # Year from h6
            if h6_year and h6_year != "-":
                ym = re.search(r"\b(19[5-9]\d|20[0-3]\d)\b", h6_year)
                if ym:
                    artwork["year"] = int(ym.group(1))

        artworks.append(artwork)

    return artworks


def extract_writings_from_elements(elements, category):
    """Extract writing entries from bookshelf page Elementor tree.

    Bookshelf pages use repeating groups:
      [image] cover scan
      [h5/h4] article title (e.g. "Last Exit- Painting")
      [h6/h5] publication + date (e.g. "Artforum, October 1981")

    We flatten all widgets in order and pair consecutive headings.
    """
    writings = []
    seen = set()

    # Flatten all widgets in document order
    widgets = []
    def flatten(els):
        for el in els:
            wtype = el.get("widgetType", "")
            if wtype:
                widgets.append(el)
            if "elements" in el:
                flatten(el["elements"])
    flatten(elements)

    # Walk through widgets and group (image?, title heading, pub/date heading?)
    i = 0
    while i < len(widgets):
        w = widgets[i]
        wtype = w.get("widgetType", "")
        settings = w.get("settings", {})

        if wtype == "heading":
            tag = settings.get("header_size", "h2")
            raw_title = settings.get("title", "").strip()
            title_clean = re.sub(r"<[^>]+>", "", raw_title).strip()

            if not title_clean or len(title_clean) < 3:
                i += 1
                continue

            # Skip section headers
            if title_clean.lower() in ("bookshelf", "menu", "home", "publications"):
                i += 1
                continue

            # Skip pure year headers like "1977" or "1982"
            if re.match(r"^\d{4}$", title_clean):
                i += 1
                continue

            # Check if this looks like a publication/date line (e.g. "Artforum, October 1981")
            # Must contain a comma + month/season or year to be a pub line
            pub_names = ["artforum", "arforum", "afterall", "east of borneo"]
            lower_tc = title_clean.lower()
            is_pub_line = (
                any(lower_tc.startswith(p) for p in pub_names) and
                ("," in title_clean) and
                bool(re.search(r"\b(19[5-9]\d|20[0-3]\d)\b", title_clean))
            )

            if is_pub_line:
                # This is a publication line without a preceding title — skip
                i += 1
                continue

            # This is a title heading — look ahead for a publication/date heading
            pub_info = None
            year = None
            ext_url = None

            # Extract URL from anchor in the heading HTML
            url_match = re.search(r'href="([^"]+)"', raw_title)
            if url_match:
                ext_url = url_match.group(1)
                if not ext_url.startswith("http"):
                    ext_url = None

            # Look at next widget for pub/date info
            if i + 1 < len(widgets):
                next_w = widgets[i + 1]
                if next_w.get("widgetType") == "heading":
                    next_title = re.sub(r"<[^>]+>", "", next_w.get("settings", {}).get("title", "")).strip()
                    next_lower = next_title.lower()
                    next_is_pub = (
                        any(next_lower.startswith(p) for p in pub_names) and
                        "," in next_title and
                        bool(re.search(r"\b(19[5-9]\d|20[0-3]\d)\b", next_title))
                    )
                    # Also check if it's a date-like line (month + year)
                    has_month = bool(re.search(r"(?:January|February|March|April|May|June|July|August|September|October|November|December|Summer|Spring|Fall|Winter)", next_title, re.I))
                    has_year = bool(re.search(r"\b(19[5-9]\d|20[0-3]\d)\b", next_title))

                    if next_is_pub or (has_month and has_year):
                        pub_info = next_title
                        ym = re.search(r"\b(19[5-9]\d|20[0-3]\d)\b", next_title)
                        if ym:
                            year = int(ym.group(1))
                        i += 1  # consume the pub heading

            # Fall back to year in title
            if not year:
                ym = re.search(r"\b(19[5-9]\d|20[0-3]\d)\b", title_clean)
                if ym:
                    year = int(ym.group(1))

            # Clean title — remove year suffix
            clean_title = re.sub(r"\s*\(?\d{4}\)?\s*$", "", title_clean).strip()

            if clean_title.lower() in seen:
                i += 1
                continue
            seen.add(clean_title.lower())

            writings.append({
                "title": clean_title,
                "year": year,
                "url": ext_url,
                "pub_info": pub_info,
                "category": category,
            })

        elif wtype == "text-editor":
            # Some pages use rich text with links
            editor_text = settings.get("editor", "")
            for match in re.finditer(r'<a[^>]*href="([^"]*)"[^>]*>(.*?)</a>', editor_text, re.DOTALL):
                href = match.group(1)
                text = re.sub(r"<[^>]+>", "", match.group(2)).strip()
                if not text or len(text) < 3:
                    continue
                if text.lower() in seen:
                    continue
                seen.add(text.lower())

                year = None
                ym = re.search(r"\b(19[5-9]\d|20[0-3]\d)\b", text)
                if ym:
                    year = int(ym.group(1))

                writings.append({
                    "title": re.sub(r"\s*\(?\d{4}\)?\s*$", "", text).strip(),
                    "year": year,
                    "url": href if href.startswith("http") else None,
                    "category": category,
                })

        i += 1

    return writings


def main():
    print("Elementor Data Extractor for thomaslawson.com")
    print("=" * 50)

    s = requests.Session()
    login(s)
    print("Logged in to WordPress\n")

    result = {
        "studio_artworks": [],
        "beyond_artworks": [],
        "exhibition_images": {},
        "writings": [],
    }

    # Studio pages
    print("In the Studio pages:")
    for page_id, period in STUDIO_PAGES:
        print(f"  {period} (page {page_id})...", end=" ", flush=True)
        elements = get_elementor_data(s, page_id)
        if not elements:
            print("FAILED")
            continue
        columns = flatten_columns(elements)
        artworks = extract_artworks_from_columns(columns)
        for a in artworks:
            a["period"] = period
            a["page_id"] = page_id
            a["section"] = "in-the-studio"
        result["studio_artworks"].extend(artworks)

        titled = sum(1 for a in artworks if a.get("title"))
        with_year = sum(1 for a in artworks if a.get("year"))
        with_med = sum(1 for a in artworks if a.get("medium"))
        print(f"{len(artworks)} works ({titled} titled, {with_year} year, {with_med} medium)")

    # Beyond pages
    print("\nBeyond the Studio pages:")
    for page_id, tag in BEYOND_PAGES:
        print(f"  {tag} (page {page_id})...", end=" ", flush=True)
        elements = get_elementor_data(s, page_id)
        if not elements:
            print("FAILED")
            continue
        columns = flatten_columns(elements)
        artworks = extract_artworks_from_columns(columns)
        for a in artworks:
            a["tag"] = tag
            a["page_id"] = page_id
            a["section"] = "beyond-the-studio"
        result["beyond_artworks"].extend(artworks)
        print(f"{len(artworks)} works")

    # Exhibition pages
    print("\nExhibition pages:")
    for page_id, title, year in EXHIBITION_PAGES:
        print(f"  {title} ({year}, page {page_id})...", end=" ", flush=True)
        elements = get_elementor_data(s, page_id)
        if not elements:
            print("FAILED")
            continue
        columns = flatten_columns(elements)
        images = []
        for col in columns:
            for widget in col:
                if widget.get("widgetType") == "image":
                    img = widget.get("settings", {}).get("image", {})
                    if img.get("url"):
                        images.append({
                            "url": img["url"],
                            "id": img.get("id"),
                            "alt": img.get("alt", ""),
                        })
        result["exhibition_images"][str(page_id)] = images
        print(f"{len(images)} images")

    # Bookshelf pages
    print("\nBookshelf pages:")
    for page_id, category in BOOKSHELF_PAGES:
        print(f"  {category} (page {page_id})...", end=" ", flush=True)
        elements = get_elementor_data(s, page_id)
        if not elements:
            print("FAILED")
            continue
        writings = extract_writings_from_elements(elements, category)
        with_year = sum(1 for w in writings if w.get("year"))
        with_pub = sum(1 for w in writings if w.get("pub_info"))
        result["writings"].extend(writings)
        print(f"{len(writings)} entries ({with_year} with year, {with_pub} with pub info)")

    # Save
    out_path = os.path.join(os.path.dirname(__file__), "data", "elementor-raw.json")
    with open(out_path, "w") as f:
        json.dump(result, f, indent=2)

    total_artworks = len(result["studio_artworks"]) + len(result["beyond_artworks"])
    print(f"\nDone!")
    print(f"  Studio artworks:  {len(result['studio_artworks'])}")
    print(f"  Beyond artworks:  {len(result['beyond_artworks'])}")
    print(f"  Exhibition pages: {len(result['exhibition_images'])}")
    print(f"  Writings:         {len(result['writings'])}")
    print(f"  Saved to:         {out_path}")


if __name__ == "__main__":
    main()
