# scrape-shelves.py — every Bookshelf detail item (image widget + the headings after it, up to the
# next image), matched to Tom's CV only by an exact link match (TL-site PDF or article URL).
import csv, json, re, urllib.request, html as H
SLUGS = ["bookshelf_afterall","bookshelf_artforum","bookshelf_eastofborneo","bookshelf_writingsabouttl",
         "bookshelf-anthologies","bookshelf-reallife","elementor-1796","elementor-395"]
def get(u): return urllib.request.urlopen(urllib.request.Request(u, headers={"User-Agent":"Mozilla/5.0"}), timeout=60).read().decode()
def norm(u): return re.sub(r"[?#].*$","",H.unescape(u or "").strip()).rstrip("/").replace("http://","https://").replace("://www.","://").lower()
def txt(s): return re.sub(r"\s+"," ",H.unescape(re.sub(r"<[^>]+>","",s))).strip()
cv = list(csv.DictReader(open("fia-2026-09-27/tl-cv.csv")))
byurl = {}
for r in cv:
    for k in ("Page on TL site (sometimes PDF)","Website(s)"):
        for u in re.split(r"\s+", r[k] or ""):
            if u.startswith("http"): byurl.setdefault(norm(u), []).append(r)
out = {}
for slug in SLUGS:
    h = get(f"https://www.thomaslawson.com/{slug}/")
    body = h[h.find('<div data-elementor-type="wp-page"'):h.find('<footer')]
    widgets = re.split(r'(?=<div class="elementor-element elementor-element-\w+[^"]*elementor-widget-image\b)', body)[1:]
    items = []
    for w in widgets:
        img = re.search(r'<img[^>]+src="([^"]+)"', w)
        if not img: continue
        links = []
        for a in re.findall(r'<a[^>]+href="([^"]+)"', w):
            if a not in links and "thomaslawson.com/bookshelf" not in a: links.append(H.unescape(a))
        heads = [x for x in (txt(x) for x in re.findall(r'<h[1-6][^>]*elementor-heading-title[^>]*>(.*?)</h[1-6]>', w, re.S)) if x]
        rows = []
        for l in links:
            for r in byurl.get(norm(l), []):
                if r not in rows: rows.append(r)
        items.append({"img": img.group(1), "links": links, "heads": heads,
                      "cv": rows[0] if len(rows) == 1 else None, "cvAmbiguous": len(rows) > 1})
    out[slug] = items
    print(f"{slug:28} {len(items):3} items  {sum(1 for i in items if i['cv']):3} matched  {sum(1 for i in items if i['cvAmbiguous'])} ambiguous")
json.dump(out, open("fia-2026-09-27/shelves.json","w"), indent=1, ensure_ascii=False)
