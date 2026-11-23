"""Build a searchable title/artist index from the cited ArtBase research snapshot."""
import csv
import io
import json
from pathlib import Path
import re
from urllib.request import urlopen

revision = "abb03fde210df5da4ca473287d05f01f968af67d"
source = f"https://raw.githubusercontent.com/laurentfintoni/Net-Art-Rev/{revision}/Rhizome_data/csv/artwork_complete.csv"
with urlopen(source, timeout=30) as response:
    rows = csv.DictReader(io.TextIOWrapper(response, encoding="utf-8-sig"))
    works = {}
    for row in rows:
        url = row["artwork_page"].strip()
        match = re.fullmatch(r"https://artbase\.rhizome\.org/wiki/(Q\d+)", url)
        title = row["artwork_label"].strip()
        if not match or not title:
            continue
        work = works.setdefault(match[1], {"id": match[1], "title": title, "artists": [], "year": row["inception"][:4]})
        artist = row["artist_label"].strip()
        if artist and artist not in work["artists"]:
            work["artists"].append(artist)

if not works:
    raise ValueError("No identifiable ArtBase works in source")
output = Path(__file__).resolve().parents[2] / "system/public/papers.aesthetic.computer/nmsearch/artbase.json"
catalog = {
    "snapshot": "2022-02-14",
    "source": source,
    "credit": "Rhizome ArtBase via Net Art Revolution? (Margherita Donelli, Chiara Catizone and Laurent Fintoni)",
    "items": list(works.values()),
}
output.write_text(json.dumps(catalog, ensure_ascii=False, separators=(",", ":")) + "\n")
print(f"{len(works)} ArtBase works")
