import json, sys
from pathlib import Path
from PIL import Image, ImageDraw, ImageFont
idx = json.loads(Path(sys.argv[1]).read_text()); src = Path(sys.argv[2]); out = sys.argv[3]
cell, cols = 230, 8
rows = (len(idx) + cols - 1) // cols
sheet = Image.new("RGB", (cols * cell, rows * (cell + 26)), "white"); d = ImageDraw.Draw(sheet)
font = ImageFont.load_default(size=13)
for i, m in enumerate(idx):
    im = Image.open(src / f"{m['name']}.png").convert("RGB"); im.thumbnail((cell - 20, cell - 20))
    x, y = (i % cols) * cell, (i // cols) * (cell + 26)
    sheet.paste(im, (x + (cell - im.width) // 2, y + (cell - im.height) // 2))
    d.text((x + 4, y + cell + 2), m["name"].replace("choir-", ""), fill=(200, 30, 60), font=font)
sheet.save(out)
