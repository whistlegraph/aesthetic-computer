#!/usr/bin/env python3
"""Build the revision-03 Google-Docs-native video receipt."""

from __future__ import annotations

import json
import subprocess
from pathlib import Path

from docx import Document
from docx.enum.text import WD_BREAK, WD_PARAGRAPH_ALIGNMENT
from docx.oxml import OxmlElement
from docx.oxml.ns import qn
from docx.shared import Inches, Pt, RGBColor
from PIL import Image as PILImage

HERE = Path(__file__).resolve().parent
REEL = HERE.parent
OUT = REEL / "out"
REV = HERE / "revision-03"
FRAMES = REV / "frames"
OUTPUT = REV / "scores-for-social-software-r03-receipt.docx"
SCREENPLAY = REV / "screenplay.md"
MASTER = OUT / "scores-for-social-software-captioned-08.mp4"
REVIEW_VIDEO = "https://drive.google.com/file/d/17jMTkVX7OCekLxKq8X1-4iRLrWzMA0E5/view"
REVIEW_DOC = "https://docs.google.com/document/d/13YiibAH8Jfsyq-qZ3ugHpxF6XulM-hY8GDo_f854Hd0/edit"
ARTICLE = "https://sosoft.arts.ucla.edu/keymaps-as-social-software/"
FINAL_SOURCE = "https://docs.google.com/document/d/1hNzUm3SmsEBRtM3zWhcQqsYvsoRf4ZioFIQMFndlwXY/edit"


def format_time(seconds: float) -> str:
    rounded = int(round(seconds))
    return f"{rounded // 60:02d}:{rounded % 60:02d}"


def add_hyperlink(paragraph, text: str, url: str) -> None:
    rel_id = paragraph.part.relate_to(
        url,
        "http://schemas.openxmlformats.org/officeDocument/2006/relationships/hyperlink",
        is_external=True,
    )
    link = OxmlElement("w:hyperlink")
    link.set(qn("r:id"), rel_id)
    run = OxmlElement("w:r")
    props = OxmlElement("w:rPr")
    color = OxmlElement("w:color")
    color.set(qn("w:val"), "1155CC")
    underline = OxmlElement("w:u")
    underline.set(qn("w:val"), "single")
    props.extend((color, underline))
    run.append(props)
    node = OxmlElement("w:t")
    node.text = text
    run.append(node)
    link.append(run)
    paragraph._p.append(link)


spec = json.loads((REEL / "narrator-spec.json").read_text())
timeline = json.loads((OUT / "narration-timeline.json").read_text())
timing_by_id = {line["id"]: line for line in timeline["lines"]}
review_duration = format_time(timeline["totalDuration"])
beats = []
for line in spec["lines"]:
    timing = timing_by_id[line["id"]]
    beats.append({**line, **timing})

REV.mkdir(parents=True, exist_ok=True)
FRAMES.mkdir(parents=True, exist_ok=True)
for beat in beats:
    slug = beat["id"].lower()
    frame = FRAMES / f"{slug}.jpg"
    # Sample the closing after its first transition so the receipt visibly
    # records the new Fuser documentation rather than a crossfade boundary.
    offset = 6.5 if beat["id"] == "SSF-00" else (9.5 if beat["id"] == "SSF-11" else 1.0)
    at = min(beat["endSec"] - 0.2, beat["startSec"] + offset)
    subprocess.run(
        [
            "ffmpeg", "-y", "-hide_banner", "-loglevel", "error",
            "-ss", f"{at:.3f}", "-i", str(MASTER), "-frames:v", "1",
            "-vf", "scale=720:-2", "-q:v", "3", str(frame),
        ],
        check=True,
    )
    with PILImage.open(frame) as image:
        rgb = image.convert("RGB")
        rgb.save(frame, format="JPEG", quality=84, optimize=True, dpi=(144, 144))
    beat["frame"] = frame

doc = Document()
section = doc.sections[0]
section.page_width = Inches(8.5)
section.page_height = Inches(11)
section.top_margin = Inches(1)
section.bottom_margin = Inches(1)
section.left_margin = Inches(1)
section.right_margin = Inches(1)

styles = doc.styles
normal = styles["Normal"]
normal.font.name = "Arial"
normal.font.size = Pt(11)
normal.paragraph_format.space_after = Pt(8)
normal.paragraph_format.line_spacing = 1.15

title_style = styles["Title"]
title_style.font.name = "Arial"
title_style.font.size = Pt(24)
title_style.font.color.rgb = RGBColor(0, 0, 0)
title_style.paragraph_format.space_after = Pt(10)

heading_specs = {
    "Heading 1": (16, 18, 8),
    "Heading 2": (14, 14, 6),
    "Heading 3": (12, 10, 4),
}
for name, (size, before, after) in heading_specs.items():
    style = styles[name]
    style.font.name = "Arial"
    style.font.size = Pt(size)
    style.font.color.rgb = RGBColor(0, 0, 0)
    style.paragraph_format.space_before = Pt(before)
    style.paragraph_format.space_after = Pt(after)

doc.add_heading("Scores for Social Software", 0)
p = doc.add_paragraph()
r = p.add_run("Revision 03 video receipt + timecoded screenplay")
r.bold = True
r.font.size = Pt(14)
p = doc.add_paragraph(f"Review cut · {review_duration} · 1080 × 1920 vertical · prepared 27 July 2026")
p.runs[0].font.color.rgb = RGBColor(90, 90, 90)

doc.add_heading("Review links", level=1)
for label, url in (
    ("Watch the revision 03 review video", REVIEW_VIDEO),
    ("Comment on the Google Doc screenplay", REVIEW_DOC),
    ("Open the final article source + Casey edit log", FINAL_SOURCE),
    ("Open the published article: The Keymap Is the Score", ARTICLE),
):
    p = doc.add_paragraph()
    add_hyperlink(p, label, url)

doc.add_heading("How to review", level=1)
doc.add_paragraph(
    "Comment directly on the words or image you want changed and include the stable "
    "scene ID—for example, SSF-04. The scene IDs persist even when later edits move "
    "the timestamps. Script, timing, image choice, and crop notes are all welcome."
)

doc.add_heading("What changed in revision 03", level=1)
changes = (
    ("Casey’s copy feedback", "Corrected the Æther packet description, replaced the limiting ‘same question’ conclusion, and incorporated the accepted Google Doc copy edits into the published article and screenplay."),
    ("Content", "Added ‘64 hand-produced artist scores,’ ‘copy #51,’ the two-edition invitation, Jeffrey’s goodbye, and Casey’s June 13 Fuser photographs; widened Jordan Silver and kept Banyi Huang’s complete credit in one utterance."),
    ("Picture and identity", "Reframed the moving images, sharpened the crop, raised the captions, added a blue chapter bar, animated credit letters individually, and softened the SO/SOFT response into a wobble."),
    ("Sound and delivery", f"Retimed the full picture spine to {review_duration}, expanded the music, balanced the guide voice, and exported a color-tagged BT.709 review cut."),
)
for label, text in changes:
    p = doc.add_paragraph()
    p.add_run(f"{label}. ").bold = True
    p.add_run(text)

doc.add_paragraph().add_run().add_break(WD_BREAK.PAGE)
doc.add_heading("Timecoded screenplay", level=1)
doc.add_paragraph(
    "The current voice is a temporary guide. This screenplay is the review surface "
    "before Jeffrey records the final narration in Narrator Wizard."
)

for index, beat in enumerate(beats):
    if index:
        doc.add_paragraph().add_run().add_break(WD_BREAK.PAGE)
    timecode = f"{format_time(beat['startSec'])}–{format_time(beat['endSec'])}"
    heading = doc.add_heading(level=2)
    heading.alignment = WD_PARAGRAPH_ALIGNMENT.LEFT
    heading.add_run(f"{beat['id']} · {timecode}")
    heading.add_run().add_break()
    heading.add_run(beat["title"])
    heading.paragraph_format.keep_with_next = True
    image_p = doc.add_paragraph()
    image_p.alignment = WD_PARAGRAPH_ALIGNMENT.CENTER
    image_p.paragraph_format.keep_with_next = True
    image_p.add_run().add_picture(str(beat["frame"]), width=Inches(2.55))
    p = doc.add_paragraph()
    p.add_run("Narration. ").bold = True
    p.add_run(beat["text"])
    p = doc.add_paragraph(f"Comment with {beat['id']} to request a script, timing, image, or crop change.")
    p.runs[0].italic = True
    p.runs[0].font.color.rgb = RGBColor(90, 90, 90)

doc.save(OUTPUT)

markdown = [
    "# Scores for Social Software — revision 03 screenplay",
    "",
    f"Review video: {REVIEW_VIDEO}",
    f"Commentable Google Doc: {REVIEW_DOC}",
    f"Published article: {ARTICLE}",
    f"Final article source + edit log: {FINAL_SOURCE}",
    "",
    "> The current voice is a temporary guide. Comment in the Google Doc with a stable SSF scene ID.",
    "",
]
for beat in beats:
    timecode = f"{format_time(beat['startSec'])}–{format_time(beat['endSec'])}"
    markdown.extend((
        f"## {beat['id']} · {beat['title']} · {timecode}",
        "",
        beat["text"],
        "",
    ))
SCREENPLAY.write_text("\n".join(markdown).rstrip() + "\n")
print(OUTPUT)
print(SCREENPLAY)
