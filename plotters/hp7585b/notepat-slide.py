#!/usr/bin/env python3
"""
notepat keymap slide — built entirely from native HP-GL primitives.

Emits:
  - notepat-slide.hpgl  (sendable to the 7585B)
  - notepat-slide.svg   (preview, written to the user's Desktop)

Primitives used (all stock HP-GL, no extensions):
  IN, SP, VS, FS    initialise / pen / speed / force
  PA, PU, PD        absolute moves, pen down/up
  EA                edge rectangle absolute   (key outlines)
  RA                fill rectangle absolute   (black piano keys, highlights)
  CI                circle                    (chip dots)
  LT                line type                 (dashed connectors)
  SI, DI            character size / direction
  LB, DT            label / label terminator

The plotter unit is 0.025 mm.  Coordinate origin is the centre of the
plotter's hard-clip area (the 7585B reports P1/P2 centred).

Layout zones (plotter units, ±X / ±Y):
   title        4000 ..  3300
   piano        3100 ..  1500
   connectors   1400 ..   900
   qwerty       800  .. -2200
   tagline     -2400 .. -2900
   chips       -3100 .. -3800
"""

from __future__ import annotations

import argparse
import os
import sys
import time
import webbrowser
from pathlib import Path

import serial


XON, XOFF = 0x11, 0x13


# ---------------------------------------------------------------------------
# Live limits query
# ---------------------------------------------------------------------------
def _ask(ser: serial.Serial, cmd: bytes) -> str:
    ser.reset_input_buffer()
    ser.write(cmd)
    ser.flush()
    deadline = time.time() + 1.0
    buf = bytearray()
    while time.time() < deadline:
        c = ser.read(64)
        if c:
            for b in c:
                if b not in (XON, XOFF):
                    buf.append(b)
            if b"\n" in buf or b"\r" in buf:
                break
        else:
            time.sleep(0.05)
    return buf.decode("ascii", errors="replace").strip()


def _ints(s: str) -> list[int]:
    out = []
    for tok in s.replace(",", " ").split():
        try:
            out.append(int(tok))
        except ValueError:
            pass
    return out


def query_limits(port: str, baud: int) -> tuple[tuple[int, int, int, int], tuple[int, int]]:
    ser = serial.Serial(
        port=port,
        baudrate=baud,
        bytesize=serial.EIGHTBITS,
        parity=serial.PARITY_NONE,
        stopbits=serial.STOPBITS_ONE,
        xonxoff=False,
        rtscts=True,
        dsrdtr=True,
        timeout=0.5,
    )
    ser.setDTR(True)
    ser.setRTS(True)
    try:
        pp = _ints(_ask(ser, b"OP;"))
        of = _ints(_ask(ser, b"OF;"))
    finally:
        ser.close()
    if len(pp) != 4 or len(of) < 2:
        raise SystemExit("plotter did not respond to OP;/OF;")
    return (pp[0], pp[1], pp[2], pp[3]), (of[0], of[1])


# ---------------------------------------------------------------------------
# Composer that emits HPGL + SVG simultaneously
# ---------------------------------------------------------------------------
class Composer:
    """Each primitive call appends both an HP-GL string and an SVG element."""

    def __init__(self, p1p2: tuple[int, int, int, int], factor: tuple[int, int]):
        self.p1p2 = p1p2
        self.fx, self.fy = factor
        # Explicit *printable* label terminator. Our 7585B firmware doesn't
        # accept the optional suppress flag on DT (E2 = wrong # of params),
        # so we use plain `DT~;` and accept that '~' draws as a small visible
        # mark after each label. Better visible than runaway labels.
        self.hpgl: list[str] = ["IN;DT~;DI1,0;LT;SP1;VS12;FS3;"]
        self.svg: list[str] = []
        self._line_type: int | None = None  # current LT setting (None = solid)

        x1, y1, x2, y2 = p1p2
        self.w_units = x2 - x1
        self.h_units = y2 - y1
        # SVG viewBox in mm (drawing space). Origin at center, y-up like HPGL.
        self.svg_w_mm = self.w_units / self.fx
        self.svg_h_mm = self.h_units / self.fy

    # --- coord helpers ---
    def _to_svg_xy(self, x: int, y: int) -> tuple[float, float]:
        # plotter is y-up at center; SVG is y-down at top-left.
        sx = (x - self.p1p2[0]) / self.fx
        sy = (self.p1p2[3] - y) / self.fy
        return sx, sy

    def set_line_type(self, lt: int | None) -> None:
        if lt == self._line_type:
            return
        if lt is None:
            self.hpgl.append("LT;")
        else:
            # Pattern length in percent of P1-P2 diagonal. 0.3% ≈ 1.4mm
            # on this sheet — very fine tics.
            self.hpgl.append(f"LT{lt},0.3;")
        self._line_type = lt

    # --- primitives ---
    def edge_rect(self, x1: int, y1: int, x2: int, y2: int) -> None:
        self.set_line_type(None)
        self.hpgl.append(f"PA{x1},{y1};EA{x2},{y2};")
        sx1, sy1 = self._to_svg_xy(x1, y1)
        sx2, sy2 = self._to_svg_xy(x2, y2)
        self.svg.append(
            f'<rect x="{min(sx1, sx2):.3f}" y="{min(sy1, sy2):.3f}" '
            f'width="{abs(sx2 - sx1):.3f}" height="{abs(sy2 - sy1):.3f}" '
            f'fill="none" stroke="#111" stroke-width="0.4"/>'
        )

    def double_rect(self, x1: int, y1: int, x2: int, y2: int, inset: int = 80) -> None:
        """Outer + inner offset rect — for highlighting without filling."""
        self.edge_rect(x1, y1, x2, y2)
        self.edge_rect(x1 + inset, y1 + inset, x2 - inset, y2 - inset)

    # ---- richer HP-GL primitives ----
    def set_fill(self, fill_type: int, spacing: int = 0, angle_deg: int = 0) -> None:
        """FT n[,sp,a]; — set the fill pattern used by FP / RA / WG.
        type 1 = solid, 2 = parallel hatch, 3 = cross-hatch, 4 = dots.
        spacing is in plotter units; angle is degrees."""
        if spacing > 0:
            self.hpgl.append(f"FT{fill_type},{spacing},{angle_deg};")
        else:
            self.hpgl.append(f"FT{fill_type};")

    def hatched_rect(
        self, x1: int, y1: int, x2: int, y2: int, spacing: int = 80, angle: int = 45
    ) -> None:
        """Cross-hatched rectangle — outline + hatched fill via FT3 + RA."""
        self.set_line_type(None)
        # FT 3 = cross-hatch, then RA fills, then EA edges.
        self.hpgl.append(
            f"FT3,{spacing},{angle};"
            f"PA{x1},{y1};RA{x2},{y2};EA{x2},{y2};FT;"
        )
        sx1, sy1 = self._to_svg_xy(x1, y1)
        sx2, sy2 = self._to_svg_xy(x2, y2)
        self.svg.append(
            f'<defs><pattern id="h{len(self.svg)}" patternUnits="userSpaceOnUse" '
            f'width="2" height="2" patternTransform="rotate({angle})">'
            f'<line x1="0" y1="0" x2="0" y2="2" stroke="#111" stroke-width="0.3"/>'
            f'<line x1="0" y1="0" x2="2" y2="0" stroke="#111" stroke-width="0.3"/>'
            f'</pattern></defs>'
            f'<rect x="{min(sx1, sx2):.3f}" y="{min(sy1, sy2):.3f}" '
            f'width="{abs(sx2 - sx1):.3f}" height="{abs(sy2 - sy1):.3f}" '
            f'fill="url(#h{len(self.svg) - 1})" stroke="#111" stroke-width="0.4"/>'
        )

    def edge_polygon(self, points: list[tuple[int, int]]) -> None:
        """EP — outline a polygon defined via PM."""
        if len(points) < 3:
            return
        self.set_line_type(None)
        cmd = [f"PA{points[0][0]},{points[0][1]};PM0;"]
        for x, y in points[1:]:
            cmd.append(f"PA{x},{y};")
        cmd.append("PM2;EP;")
        self.hpgl.append("".join(cmd))
        svg_pts = " ".join(
            f"{self._to_svg_xy(x, y)[0]:.3f},{self._to_svg_xy(x, y)[1]:.3f}"
            for x, y in points
        )
        self.svg.append(
            f'<polygon points="{svg_pts}" fill="none" stroke="#111" stroke-width="0.4"/>'
        )

    def fill_polygon(
        self,
        points: list[tuple[int, int]],
        fill_type: int = 3,
        spacing: int = 80,
        angle: int = 45,
    ) -> None:
        """FP — fill a polygon defined via PM, with FT controlling pattern."""
        if len(points) < 3:
            return
        self.set_line_type(None)
        cmd = [
            f"FT{fill_type},{spacing},{angle};",
            f"PA{points[0][0]},{points[0][1]};PM0;",
        ]
        for x, y in points[1:]:
            cmd.append(f"PA{x},{y};")
        cmd.append("PM2;FP;EP;FT;")
        self.hpgl.append("".join(cmd))
        svg_pts = " ".join(
            f"{self._to_svg_xy(x, y)[0]:.3f},{self._to_svg_xy(x, y)[1]:.3f}"
            for x, y in points
        )
        self.svg.append(
            f'<polygon points="{svg_pts}" fill="#888" fill-opacity="0.35" '
            f'stroke="#111" stroke-width="0.4"/>'
        )

    def edge_wedge(
        self, cx: int, cy: int, r: int, start_deg: float, sweep_deg: float
    ) -> None:
        """EW r,start,sweep; — outline a pie-slice. Centre is current pen pos."""
        self.set_line_type(None)
        self.hpgl.append(f"PA{cx},{cy};EW{r},{start_deg:.2f},{sweep_deg:.2f};")
        sx, sy = self._to_svg_xy(cx, cy)
        rx = r / self.fx
        # SVG arc path
        import math
        a1 = math.radians(start_deg)
        a2 = math.radians(start_deg + sweep_deg)
        x1s = sx + rx * math.cos(a1)
        y1s = sy - rx * math.sin(a1)
        x2s = sx + rx * math.cos(a2)
        y2s = sy - rx * math.sin(a2)
        large = 1 if abs(sweep_deg) > 180 else 0
        sweep = 0 if sweep_deg > 0 else 1  # SVG arcs Y-down
        self.svg.append(
            f'<path d="M {sx:.3f} {sy:.3f} L {x1s:.3f} {y1s:.3f} '
            f'A {rx:.3f} {rx:.3f} 0 {large} {sweep} {x2s:.3f} {y2s:.3f} Z" '
            f'fill="none" stroke="#111" stroke-width="0.4"/>'
        )

    def set_slant(self, slant: float) -> None:
        """SL tan(angle); — character slant. 0 = upright; ~0.4 = italic-ish."""
        self.hpgl.append(f"SL{slant:.3f};")

    def svg_polylines(
        self,
        svg_path: str,
        cx: int,
        cy: int,
        target_size: int,
        bezier_samples: int = 6,
    ) -> None:
        """Load an SVG file, flatten every <path> into polylines (sampling
        beziers), scale to fit `target_size` plotter units in the longer
        axis, centre at (cx, cy), and emit as point-to-point PA/PD strokes.
        """
        import svgelements

        svg = svgelements.SVG.parse(svg_path)
        polylines: list[list[tuple[float, float]]] = []
        current: list[tuple[float, float]] = []

        def flush() -> None:
            nonlocal current
            if len(current) >= 2:
                polylines.append(current)
            current = []

        for el in svg.elements():
            if not isinstance(el, svgelements.Path):
                continue
            for seg in el:
                if isinstance(seg, svgelements.Move):
                    flush()
                    current = [(seg.end.x, seg.end.y)]
                elif isinstance(seg, svgelements.Line):
                    current.append((seg.end.x, seg.end.y))
                elif isinstance(seg, (svgelements.CubicBezier, svgelements.QuadraticBezier, svgelements.Arc)):
                    # Faceted approximation — one straight line per curve segment.
                    # No bezier sampling: just draw start→end. Cleaner, smaller,
                    # and the polygonal look is intentional for the plot aesthetic.
                    current.append((seg.end.x, seg.end.y))
                elif isinstance(seg, svgelements.Close):
                    if current:
                        current.append(current[0])
                    flush()
            flush()

        if not polylines:
            return

        # bounding box, scale, centre
        xs = [x for pl in polylines for x, _ in pl]
        ys = [y for pl in polylines for _, y in pl]
        sw = max(xs) - min(xs)
        sh = max(ys) - min(ys)
        scale = target_size / max(sw, sh)
        scx = (min(xs) + max(xs)) / 2
        scy = (min(ys) + max(ys)) / 2

        def xform(p: tuple[float, float]) -> tuple[int, int]:
            x, y = p
            px = cx + (x - scx) * scale
            py = cy - (y - scy) * scale  # flip Y for HPGL (y-up)
            return int(round(px)), int(round(py))

        # de-duplicate consecutive points within ~1 plotter unit
        for pl in polylines:
            tpts: list[tuple[int, int]] = []
            for p in pl:
                tp = xform(p)
                if tpts and abs(tp[0] - tpts[-1][0]) + abs(tp[1] - tpts[-1][1]) < 2:
                    continue
                tpts.append(tp)
            if len(tpts) < 2:
                continue
            self.set_line_type(None)
            cmd = [f"PU;PA{tpts[0][0]},{tpts[0][1]};PD;"]
            for x, y in tpts[1:]:
                cmd.append(f"PA{x},{y};")
            cmd.append("PU;")
            self.hpgl.append("".join(cmd))
            svg_pts = " ".join(
                f"{self._to_svg_xy(x, y)[0]:.3f},{self._to_svg_xy(x, y)[1]:.3f}"
                for x, y in tpts
            )
            self.svg.append(
                f'<polyline points="{svg_pts}" fill="none" stroke="#111" stroke-width="0.4"/>'
            )

    def line(self, x1: int, y1: int, x2: int, y2: int, dashed: bool = False) -> None:
        self.set_line_type(2 if dashed else None)
        self.hpgl.append(f"PU;PA{x1},{y1};PD;PA{x2},{y2};PU;")
        sx1, sy1 = self._to_svg_xy(x1, y1)
        sx2, sy2 = self._to_svg_xy(x2, y2)
        dasharray = ' stroke-dasharray="2 2"' if dashed else ""
        self.svg.append(
            f'<line x1="{sx1:.3f}" y1="{sy1:.3f}" x2="{sx2:.3f}" y2="{sy2:.3f}" '
            f'stroke="#111" stroke-width="0.3"{dasharray}/>'
        )

    def circle(self, cx: int, cy: int, r: int, fill: bool = False) -> None:
        self.set_line_type(None)
        # never fill — just an outline.
        self.hpgl.append(f"PA{cx},{cy};CI{r};")
        sx, sy = self._to_svg_xy(cx, cy)
        rx = r / self.fx
        self.svg.append(
            f'<circle cx="{sx:.3f}" cy="{sy:.3f}" r="{rx:.3f}" '
            f'fill="none" stroke="#111" stroke-width="0.3"/>'
        )

    def label(
        self,
        cx: int,
        cy: int,
        text: str,
        char_w_cm: float,
        anchor: str = "middle",
    ) -> None:
        """Draw a horizontal label centered (anchor=middle) or left (start)
        at (cx, cy). cy is the baseline. Auto-shrinks the character size if
        the label would otherwise start beyond the plot's hard-clip limit."""
        self.set_line_type(None)
        spacing = 1.5
        char_h_cm = char_w_cm * 1.5

        # plot bounds in plotter units (P1..P2 with a small inner margin)
        x1 = int(self.p1p2[0] * 0.99)
        x2 = int(self.p1p2[2] * 0.99)
        avail = x2 - x1

        text_w = int(len(text) * char_w_cm * spacing * 10 * self.fx)
        if text_w > avail:
            scale = avail / text_w
            char_w_cm *= scale
            # Floor: the 7585B refuses SI values below ~0.5 cm with E3.
            char_w_cm = max(char_w_cm, 0.5)
            char_h_cm = char_w_cm * 1.5
            text_w = int(len(text) * char_w_cm * spacing * 10 * self.fx)

        if anchor == "middle":
            x = cx - text_w // 2
        elif anchor == "end":
            x = cx - text_w
        else:
            x = cx
        # clamp to plot area so PA can never go off-limit
        x = max(x1, min(x, x2 - text_w))

        # Strip any '~' (our terminator) or '\x03' from label content.
        safe = text.replace("~", "").replace("\x03", "")
        self.hpgl.append(
            f"SI{char_w_cm:.3f},{char_h_cm:.3f};"
            f"PA{x},{cy};LB{safe}~"
        )
        sx, sy = self._to_svg_xy(cx, cy)
        font_px = char_h_cm * 10  # in mm
        self.svg.append(
            f'<text x="{sx:.3f}" y="{sy:.3f}" font-size="{font_px:.2f}" '
            f'font-family="Helvetica, Arial, sans-serif" '
            f'text-anchor="{anchor}" fill="#111">{_xml_escape(text)}</text>'
        )

    def end(self) -> None:
        self.hpgl.append("LT;PU;PA0,0;SP0;")

    # --- output ---
    def hpgl_bytes(self) -> bytes:
        return ("\n".join(self.hpgl) + "\n").encode("ascii")

    def svg_doc(self) -> str:
        body = "\n  ".join(self.svg)
        return (
            f'<?xml version="1.0" encoding="UTF-8"?>\n'
            f'<svg xmlns="http://www.w3.org/2000/svg" '
            f'width="{self.svg_w_mm:.2f}mm" height="{self.svg_h_mm:.2f}mm" '
            f'viewBox="0 0 {self.svg_w_mm:.2f} {self.svg_h_mm:.2f}">\n'
            f'  <rect x="0" y="0" width="{self.svg_w_mm:.2f}" '
            f'height="{self.svg_h_mm:.2f}" fill="#fafafa"/>\n'
            f'  {body}\n'
            f'</svg>\n'
        )


def _xml_escape(s: str) -> str:
    return (
        s.replace("&", "&amp;")
        .replace("<", "&lt;")
        .replace(">", "&gt;")
        .replace('"', "&quot;")
    )


# ---------------------------------------------------------------------------
# Slide composition
# ---------------------------------------------------------------------------
NATURALS = list("CDEFGABCDEFGAB")  # 14 white keys = 2 octaves
# black-key positions: True if a sharp sits *above* this natural's right edge.
HAS_SHARP_RIGHT = [True, True, False, True, True, True, False] * 2  # C# D# (skip E) F# G# A# (skip B)


def compose_slide(
    p1p2: tuple[int, int, int, int],
    factor: tuple[int, int],
    title: str = "notepat.com keymap",
    tagline: str = "C  D  E  F  G  A  B  play the notes they name",
) -> Composer:
    c = Composer(p1p2, factor)
    x1, y1, x2, y2 = p1p2

    # ---- title (with a touch of slant for character) ----
    c.set_slant(0.20)  # ~11° forward slant
    c.label(0, 3500, title, char_w_cm=1.6)
    c.set_slant(0.0)   # back to upright for the rest

    # divider under header
    c.line(int(x1 * 0.94), 3050, int(x2 * 0.94), 3050)

    # ---- PALS logo in top-right corner — real SVG flattened to polylines ----
    logo_cx = int(x2 * 0.88)
    logo_cy = 3450
    pals_svg = "/Users/jas/aesthetic-computer/bills/invoices/pals.svg"
    c.svg_polylines(pals_svg, logo_cx, logo_cy, target_size=1000, bezier_samples=5)

    # ---- piano (2 octaves) ----
    n_white = 14
    white_w = 900
    white_h = 1300
    piano_top = 2500
    piano_bot = piano_top - white_h  # = 1200
    piano_x_start = -(n_white * white_w) // 2
    black_w = 540
    black_h = 800

    # remember each white key's geometric centre for connector use
    white_centers: dict[str, tuple[int, int]] = {}
    piano_mid_y = (piano_top + piano_bot) // 2
    octave = 0
    for i, note in enumerate(NATURALS):
        kx1 = piano_x_start + i * white_w
        kx2 = kx1 + white_w
        c.edge_rect(kx1, piano_bot, kx2, piano_top)
        # natural label inside, lower part — bigger so it's actually legible
        c.label((kx1 + kx2) // 2, piano_bot + 170, note, char_w_cm=0.55)
        # tag this key
        if note == "C" and i > 0:
            octave = 1
        white_centers[f"{note}{octave}"] = ((kx1 + kx2) // 2, piano_mid_y)

    # "black" keys — cross-hatched (FT3) at 80-unit spacing / 45°, so they
    # read as the sharps without scribbling a solid block of ink.
    for i, has in enumerate(HAS_SHARP_RIGHT):
        if not has or i >= n_white - 1:
            continue
        kx1 = piano_x_start + i * white_w + white_w - black_w // 2
        kx2 = kx1 + black_w
        c.hatched_rect(kx1, piano_top - black_h, kx2, piano_top, spacing=80, angle=45)

    # ---- dashed connectors from C-D-E-F-G (lower octave) to QWERTY ----
    qwerty_top = 800
    qwerty_bot = -2200

    # First-octave naturals and their QWERTY home keys
    note_to_qwerty = {
        "C": ("C", -1),  # row, with index in row
        "D": ("D", -1),
        "E": ("E", -1),
        "F": ("F", -1),
        "G": ("G", -1),
    }

    # ---- QWERTY ----
    rows = [
        ("0", list("QWERTYUIOP")),
        ("1", list("ASDFGHJKL'")),
        ("2", list("ZXCVBNM,./")),
    ]
    cap_w = 1300
    cap_h = 900
    gap = 100
    slot = cap_w + gap
    row_offsets = [0, 350, 800]
    row_y_top = [qwerty_top, qwerty_top - cap_h - 150, qwerty_top - 2 * (cap_h + 150)]

    qwerty_centers: dict[str, tuple[int, int]] = {}
    notes_set = set("CDEFGAB")
    for r_idx, (rname, keys) in enumerate(rows):
        n = len(keys)
        row_w = n * slot - gap
        row_x_start = -row_w // 2 + row_offsets[r_idx]
        ytop = row_y_top[r_idx]
        ybot = ytop - cap_h
        for i, k in enumerate(keys):
            kx1 = row_x_start + i * slot
            kx2 = kx1 + cap_w
            if k in notes_set:
                # highlight via double-stroked outline — no fill.
                c.double_rect(kx1, ybot, kx2, ytop, inset=90)
            else:
                c.edge_rect(kx1, ybot, kx2, ytop)
            cap_cx = (kx1 + kx2) // 2
            cap_cy = (ytop + ybot) // 2
            c.label(cap_cx, ybot + cap_h // 3, k, char_w_cm=0.85)
            qwerty_centers[k] = (cap_cx, cap_cy)

    # ---- connectors: centre-of-piano-key → centre-of-qwerty-cap ----
    for note in "CDEFG":
        if note in qwerty_centers and f"{note}0" in white_centers:
            px, py = white_centers[f"{note}0"]
            qx, qy = qwerty_centers[note]
            c.line(px, py, qx, qy, dashed=True)

    # ---- tagline ----
    c.label(0, -2700, tagline, char_w_cm=0.7)

    # ---- URL row — short enough to fit at min SI 0.5 ----
    c.label(0, -3300, "notepat lives in:", char_w_cm=0.5)
    c.label(0, -3700, "notepat.com   prompt.ac/menuband   aesthetic.computer", char_w_cm=0.55)

    c.end()
    return c


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------
def main() -> int:
    p = argparse.ArgumentParser()
    p.add_argument("--port", default=os.environ.get("HP7585B_TTY", ""))
    p.add_argument("--baud", type=int, default=9600)
    p.add_argument("--no-query", action="store_true",
                   help="skip plotter query, use last-known ANSI B P1/P2")
    p.add_argument("--out-hpgl", default=str(Path(__file__).parent / "notepat-slide.hpgl"))
    p.add_argument("--out-svg", default=str(Path.home() / "Desktop" / "notepat-slide-preview.svg"))
    p.add_argument("--open", action="store_true",
                   help="open the SVG preview in the default app")
    args = p.parse_args()

    if args.no_query or not args.port:
        # last-known ANSI B landscape sheet from earlier OH;/OP;
        p1p2 = (-8050, -4574, 8050, 4574)
        factor = (40, 40)
        sys.stderr.write(f"using cached limits {p1p2} factor={factor}\n")
    else:
        p1p2, factor = query_limits(args.port, args.baud)
        sys.stderr.write(f"queried limits {p1p2} factor={factor}\n")

    comp = compose_slide(p1p2, factor)

    Path(args.out_hpgl).write_bytes(comp.hpgl_bytes())
    Path(args.out_svg).write_text(comp.svg_doc())

    sys.stderr.write(
        f"wrote HPGL: {args.out_hpgl} ({len(comp.hpgl_bytes())} B)\n"
        f"wrote SVG:  {args.out_svg}\n"
    )

    if args.open:
        webbrowser.open(f"file://{args.out_svg}")

    return 0


if __name__ == "__main__":
    sys.exit(main())
