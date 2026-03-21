"""ac — Aesthetic Computer notebook utilities.

Usage (in any notebook):
    import sys, os; sys.path.insert(0, os.path.join(os.getcwd(), "notebooks") if not os.getcwd().endswith("notebooks") else os.getcwd())
    from ac import kidlisp, show_card

    # Render and display a KidLisp program inline
    show_card("Starburst", "(wipe black) (ink white) (repeat 72 i (line 80 60 ...))")

    # Just render to PIL image (no display)
    img = kidlisp("(wipe navy) (ink cyan) (circle 80 60 40)")
"""

import math, io, base64, sys, os

# Ensure notebooks/ is on sys.path for sibling imports
_dir = os.path.dirname(os.path.abspath(__file__))
if _dir not in sys.path:
    sys.path.insert(0, _dir)
from PIL import Image, ImageDraw
from IPython.display import display, HTML


# ---------------------------------------------------------------------------
# Colors
# ---------------------------------------------------------------------------

CSS_COLORS = {
    "black": (0,0,0), "white": (255,255,255), "red": (255,0,0),
    "green": (0,128,0), "blue": (0,0,255), "lime": (0,255,0),
    "cyan": (0,255,255), "magenta": (255,0,255), "yellow": (255,255,0),
    "orange": (255,165,0), "navy": (0,0,128), "purple": (128,0,128),
    "brown": (139,69,19), "pink": (255,192,203), "gray": (128,128,128),
    "grey": (128,128,128), "beige": (245,245,220), "coral": (255,127,80),
    "gold": (255,215,0), "silver": (192,192,192), "teal": (0,128,128),
    "maroon": (128,0,0), "olive": (128,128,0), "aqua": (0,255,255),
    "salmon": (250,128,114), "khaki": (240,230,140),
    "indigo": (75,0,130), "violet": (238,130,238), "crimson": (220,20,60),
    "tomato": (255,99,71), "turquoise": (64,224,208), "plum": (221,160,221),
    "tan": (210,180,140), "sienna": (160,82,45), "peru": (205,133,63),
    "lavender": (230,230,250), "ivory": (255,255,240), "linen": (250,240,230),
    "wheat": (245,222,179), "chocolate": (210,105,30), "firebrick": (178,34,34),
}

def _resolve_color(c):
    if isinstance(c, tuple): return c
    if isinstance(c, str):
        c = c.strip().strip('"').strip("'").lower()
        if c in CSS_COLORS: return CSS_COLORS[c]
    if isinstance(c, (int, float)): return (int(c), int(c), int(c))
    return (255, 255, 255)


# ---------------------------------------------------------------------------
# Canvas
# ---------------------------------------------------------------------------

class _KLCanvas:
    def __init__(self, w=160, h=120):
        self.W, self.H = w, h
        self.img = Image.new("RGBA", (w, h), (0, 0, 0, 255))
        self.draw = ImageDraw.Draw(self.img)
        self.ink = (255, 255, 255, 255)
        self.fill_mode = True
        self.stroke_w = 1
        self.env = {}
        self.fns = {}

    def set_ink(self, *args):
        args = [a for a in args if a is not None]
        if len(args) == 1:
            c = _resolve_color(args[0])
            self.ink = (*c[:3], 255) if len(c) < 4 else c
        elif len(args) == 2:
            c = _resolve_color(args[0])
            self.ink = (*c[:3], int(args[1]))
        elif len(args) >= 3:
            vals = [int(float(a)) for a in args[:4]]
            if len(vals) == 3: vals.append(255)
            self.ink = tuple(vals)

    def wipe(self, *args):
        if args:
            c = _resolve_color(args[0]) if len(args) == 1 else tuple(int(float(a)) for a in args[:3])
            self.img.paste(Image.new("RGBA", (self.W, self.H), (*c[:3], 255)))
        else:
            self.img.paste(Image.new("RGBA", (self.W, self.H), (0, 0, 0, 255)))
        self.draw = ImageDraw.Draw(self.img)

    def line(self, x1, y1, x2, y2):
        self.draw.line([(x1, y1), (x2, y2)], fill=self.ink, width=max(1, self.stroke_w))

    def box(self, x, y, w, h):
        if self.fill_mode:
            self.draw.rectangle([(x, y), (x+w, y+h)], fill=self.ink)
        else:
            self.draw.rectangle([(x, y), (x+w, y+h)], outline=self.ink, width=self.stroke_w)

    def circle(self, cx, cy, r):
        r = abs(r)
        if self.fill_mode:
            self.draw.ellipse([(cx-r, cy-r), (cx+r, cy+r)], fill=self.ink)
        else:
            self.draw.ellipse([(cx-r, cy-r), (cx+r, cy+r)], outline=self.ink, width=self.stroke_w)

    def tri(self, x1, y1, x2, y2, x3, y3):
        pts = [(x1, y1), (x2, y2), (x3, y3)]
        if self.fill_mode:
            self.draw.polygon(pts, fill=self.ink)
        else:
            self.draw.polygon(pts, outline=self.ink)

    def plot(self, x, y):
        ix, iy = int(x), int(y)
        if 0 <= ix < self.W and 0 <= iy < self.H:
            self.img.putpixel((ix, iy), self.ink)

    def shape(self, *coords):
        pts = [(coords[i], coords[i+1]) for i in range(0, len(coords)-1, 2)]
        if self.fill_mode:
            self.draw.polygon(pts, fill=self.ink)
        else:
            self.draw.polygon(pts, outline=self.ink)

    # --- turtle graphics ---
    def _init_turtle(self):
        if not hasattr(self, '_tx'):
            self._tx, self._ty = self.W / 2, self.H / 2
            self._tangle = 0  # degrees, 0 = right
            self._tpen = True

    def crawl(self, steps=1):
        self._init_turtle()
        rad = math.radians(self._tangle)
        nx = self._tx + steps * math.cos(rad)
        ny = self._ty + steps * math.sin(rad)
        if self._tpen:
            self.draw.line([(self._tx, self._ty), (nx, ny)],
                           fill=self.ink, width=max(1, self.stroke_w))
        self._tx, self._ty = nx, ny

    def turtle_left(self, deg=1):
        self._init_turtle(); self._tangle -= deg

    def turtle_right(self, deg=1):
        self._init_turtle(); self._tangle += deg

    def turtle_up(self):
        self._init_turtle(); self._tpen = False

    def turtle_down(self):
        self._init_turtle(); self._tpen = True

    def turtle_goto(self, x=None, y=None):
        self._init_turtle()
        if x is None: x = self.W / 2
        if y is None: y = self.H / 2
        if self._tpen:
            self.draw.line([(self._tx, self._ty), (x, y)],
                           fill=self.ink, width=max(1, self.stroke_w))
        self._tx, self._ty = x, y

    def turtle_face(self, angle=0):
        self._init_turtle(); self._tangle = angle


# ---------------------------------------------------------------------------
# Parser
# ---------------------------------------------------------------------------

def _tokenize(src):
    tokens = []
    i = 0
    while i < len(src):
        c = src[i]
        if c == ';':
            while i < len(src) and src[i] != '\n': i += 1
            continue
        if c in '()':
            tokens.append(c); i += 1; continue
        if c in ' \t\n\r,':
            i += 1; continue
        if c == '"':
            j = i + 1
            while j < len(src) and src[j] != '"': j += 1
            tokens.append(src[i:j+1]); i = j + 1; continue
        j = i
        while j < len(src) and src[j] not in ' \t\n\r,();"': j += 1
        tokens.append(src[i:j]); i = j
    return tokens

def _parse_atom(t):
    if t.startswith('"') and t.endswith('"'): return t[1:-1]
    try: return int(t)
    except ValueError: pass
    try: return float(t)
    except ValueError: pass
    return t

def _parse_one(tokens, pos):
    if tokens[pos] == '(':
        pos += 1; sub = []
        while pos < len(tokens) and tokens[pos] != ')':
            val, pos = _parse_one(tokens, pos); sub.append(val)
        return sub, pos + 1
    return _parse_atom(tokens[pos]), pos + 1

def _parse(tokens):
    results = []; pos = 0
    while pos < len(tokens):
        t = tokens[pos]
        if t == '(':
            pos += 1; sub = []
            while pos < len(tokens) and tokens[pos] != ')':
                val, pos = _parse_one(tokens, pos); sub.append(val)
            pos += 1; results.append(sub)
        elif t == ')':
            break
        else:
            results.append(_parse_atom(t)); pos += 1
    return results

def _parse_program(src):
    lines = src.strip().split('\n')
    converted = []
    for line in lines:
        s = line.strip()
        if not s or s.startswith(';'): converted.append(s); continue
        if s.startswith('('):
            converted.append(s)
        else:
            for part in (p.strip() for p in s.split(',')):
                if not part: continue
                toks = part.split()
                if len(toks) == 1 and toks[0] in CSS_COLORS:
                    converted.append(f'(wipe "{toks[0]}")')
                elif len(toks) == 1:
                    converted.append(f'({toks[0]})')
                else:
                    converted.append(f'({part})')
    return _parse(_tokenize(' '.join(converted)))


# ---------------------------------------------------------------------------
# Evaluator
# ---------------------------------------------------------------------------

def _eval(c, expr):
    if isinstance(expr, (int, float)): return expr
    if isinstance(expr, str):
        if expr in ('w', 'width'): return c.W
        if expr in ('h', 'height'): return c.H
        if expr == 'pi': return math.pi
        if expr in c.env: return c.env[expr]
        if expr in CSS_COLORS: return expr
        return expr
    if not isinstance(expr, list) or not expr: return expr

    h = expr[0]

    # --- arithmetic ---
    if h == '+':  return sum(v for v in (_eval(c, a) for a in expr[1:]) if isinstance(v, (int, float)))
    if h == '-':
        vs = [_eval(c, a) for a in expr[1:]]
        return -vs[0] if len(vs) == 1 else vs[0] - sum(vs[1:])
    if h == '*':
        r = 1
        for v in (_eval(c, a) for a in expr[1:]):
            if isinstance(v, (int, float)): r *= v
        return r
    if h == '/':
        vs = [_eval(c, a) for a in expr[1:]]
        return vs[0] / vs[1] if len(vs) >= 2 and vs[1] != 0 else 0
    if h == '%':
        vs = [_eval(c, a) for a in expr[1:]]
        return vs[0] % vs[1] if len(vs) >= 2 and vs[1] != 0 else 0
    if h == '&':
        vs = [int(_eval(c, a)) for a in expr[1:]]
        return vs[0] & vs[1] if len(vs) >= 2 else 0
    if h == 'abs':   return abs(_eval(c, expr[1]))
    if h == 'sin':   return math.sin(_eval(c, expr[1]))
    if h == 'cos':   return math.cos(_eval(c, expr[1]))
    if h == 'tan':   return math.tan(_eval(c, expr[1]))
    if h == 'sqrt':  return math.sqrt(max(0, _eval(c, expr[1])))
    if h == 'pow':   return math.pow(_eval(c, expr[1]), _eval(c, expr[2]))
    if h == 'floor': return math.floor(_eval(c, expr[1]))
    if h == 'ceil':  return math.ceil(_eval(c, expr[1]))
    if h == 'round': return round(_eval(c, expr[1]))
    if h == 'min':   return min(_eval(c, a) for a in expr[1:])
    if h == 'max':   return max(_eval(c, a) for a in expr[1:])
    if h == '=':
        vs = [_eval(c, a) for a in expr[1:]]
        return 1 if len(vs) >= 2 and vs[0] == vs[1] else 0
    if h in ('<', '>', '<=', '>=', '!='):
        a, b = _eval(c, expr[1]), _eval(c, expr[2])
        return 1 if (h == '<' and a < b) or (h == '>' and a > b) or \
                     (h == '<=' and a <= b) or (h == '>=' and a >= b) or \
                     (h == '!=' and a != b) else 0

    # --- drawing ---
    if h == 'wipe':    c.wipe(*[_eval(c, a) for a in expr[1:]]); return
    if h == 'ink':     c.set_ink(*[_eval(c, a) for a in expr[1:]]); return
    if h == 'line':
        vs = [_eval(c, a) for a in expr[1:]]
        if len(vs) >= 4: c.line(*[float(v) for v in vs[:4]])
        return
    if h == 'box':
        vs = [_eval(c, a) for a in expr[1:]]
        if len(vs) >= 4: c.box(*[float(v) for v in vs[:4]])
        return
    if h == 'circle':
        vs = [_eval(c, a) for a in expr[1:]]
        if len(vs) >= 3: c.circle(*[float(v) for v in vs[:3]])
        return
    if h == 'tri':
        vs = [_eval(c, a) for a in expr[1:]]
        if len(vs) >= 6: c.tri(*[float(v) for v in vs[:6]])
        return
    if h == 'plot':
        vs = [_eval(c, a) for a in expr[1:]]
        if len(vs) >= 2: c.plot(float(vs[0]), float(vs[1]))
        return
    if h == 'shape':
        vs = [_eval(c, a) for a in expr[1:]]
        c.shape(*[float(v) for v in vs if isinstance(v, (int, float))])
        return
    if h == 'fill':       c.fill_mode = True; return
    if h == 'outline':    c.fill_mode = False; return
    if h == 'stroke':     c.stroke_w = int(_eval(c, expr[1])); return
    if h == 'resolution':
        nw, nh = int(_eval(c, expr[1])), int(_eval(c, expr[2]))
        c.W, c.H = nw, nh
        c.img = Image.new("RGBA", (nw, nh), (0, 0, 0, 255))
        c.draw = ImageDraw.Draw(c.img)
        return
    if h == 'flood':
        vs = [_eval(c, a) for a in expr[1:]]
        if len(vs) >= 2: ImageDraw.floodfill(c.img, (int(vs[0]), int(vs[1])), c.ink)
        return

    # --- turtle ---
    if h == 'crawl':
        vs = [_eval(c, a) for a in expr[1:]]
        c.crawl(float(vs[0]) if vs else 1); return
    if h == 'left':
        vs = [_eval(c, a) for a in expr[1:]]
        c.turtle_left(float(vs[0]) if vs else 1); return
    if h == 'right':
        vs = [_eval(c, a) for a in expr[1:]]
        c.turtle_right(float(vs[0]) if vs else 1); return
    if h == 'up':    c.turtle_up(); return
    if h == 'down':  c.turtle_down(); return
    if h == 'goto':
        vs = [_eval(c, a) for a in expr[1:]]
        c.turtle_goto(float(vs[0]) if len(vs) >= 1 else None,
                       float(vs[1]) if len(vs) >= 2 else None); return
    if h == 'face':
        vs = [_eval(c, a) for a in expr[1:]]
        c.turtle_face(float(vs[0]) if vs else 0); return

    # --- control flow ---
    if h == 'def' or h == 'now':
        c.env[expr[1]] = _eval(c, expr[2]); return c.env[expr[1]]
    if h == 'if':
        cond = _eval(c, expr[1])
        if cond and cond != 0: return _eval(c, expr[2])
        elif len(expr) > 3: return _eval(c, expr[3])
        return
    if h in ('repeat', 'bunch'):
        count = min(int(_eval(c, expr[1])), 10000)
        if len(expr) >= 4 and isinstance(expr[2], str):
            var, bodies = expr[2], expr[3:]
            for i in range(count):
                c.env[var] = i
                for body in bodies: _eval(c, body)
        elif len(expr) >= 3:
            bodies = expr[2:]
            for _ in range(count):
                for body in bodies: _eval(c, body)
        return
    if h == 'later':
        name = expr[1]
        params = expr[2] if isinstance(expr[2], list) else [expr[2]]
        c.fns[name] = (params, expr[3:])
        return
    if h == 'do':
        r = None
        for sub in expr[1:]: r = _eval(c, sub)
        return r

    # --- user function call ---
    if isinstance(h, str) and h in c.fns:
        params, body = c.fns[h]
        args = [_eval(c, a) for a in expr[1:]]
        old = dict(c.env)
        for p, a in zip(params, args): c.env[p] = a
        r = None
        for b in body: r = _eval(c, b)
        c.env = old
        return r

    # fallback
    results = [_eval(c, a) for a in expr]
    return results[-1] if results else None


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

def kidlisp(source, w=160, h=120, scale=3):
    """Render a KidLisp program and return a scaled PIL Image.

    Args:
        source: KidLisp source code (s-expr or comma syntax).
        w, h:   Canvas size in pixels (before scaling).
        scale:  Integer upscale factor (NEAREST for pixel-art look).

    Returns:
        PIL.Image.Image (RGBA).
    """
    c = _KLCanvas(w, h)
    try:
        for expr in _parse_program(source):
            _eval(c, expr)
    except Exception as e:
        c.draw.text((4, 4), f"ERR: {e}", fill=(255, 0, 0, 255))
    return c.img.resize((w * scale, h * scale), Image.NEAREST)


def show_card(name, source, w=160, h=120, scale=3):
    """Render a KidLisp program and display it inline as a card.

    Args:
        name:   Card title shown below the image.
        source: KidLisp source code.
        w, h:   Canvas size in pixels.
        scale:  Upscale factor.
    """
    img = kidlisp(source, w, h, scale)
    buf = io.BytesIO()
    img.save(buf, format='PNG')
    b64 = base64.b64encode(buf.getvalue()).decode()
    display(HTML(f'''
    <div style="display:inline-block; margin:10px; vertical-align:top; max-width:{w*scale+24}px;">
      <div style="background:#111; border-radius:12px; padding:12px; border:1px solid #333;">
        <div style="color:#eee; font-family:monospace; font-size:14px; font-weight:bold; margin-bottom:8px;">{name}</div>
        <img src="data:image/png;base64,{b64}" style="border-radius:8px; display:block;" />
        <pre style="color:#9d9; font-family:monospace; font-size:13px; margin:10px 0 0 0; white-space:pre-wrap; line-height:1.5;">{source}</pre>
      </div>
    </div>'''))
