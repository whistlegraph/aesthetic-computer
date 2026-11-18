# liquid-hand.py — a capture plays on an unbranded kid-proof tablet while a
# hand made of liquid metal reaches in and does the touches, headless:
#   blender -b --gpu-backend vulkan -P liquid-hand.py -- --frames DIR --count N
#       --events FILE --out DIR --hdri FILE [--still F] [--engine eevee|cycles]
#
# The tablet is our own model: a black slab in a thick rounded bumper, no
# logos and no detail borrowed from a real product. The screen is pure
# emission, so AC's colours come out as captured.
#
# The hand is a family of metaballs: a palm, an arm that runs out of frame,
# and five fingers that are each a chain of balls along a curve from the
# palm to a fingertip. Fingertips follow the events file, which is measured
# from the capture itself (when and where each key lights), so a tip lands
# on a key as the key lights. Typed letters have no key on screen; fingers
# dip in the air for those. Units are meters. Screen x,y are 0..1 from the
# top-left of the capture.

import bpy, bmesh, math, sys, argparse, os, json, time
from mathutils import Vector

argv = sys.argv[sys.argv.index("--") + 1:] if "--" in sys.argv else []
ap = argparse.ArgumentParser()
ap.add_argument("--frames", required=True)        # dir of 00001.png …
ap.add_argument("--count", type=int, required=True)
ap.add_argument("--events", required=True)
ap.add_argument("--out", required=True)
ap.add_argument("--hdri", required=True)
ap.add_argument("--engine", default="eevee")
ap.add_argument("--fps", type=int, default=30)
ap.add_argument("--width", type=int, default=1080)
ap.add_argument("--height", type=int, default=1920)
ap.add_argument("--aspect", type=float, default=1080 / 1920)  # capture w/h
ap.add_argument("--still", type=int, default=0)
ap.add_argument("--bumper", default="0.62,0.63,0.66")   # frame metal          # rgb 0..1
ap.add_argument("--liquid", default="0.90,0.95,1.0")
ap.add_argument("--floor", default="0.055,0.03,0.12")
args = ap.parse_args(argv)
rgb = lambda s: tuple(float(v) for v in s.split(",")) + (1.0,)
EV = json.load(open(args.events))
FPS = args.fps

bpy.ops.wm.read_factory_settings(use_empty=True)
scene = bpy.context.scene
scene.unit_settings.system = "METRIC"
scene.render.fps = FPS
scene.frame_start, scene.frame_end = 1, args.count
scene.render.resolution_x, scene.render.resolution_y = args.width, args.height
scene.view_settings.view_transform = "Standard"
scene.view_settings.look = "None"

def material(name, **inputs):
    m = bpy.data.materials.new(name); m.use_nodes = True
    bsdf = m.node_tree.nodes["Principled BSDF"]
    for k, v in inputs.items(): bsdf.inputs[k].default_value = v
    return m

def link(ob):
    scene.collection.objects.link(ob); return ob

def prism(name, w, h, r, z0, z1, seg=14):
    # A w×h rounded rectangle centred on the origin, extruded from z0 to z1.
    pts = []
    for cx, cy, a0 in ((w / 2 - r, h / 2 - r, 0), (-w / 2 + r, h / 2 - r, 90),
                       (-w / 2 + r, -h / 2 + r, 180), (w / 2 - r, -h / 2 + r, 270)):
        for i in range(seg + 1):
            a = math.radians(a0 + 90 * i / seg)
            pts.append((cx + r * math.cos(a), cy + r * math.sin(a)))
    bm = bmesh.new()
    bot = [bm.verts.new((x, y, z0)) for x, y in pts]
    top = [bm.verts.new((x, y, z1)) for x, y in pts]
    bm.faces.new(bot[::-1]); bm.faces.new(top)
    n = len(pts)
    for i in range(n):
        bm.faces.new((bot[i], bot[(i + 1) % n], top[(i + 1) % n], top[i]))
    bm.normal_update()
    me = bpy.data.meshes.new(name); bm.to_mesh(me); bm.free()
    for p in me.polygons: p.use_smooth = len(p.vertices) == 4
    return link(bpy.data.objects.new(name, me))

# ── world ────────────────────────────────────────────────────────────────
world = bpy.data.worlds.new("room"); scene.world = world; world.use_nodes = True
wn, wl = world.node_tree.nodes, world.node_tree.links
env = wn.new("ShaderNodeTexEnvironment"); env.image = bpy.data.images.load(args.hdri)
mapping = wn.new("ShaderNodeMapping"); coord = wn.new("ShaderNodeTexCoord")
mapping.inputs["Rotation"].default_value[2] = math.radians(200)
wl.new(coord.outputs["Generated"], mapping.inputs["Vector"])
wl.new(mapping.outputs["Vector"], env.inputs["Vector"])
wl.new(env.outputs["Color"], wn["Background"].inputs["Color"])
wn["Background"].inputs["Strength"].default_value = 0.42

# ── tablet ───────────────────────────────────────────────────────────────
# A plain slate: black glass front with an even bezel, in a thin metal frame.
SW = 0.100; SH = SW / args.aspect            # screen
BEZ = 0.0075                                 # black border around the screen
EDGE = 0.0016                                # metal frame showing around the glass
BODY_T = 0.0085
bw, bh = SW + 2 * BEZ, SH + 2 * BEZ
frame = prism("frame", bw + 2 * EDGE, bh + 2 * EDGE, 0.0125, 0.0, BODY_T)
fb = frame.modifiers.new("soft", "BEVEL"); fb.width = 0.0012; fb.segments = 5; fb.limit_method = "ANGLE"
frame.data.materials.append(material("frame", **{"Base Color": rgb(args.bumper), "Metallic": 1.0, "Roughness": 0.32}))
glass = prism("glass", bw, bh, 0.0110, BODY_T - 0.0004, BODY_T + 0.0003)
glass.data.materials.append(material("glass", **{"Base Color": (0.006, 0.006, 0.008, 1), "Roughness": 0.12}))
SCREEN_Z = BODY_T + 0.0005
RIM = EDGE

me = bpy.data.meshes.new("screen")
me.from_pydata([(-SW / 2, -SH / 2, SCREEN_Z), (SW / 2, -SH / 2, SCREEN_Z),
                (SW / 2, SH / 2, SCREEN_Z), (-SW / 2, SH / 2, SCREEN_Z)], [], [(0, 1, 2, 3)])
uv = me.uv_layers.new(name="uv")
for i, co in enumerate(((0, 0), (1, 0), (1, 1), (0, 1))): uv.data[i].uv = co
screen = link(bpy.data.objects.new("screen", me))
sm = bpy.data.materials.new("screen"); sm.use_nodes = True
nodes, links = sm.node_tree.nodes, sm.node_tree.links
bsdf = nodes["Principled BSDF"]
bsdf.inputs["Base Color"].default_value = (0, 0, 0, 1)
bsdf.inputs["Roughness"].default_value = 0.18
bsdf.inputs["Coat Weight"].default_value = 0.08
bsdf.inputs["Coat Roughness"].default_value = 0.04
bsdf.inputs["Emission Strength"].default_value = 1.0
tex = nodes.new("ShaderNodeTexImage")
img = bpy.data.images.load(os.path.join(args.frames, "00001.png"))
img.source = "SEQUENCE"; img.colorspace_settings.name = "sRGB"
tex.image = img; tex.interpolation = "Closest"
tex.image_user.frame_duration = args.count
tex.image_user.frame_start = 1; tex.image_user.frame_offset = 0
tex.image_user.use_auto_refresh = True
links.new(tex.outputs["Color"], bsdf.inputs["Emission Color"])
me.materials.append(sm)

# ── ground ───────────────────────────────────────────────────────────────
bpy.ops.mesh.primitive_plane_add(size=6, location=(0, 0, 0))
bpy.context.object.data.materials.append(material("floor", **{"Base Color": rgb(args.floor), "Roughness": 0.42}))

# ── lights ───────────────────────────────────────────────────────────────
def area(name, loc, size, watts, color=(1, 1, 1)):
    d = bpy.data.lights.new(name, "AREA"); d.size = size; d.energy = watts; d.color = color
    ob = link(bpy.data.objects.new(name, d)); ob.location = loc
    ob.rotation_euler = (Vector((0, 0, 0)) - Vector(loc)).to_track_quat("-Z", "Y").to_euler()
    return ob
area("key", (-0.35, 0.25, 0.55), 0.5, 6)
area("rim", (0.40, -0.20, 0.30), 0.3, 3.5, (1.0, 0.75, 0.95))

# ── the touch track ──────────────────────────────────────────────────────
def world_xy(x, y):
    return ((x - 0.5) * SW, (0.5 - y) * SH)

touches = []   # t (arrive), up, X, Y, air
tap = EV.get("tap")
if tap: touches.append((tap["t"], tap["t"] + 0.16, *world_xy(tap["x"], tap["y"]), False))
for i, t in enumerate(EV.get("letters", [])):
    touches.append((t, t + 0.09, None, None, True))
if "enter" in EV: touches.append((EV["enter"], EV["enter"] + 0.14, None, None, True))
for k in EV.get("keys", []):
    touches.append((k["t"] - 0.03, max(k["up"] - 0.03, k["t"] + 0.08), *world_xy(k["x"], k["y"]), False))
touches.sort(key=lambda r: r[0])

PALM_Y = -SH / 2 - RIM - 0.082
PALM_Z = 0.048
FINGERS = [(-0.052, 0.018), (-0.030, 0.064), (-0.009, 0.074), (0.013, 0.068), (0.034, 0.052)]  # rest tip offset from palm (x, y); 0 = thumb
REST_Z = 0.046
NB = 15          # balls per finger

def smooth(u):
    u = max(0.0, min(1.0, u)); return u * u * (3 - 2 * u)

def palm_x(t):
    # The palm leans toward the keys being played, about half the way.
    num = den = 0.0
    for (ta, up, X, Y, air) in touches:
        if air or X is None: continue
        w = math.exp(-((ta - t) / 0.55) ** 2)
        num += w * X; den += w
    return 0.55 * (num / den) if den > 1e-6 else 0.0

def entrance(t):
    # The hand pours in from below the frame over the first second.
    return -0.26 * (1 - smooth((t - 0.15) / 1.0))

def palm(t):
    return Vector((palm_x(t), PALM_Y + entrance(t) + 0.004 * math.sin(t * 1.7), PALM_Z + 0.004 * math.sin(t * 2.3 + 1)))

def rest_tip(i, t):
    p = palm(t); ox, oy = FINGERS[i]
    return Vector((p.x + ox, p.y + oy + 0.004 * math.sin(t * 2.1 + i), REST_Z + 0.006 * math.sin(t * 2.9 + i * 1.3)))

# Give each touch to the finger whose resting tip is nearest in x and that
# is free; air dips walk across the fingers so typing looks like drumming.
plans = [[] for _ in FINGERS]      # per finger: (t, up, target or None)
busy = [-1.0] * len(FINGERS); air_i = 1
for (ta, up, X, Y, air) in touches:
    if air:
        i = air_i; air_i = air_i % 4 + 1            # index..pinky, never the thumb
        plans[i].append((ta, up, None)); busy[i] = up + 0.12
        continue
    px = palm_x(ta)
    order = sorted(range(1, len(FINGERS)), key=lambda i: abs(px + FINGERS[i][0] - X))
    i = next((i for i in order if busy[i] <= ta - 0.14), order[0])
    plans[i].append((ta, up, Vector((X, Y, SCREEN_Z + 0.0075))))
    busy[i] = up + 0.10

def tip(i, t):
    r = rest_tip(i, t)
    pos = r
    for (ta, up, target) in plans[i]:
        if t < ta - 0.20 or t > up + 0.38: continue
        if target is None:                           # a dip in the air
            target = Vector((r.x, r.y + 0.012, SCREEN_Z + 0.022))
        hover = target + Vector((0, -0.006, 0.030))
        if t < ta - 0.07:   pos = r.lerp(hover, smooth((t - (ta - 0.20)) / 0.13))
        elif t < ta:        pos = hover.lerp(target, smooth((t - (ta - 0.07)) / 0.07))
        elif t <= up:       pos = target
        elif t < up + 0.10: pos = target.lerp(hover, smooth((t - up) / 0.10))
        else:               pos = hover.lerp(r, smooth((t - up - 0.10) / 0.28))
        break
    return pos

def finger_balls(i, t):
    p = palm(t); ox, oy = FINGERS[i]
    base = Vector((p.x + ox * 0.62, p.y + 0.016 + (0.0 if i else -0.014), p.z - 0.003))
    end = tip(i, t)
    mid = (base + end) / 2 + Vector((0, 0, 0.018 + 0.14 * (end - base).length))
    mid += Vector((0.003 * math.sin(t * 3.1 + i * 2), 0, 0.003 * math.sin(t * 4.3 + i)))
    out = []
    for j in range(NB):
        s = (j + 1) / NB
        out.append((1 - s) ** 2 * base + 2 * (1 - s) * s * mid + s * s * end)
    return out

# ── the hand: one metaball family ────────────────────────────────────────
VIS = 0.396     # a lone ball's surface sits at this fraction of its radius (threshold 0.6)
# Clear liquid: you see the screen through it, bent. A faint tint and a
# sharp coat keep its outline readable against a dark screen.
liq = material("liquid", **{"Base Color": rgb(args.liquid), "Metallic": 0.0, "Roughness": 0.02,
                            "IOR": 1.36, "Transmission Weight": 1.0, "Coat Weight": 0.6, "Coat Roughness": 0.02})
for attr, val in (("use_raytrace_refraction", True), ("surface_render_method", "DITHERED"),
                  ("use_transparency_overlap", False), ("thickness_mode", "SPHERE")):
    if hasattr(liq, attr): setattr(liq, attr, val)
balls = []      # (object, fn(t) -> Vector)
def ball(radius, fn):
    mb = bpy.data.metaballs.new("Liq"); mb.resolution = 0.0014; mb.render_resolution = 0.0014; mb.threshold = 0.6
    el = mb.elements.new(); el.radius = radius / VIS
    mb.materials.append(liq)
    ob = link(bpy.data.objects.new("Liq", mb)); balls.append((ob, fn))

ball(0.022, lambda t: palm(t))
ball(0.017, lambda t: palm(t) + Vector((-0.021, -0.004, -0.003)))
ball(0.017, lambda t: palm(t) + Vector((0.021, -0.004, -0.003)))
for k in range(1, 8):   # the arm, running down out of frame
    ball(0.019 - 0.001 * k, lambda t, k=k: palm(t) + Vector((0.005 * k * math.sin(t * 1.3 + k), -0.030 * k, -0.003 * k)))
for i in range(len(FINGERS)):
    for j in range(NB):
        rad = 0.0080 - (0.0080 - 0.0060) * j / (NB - 1)
        if i == 0: rad *= 1.12
        ball(rad, lambda t, i=i, j=j: finger_balls(i, t)[j])

frames = [args.still] if args.still else range(1, args.count + 1)
for f in frames:
    t = (f - 1) / FPS
    cache = {}
    for ob, fn in balls:
        ob.location = fn(t)
        if not args.still: ob.keyframe_insert("location", frame=f)

# ── camera ───────────────────────────────────────────────────────────────
cam_data = bpy.data.cameras.new("cam"); cam_data.lens = 50; cam_data.sensor_width = 36
cam = link(bpy.data.objects.new("cam", cam_data)); scene.camera = cam
aim = link(bpy.data.objects.new("aim", None)); aim.location = (0, -0.036, 0.01)
tr = cam.constraints.new("TRACK_TO"); tr.target = aim; tr.track_axis = "TRACK_NEGATIVE_Z"; tr.up_axis = "UP_Y"
cam.location = (0.025, -0.29, 0.50)
if not args.still:
    cam.keyframe_insert("location", frame=1)
    cam.location = (-0.015, -0.265, 0.465); cam.keyframe_insert("location", frame=args.count)
else:
    u = (args.still - 1) / max(1, args.count - 1)
    cam.location = Vector((0.025, -0.29, 0.50)).lerp(Vector((-0.015, -0.265, 0.465)), u)

# ── render ───────────────────────────────────────────────────────────────
if args.engine == "cycles":
    scene.render.engine = "CYCLES"
    prefs = bpy.context.preferences.addons["cycles"].preferences
    prefs.compute_device_type = "OPTIX"; prefs.get_devices()
    for d in prefs.devices: d.use = d.type == "OPTIX"
    scene.cycles.device = "GPU"; scene.cycles.samples = 64
    scene.cycles.use_adaptive_sampling = True; scene.cycles.use_denoising = True
    scene.cycles.denoiser = "OPTIX"; scene.render.use_persistent_data = True
    scene.cycles.max_bounces = 6
else:
    scene.render.engine = "BLENDER_EEVEE"
    scene.eevee.taa_render_samples = 32
    scene.eevee.use_raytracing = True

scene.render.image_settings.file_format = "PNG"
scene.render.image_settings.color_mode = "RGB"
os.makedirs(args.out, exist_ok=True)
t0 = time.time()
if args.still:
    scene.frame_set(args.still)
    for ob, fn in balls: ob.location = fn((args.still - 1) / FPS)
    scene.render.filepath = os.path.join(args.out, f"still-{args.still:05d}.png")
    bpy.ops.render.render(write_still=True)
else:
    scene.render.filepath = os.path.join(args.out, "#####")
    bpy.ops.render.render(animation=True)
print(f"SCENE_DONE {args.engine} {time.time() - t0:.1f}s")
