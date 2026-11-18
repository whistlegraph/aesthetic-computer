# liquid-hand-rig.py — a capture plays on a plain slate while a clear liquid
# hand on a real hand rig reaches in from the lower right and does the
# touches, headless:
#   blender -b --gpu-backend vulkan -P liquid-hand-rig.py -- --frames DIR --count N
#       --events FILE --hand right.glb --out DIR --hdri FILE [--still F]
#
# The rig is the WebXR Input Profiles "generic-hand" (MIT, (c) 2019 Amazon):
# one skinned mesh on the 25 WebXR hand joints, which are all root bones, so
# each joint is posed by giving it a rigid transform of its bind frame.
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
ap.add_argument("--hand", required=True)             # generic-hand right.glb
ap.add_argument("--engine", default="eevee")
ap.add_argument("--fps", type=int, default=30)
ap.add_argument("--width", type=int, default=1080)
ap.add_argument("--height", type=int, default=1920)
ap.add_argument("--aspect", type=float, default=1080 / 1920)  # capture w/h
ap.add_argument("--still", type=int, default=0)
ap.add_argument("--bumper", default="0.62,0.63,0.66")   # frame metal          # rgb 0..1
ap.add_argument("--liquid", default="0.55,0.86,1.0")
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
SW = 0.160; SH = SW / args.aspect            # screen
BEZ = 0.0105                                 # black border around the screen
EDGE = 0.0016                                # metal frame showing around the glass
BODY_T = 0.0085
bw, bh = SW + 2 * BEZ, SH + 2 * BEZ
frame = prism("frame", bw + 2 * EDGE, bh + 2 * EDGE, 0.0185, 0.0, BODY_T)
fb = frame.modifiers.new("soft", "BEVEL"); fb.width = 0.0012; fb.segments = 5; fb.limit_method = "ANGLE"
frame.data.materials.append(material("frame", **{"Base Color": rgb(args.bumper), "Metallic": 1.0, "Roughness": 0.32}))
glass = prism("glass", bw, bh, 0.0170, BODY_T - 0.0004, BODY_T + 0.0003)
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
from mathutils import Matrix
def world_xy(x, y):
    return ((x - 0.5) * SW, (0.5 - y) * SH)

touches = []   # (arrive, up, X, Y, air)
tap = EV.get("tap")
if tap: touches.append((tap["t"], tap["t"] + 0.16, *world_xy(tap["x"], tap["y"]), False))
for t in EV.get("letters", []): touches.append((t, t + 0.08, None, None, True))
if "enter" in EV: touches.append((EV["enter"], EV["enter"] + 0.13, None, None, True))
for k in EV.get("keys", []):
    touches.append((k["t"] - 0.03, max(k["up"] - 0.03, k["t"] + 0.08), *world_xy(k["x"], k["y"]), False))
touches.sort(key=lambda r: r[0])

def smooth(u):
    u = max(0.0, min(1.0, u)); return u * u * (3 - 2 * u)

HOVER = 0.028                       # fingertip height between touches
TOUCH_Z = SCREEN_Z + 0.0045         # fingertip joint height at contact (the pad has thickness)
HOME = Vector((0.020, -0.050))      # where the finger idles while typing
OFF = Vector((0.27, -0.44))         # off frame, lower right
ELBOW = Vector((0.40, -0.66))       # the forearm pivots about here

# The fingertip path: a list of (time, xy, z) knots, eased between.
knots = [(0.0, OFF, 0.10), (0.35, OFF, 0.10)]
def at(t, xy, z): knots.append((t, Vector(xy), z))
prev_xy = None
for n, (ta, up, X, Y, air) in enumerate(touches):
    xy = Vector((X, Y)) if not air else Vector((HOME.x + 0.004 * ((n * 7) % 5 - 2), HOME.y + 0.003 * ((n * 3) % 4 - 1.5)))
    down = TOUCH_Z if not air else SCREEN_Z + 0.014
    last_t = knots[-1][0]
    arrive = max(last_t + 0.02, ta - 0.085)          # be above the spot just before pressing
    at(arrive, xy, HOVER if not air else 0.024)
    at(max(arrive + 0.02, ta), xy, down)
    at(max(up, ta + 0.05), xy, down)
    lift_t = max(up, ta + 0.05) + 0.10
    nxt = touches[n + 1][0] if n + 1 < len(touches) else None
    if nxt is None or nxt - lift_t > 0.14: at(lift_t, xy, HOVER if not air else 0.024)
    if not air and (nxt is None or nxt - lift_t > 1.0): at(lift_t + 0.45, HOME, HOVER + 0.012)   # rest back
knots.sort(key=lambda k: k[0])

def tip_at(t):
    if t <= knots[0][0]: k = knots[0]; return Vector((k[1].x, k[1].y, k[2]))
    for a, b in zip(knots, knots[1:]):
        if a[0] <= t <= b[0]:
            u = smooth((t - a[0]) / max(1e-6, b[0] - a[0]))
            xy = a[1].lerp(b[1], u); z = a[2] + (b[2] - a[2]) * u
            # travelling between far-apart spots arcs up a little
            z += 0.012 * math.sin(math.pi * u) * min(1.0, (a[1] - b[1]).length / 0.05)
            return Vector((xy.x, xy.y, z))
    k = knots[-1]; return Vector((k[1].x, k[1].y, k[2]))

def press_at(t):
    # 0..1: how pressed the finger is (drives a little extra flex)
    z = tip_at(t).z
    return max(0.0, min(1.0, (HOVER - z) / (HOVER - TOUCH_Z)))

# ── the hand: generic-hand rig, posed joint by joint ─────────────────────
before = set(bpy.data.objects)
bpy.ops.import_scene.gltf(filepath=args.hand)
new = [o for o in bpy.data.objects if o not in before]
arm = next(o for o in new if o.type == "ARMATURE")
skin = next(o for o in new if o.type == "MESH" and o.parent == arm)
for o in new:
    if o not in (arm, skin): bpy.data.objects.remove(o, do_unlink=True)
arm.location = (0, 0, 0); arm.rotation_mode = "QUATERNION"; arm.rotation_quaternion = (1, 0, 0, 0); arm.scale = (1, 1, 1)
bpy.context.view_layer.update()
A = arm.matrix_world.copy()
REST = {b.name: (A @ b.matrix_local) for b in arm.data.bones}          # bind frames, world
HEAD = {n: m.translation.copy() for n, m in REST.items()}
for pb in arm.pose.bones: pb.rotation_mode = "QUATERNION"

FINGER = {f: [f"{f}-metacarpal", f"{f}-phalanx-proximal", f"{f}-phalanx-intermediate", f"{f}-phalanx-distal", f"{f}-tip"]
          for f in ("index-finger", "middle-finger", "ring-finger", "pinky-finger")}
THUMB = ["thumb-metacarpal", "thumb-phalanx-proximal", "thumb-phalanx-distal", "thumb-tip"]
F_B = (HEAD["middle-finger-tip"] - HEAD["wrist"]).normalized()            # fingers point this way in bind space
side = (HEAD["index-finger-phalanx-proximal"] - HEAD["pinky-finger-phalanx-proximal"]).normalized()
N_B = F_B.cross(side).normalized()                                        # palm normal candidate
if (HEAD["thumb-tip"] - HEAD["index-finger-phalanx-proximal"]).dot(N_B) < 0: N_B = -N_B   # thumb sits on the palm side
S_B = N_B.cross(F_B).normalized()

def rot_about(pivot, axis, deg):
    return Matrix.Translation(pivot) @ Matrix.Rotation(math.radians(deg), 4, axis) @ Matrix.Translation(-pivot)

def flex_axis(chain):
    d = (HEAD[chain[-1]] - HEAD[chain[1]]).normalized()
    ax = d.cross(N_B).normalized()
    # choose the sign that carries the tip toward the palm
    test = rot_about(HEAD[chain[1]], ax, 20) @ HEAD[chain[-1]]
    return ax if (test - HEAD[chain[-1]]).dot(N_B) > 0 else -ax

AX = {f: flex_axis(c) for f, c in FINGER.items()}
AX_T = flex_axis(["x", THUMB[0], THUMB[1], THUMB[2], THUMB[3]][1:]) if False else None
td = (HEAD[THUMB[-1]] - HEAD[THUMB[0]]).normalized()
AX_T = td.cross(F_B).normalized()
if ((rot_about(HEAD[THUMB[0]], AX_T, 20) @ HEAD[THUMB[-1]]) - HEAD[THUMB[-1]]).dot(F_B.cross(td).cross(td)) < 0: pass

def local_pose(curls, thumb):
    # curls: {finger: (mcp, pip, dip)} in degrees. Returns {joint: 4x4 in bind space}.
    X = {"wrist": Matrix.Identity(4)}
    for f, chain in FINGER.items():
        a, b, c = curls[f]; ax = AX[f]
        m = Matrix.Identity(4); X[chain[0]] = m.copy()
        m = m @ rot_about(HEAD[chain[1]], ax, a); X[chain[1]] = m.copy()
        m = m @ rot_about(HEAD[chain[2]], ax, b); X[chain[2]] = m.copy()
        m = m @ rot_about(HEAD[chain[3]], ax, c); X[chain[3]] = m.copy(); X[chain[4]] = m.copy()
    m = rot_about(HEAD[THUMB[0]], F_B, thumb[0]); X[THUMB[0]] = m.copy()
    m = m @ rot_about(HEAD[THUMB[1]], AX_T, thumb[1]); X[THUMB[1]] = m.copy()
    m = m @ rot_about(HEAD[THUMB[2]], AX_T, thumb[2]); X[THUMB[2]] = m.copy(); X[THUMB[3]] = m.copy()
    return X

def pose(t):
    p = press_at(t); tip = tip_at(t)
    breathe = 2.5 * math.sin(t * 2.1)
    curls = {"index-finger": (4 + 8 * p, 7 + 6 * p, 4 + 3 * p),
             "middle-finger": (58 + breathe, 84, 46), "ring-finger": (64 + breathe, 88, 50), "pinky-finger": (66 - breathe, 86, 48)}
    X = local_pose(curls, (-26, 22, 26))
    if os.environ.get("LH_REST"): X = {n: Matrix.Identity(4) for n in X}
    tip_b = X["index-finger-tip"] @ HEAD["index-finger-tip"]
    # the hand reaches from the elbow: fingers point from there toward the target, tilted down onto the glass
    d = (Vector((tip.x, tip.y)) - ELBOW).normalized()
    pitch = math.radians(21 + 6 * p)
    f_s = Vector((d.x * math.cos(pitch), d.y * math.cos(pitch), -math.sin(pitch)))
    down = Vector((0, 0, -1)); n_s = (down - down.dot(f_s) * f_s).normalized()
    roll = Matrix.Rotation(math.radians(-22), 3, f_s); n_s = roll @ n_s
    s_s = n_s.cross(f_s).normalized()
    Rb = Matrix((F_B, N_B, S_B)).transposed(); Rs = Matrix((f_s, n_s, s_s)).transposed()
    R = Rs @ Rb.inverted()
    H = Matrix.Translation(tip - R @ tip_b) @ R.to_4x4()
    return {n: H @ x for n, x in X.items()}, H

AI = A.inverted()
def apply(t, frame=None):
    M, H = pose(t)
    for pb in arm.pose.bones:
        pb.matrix = AI @ M[pb.name] @ REST[pb.name]
        if frame is not None:
            pb.keyframe_insert("location", frame=frame); pb.keyframe_insert("rotation_quaternion", frame=frame)
    return H

# Clear liquid: you see the screen through it, bent.
liq = material("liquid", **{"Base Color": rgb(args.liquid), "Metallic": 0.0, "Roughness": 0.16,
                            "IOR": 1.36, "Transmission Weight": 0.86, "Coat Weight": 0.9, "Coat Roughness": 0.03})
for attr, val in (("use_raytrace_refraction", True), ("surface_render_method", "DITHERED"),
                  ("use_transparency_overlap", False), ("thickness_mode", "SPHERE")):
    if hasattr(liq, attr): setattr(liq, attr, val)
# A soft glow where the surface turns away from the camera: clear in the
# middle, bright at the edges, so the hand reads as a shape over any screen.
ln, ll = liq.node_tree.nodes, liq.node_tree.links
lb = ln["Principled BSDF"]; lb.inputs["IOR"].default_value = 1.12
lw = ln.new("ShaderNodeLayerWeight"); lw.inputs["Blend"].default_value = 0.42
pw = ln.new("ShaderNodeMath"); pw.operation = "POWER"; pw.inputs[1].default_value = 3.0
mu = ln.new("ShaderNodeMath"); mu.operation = "MULTIPLY"; mu.inputs[1].default_value = 0.9
ll.new(lw.outputs["Facing"], pw.inputs[0]); ll.new(pw.outputs[0], mu.inputs[0])
ll.new(mu.outputs[0], lb.inputs["Emission Strength"])
lb.inputs["Emission Color"].default_value = (0.62, 0.90, 1.0, 1)
if os.environ.get("LH_DEBUG"): liq = material("clay", **{"Base Color": (0.9, 0.55, 0.35, 1), "Roughness": 0.6})
skin.data.materials.clear(); skin.data.materials.append(liq)
print("RIG F_B", [round(v, 2) for v in F_B], "N_B", [round(v, 2) for v in N_B], "AX", {k: [round(v, 2) for v in a] for k, a in AX.items()}, "AX_T", [round(v, 2) for v in AX_T])
print("RIG skin verts", len(skin.data.vertices), "groups", [g.name for g in skin.vertex_groups][:30], "mods", [m.type for m in skin.modifiers])
for p in skin.data.polygons: p.use_smooth = True
sub = skin.modifiers.new("smooth", "SUBSURF"); sub.levels = 2; sub.render_levels = 2

# A liquid forearm, so the wrist does not end in a hole: a tapered tube from
# just inside the wrist back toward the elbow, out of frame.
bpy.ops.mesh.primitive_cone_add(vertices=48, radius1=0.0245, radius2=0.0015, depth=0.26, end_fill_type="NOTHING", location=(0, 0, 0))
fore = bpy.context.object; fore.name = "forearm"
for v in fore.data.vertices: v.co.z += 0.13                    # base at the origin, running along +z
fore.data.materials.append(liq)
for p in fore.data.polygons: p.use_smooth = True
fore.rotation_mode = "QUATERNION"
ELBOW3 = Vector((ELBOW.x, ELBOW.y, 0.17))
def place_forearm(H, frame=None):
    w = H @ HEAD["wrist"]; back = (ELBOW3 - w).normalized()
    fore.location = w - back * 0.012
    fore.rotation_quaternion = back.to_track_quat("Z", "Y")
    fore.scale = (1.0, 0.72, 1.0)
    if frame is not None:
        fore.keyframe_insert("location", frame=frame); fore.keyframe_insert("rotation_quaternion", frame=frame)

frames = [args.still] if args.still else range(1, args.count + 1)
for f in frames:
    place_forearm(apply((f - 1) / FPS, None if args.still else f), None if args.still else f)
if args.still:
    bpy.context.view_layer.update()
    tj = arm.matrix_world @ arm.pose.bones["index-finger-tip"].matrix.translation
    print("TIP joint", [round(v, 4) for v in tj], "wanted", [round(v, 4) for v in tip_at((args.still - 1) / FPS)])
balls = []

# ── camera ───────────────────────────────────────────────────────────────
cam_data = bpy.data.cameras.new("cam"); cam_data.lens = 50; cam_data.sensor_width = 36
cam = link(bpy.data.objects.new("cam", cam_data)); scene.camera = cam
aim = link(bpy.data.objects.new("aim", None)); aim.location = (0.018, -0.022, 0.01)
tr = cam.constraints.new("TRACK_TO"); tr.target = aim; tr.track_axis = "TRACK_NEGATIVE_Z"; tr.up_axis = "UP_Y"
cam.location = (0.03, -0.315, 0.555)
if not args.still:
    cam.keyframe_insert("location", frame=1)
    cam.location = (-0.012, -0.295, 0.525); cam.keyframe_insert("location", frame=args.count)
else:
    u = (args.still - 1) / max(1, args.count - 1)
    cam.location = Vector((0.03, -0.315, 0.555)).lerp(Vector((-0.012, -0.295, 0.525)), u)

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
