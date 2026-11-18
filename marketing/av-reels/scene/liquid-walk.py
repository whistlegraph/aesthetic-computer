# liquid-walk.py — walking down a street with the device in hand, headless:
#   blender -b --gpu-backend vulkan -P liquid-walk.py -- --frames DIR --count N
#       --events FILE --hand right.glb --hand-left left.glb --out DIR --hdri FILE [--still F]
#
# The view is your own, looking down at a plain slate held in a liquid left
# hand while a liquid right hand does the touches. The pavement passes
# underneath at walking pace and everything bobs with each step.
#
# The slate, both hands and the camera share one frame (the slate's: screen
# in the xy plane, z toward your face). The street is built in its own frame
# and moved past: it scrolls backward at walking speed and carries the
# inverse of the head's bob, so nothing on the screen changes and the touch
# track still lands on the keys.
#
# Hands are the WebXR Input Profiles "generic-hand" rigs (MIT, (c) 2019
# Amazon): one skinned mesh on 25 joints that are all root bones, so a joint
# is posed by giving it a rigid transform of its bind frame. Units: meters.

import bpy, bmesh, math, sys, argparse, os, json, time, random
from mathutils import Vector, Matrix

argv = sys.argv[sys.argv.index("--") + 1:] if "--" in sys.argv else []
ap = argparse.ArgumentParser()
ap.add_argument("--frames", required=True)
ap.add_argument("--count", type=int, required=True)
ap.add_argument("--events", required=True)
ap.add_argument("--out", required=True)
ap.add_argument("--hdri", required=True)
ap.add_argument("--hand", required=True)
ap.add_argument("--hand-left", required=True)
ap.add_argument("--engine", default="eevee")
ap.add_argument("--fps", type=int, default=30)
ap.add_argument("--width", type=int, default=1080)
ap.add_argument("--height", type=int, default=1920)
ap.add_argument("--aspect", type=float, default=1080 / 1920)
ap.add_argument("--still", type=int, default=0)
ap.add_argument("--frame-metal", default="0.62,0.63,0.66")
ap.add_argument("--liquid", default="0.55,0.86,1.0")
ap.add_argument("--pace", type=float, default=0.9)       # meters per second
ap.add_argument("--ground", default="street")            # street | grass
ap.add_argument("--scratch", action="store_true")        # quick look: half size, every other frame, few samples, no blur
ap.add_argument("--steps", type=float, default=1.5)      # steps per second
ap.add_argument("--poly", type=float, default=0.0)        # low-poly hands: keep this fraction of the faces (0 = smooth)
ap.add_argument("--step-phase", type=float, default=0.0)  # time of a footfall, so steps can keep time with the music
args = ap.parse_args(argv)
rgb = lambda s: tuple(float(v) for v in s.split(",")) + (1.0,)
EV = json.load(open(args.events))
FPS = args.fps
CLAY = bool(os.environ.get("LH_DEBUG"))

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
    for i in range(n): bm.faces.new((bot[i], bot[(i + 1) % n], top[(i + 1) % n], top[i]))
    bm.normal_update()
    me = bpy.data.meshes.new(name); bm.to_mesh(me); bm.free()
    for p in me.polygons: p.use_smooth = len(p.vertices) == 4
    return link(bpy.data.objects.new(name, me))

def smooth(u):
    u = max(0.0, min(1.0, u)); return u * u * (3 - 2 * u)

# ── world: outdoor light ─────────────────────────────────────────────────
world = bpy.data.worlds.new("street"); scene.world = world; world.use_nodes = True
wn, wl = world.node_tree.nodes, world.node_tree.links
env = wn.new("ShaderNodeTexEnvironment"); env.image = bpy.data.images.load(args.hdri)
mapping = wn.new("ShaderNodeMapping"); coord = wn.new("ShaderNodeTexCoord")
mapping.inputs["Rotation"].default_value[2] = math.radians(140)
wl.new(coord.outputs["Generated"], mapping.inputs["Vector"])
wl.new(mapping.outputs["Vector"], env.inputs["Vector"])
wl.new(env.outputs["Color"], wn["Background"].inputs["Color"])
wn["Background"].inputs["Strength"].default_value = 0.85

# Everything you carry hangs off this, so a little handheld sway moves the
# slate and both hands together.
held = link(bpy.data.objects.new("held", None)); held.rotation_mode = "XYZ"

# ── the slate ────────────────────────────────────────────────────────────
SW = 0.160; SH = SW / args.aspect
BEZ = 0.0105; EDGE = 0.0016; BODY_T = 0.0085
bw, bh = SW + 2 * BEZ, SH + 2 * BEZ
frame = prism("frame", bw + 2 * EDGE, bh + 2 * EDGE, 0.0185, 0.0, BODY_T)
fb = frame.modifiers.new("soft", "BEVEL"); fb.width = 0.0012; fb.segments = 5; fb.limit_method = "ANGLE"
frame.data.materials.append(material("frame", **{"Base Color": rgb(args.frame_metal), "Metallic": 1.0, "Roughness": 0.32}))
glass = prism("glass", bw, bh, 0.0170, BODY_T - 0.0004, BODY_T + 0.0003)
glass.data.materials.append(material("glass", **{"Base Color": (0.006, 0.006, 0.008, 1), "Roughness": 0.12}))
SCREEN_Z = BODY_T + 0.0005
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
bsdf.inputs["Emission Strength"].default_value = 1.6      # daylight: the screen has to hold its own
tex = nodes.new("ShaderNodeTexImage")
img = bpy.data.images.load(os.path.join(args.frames, "00001.png"))
img.source = "SEQUENCE"; img.colorspace_settings.name = "sRGB"
tex.image = img; tex.interpolation = "Closest"
tex.image_user.frame_duration = args.count
tex.image_user.frame_start = 1; tex.image_user.frame_offset = 0
tex.image_user.use_auto_refresh = True
links.new(tex.outputs["Color"], bsdf.inputs["Emission Color"])
me.materials.append(sm)
for ob in (frame, glass, screen): ob.parent = held

# ── the street, in its own frame ─────────────────────────────────────────
# You hold the slate tilted up toward your face, so "up" in the slate's
# frame leans back toward you; "forward" is the way you walk.
TILT = math.radians(36)
UP = Vector((0, -math.sin(TILT), math.cos(TILT)))
FWD = Vector((0, math.cos(TILT), math.sin(TILT)))
RIGHT = Vector((1, 0, 0))
DROP = 1.02                                              # slate above the pavement
street = link(bpy.data.objects.new("street", None))
ROT = Matrix((RIGHT, FWD, UP)).transposed().to_4x4()     # street x,y,z -> slate frame
LEN = 80.0

def ground(name, x0, x1, z, mat):
    m = bpy.data.meshes.new(name)
    m.from_pydata([(x0, -6, z), (x1, -6, z), (x1, LEN, z), (x0, LEN, z)], [], [(0, 1, 2, 3)])
    ob = link(bpy.data.objects.new(name, m)); ob.parent = street; m.materials.append(mat); return ob

def tex_mat(name, build):
    m = bpy.data.materials.new(name); m.use_nodes = True
    n, l = m.node_tree.nodes, m.node_tree.links; b = n["Principled BSDF"]
    tc = n.new("ShaderNodeTexCoord"); build(n, l, b, tc); return m

def pavement(n, l, b, tc):
    # concrete flags: big slabs with dark joints, speckled and stained
    br = n.new("ShaderNodeTexBrick"); br.inputs["Scale"].default_value = 1.0
    br.inputs["Mortar Size"].default_value = 0.012; br.inputs["Brick Width"].default_value = 0.9; br.inputs["Row Height"].default_value = 0.6
    br.inputs["Color1"].default_value = (0.46, 0.45, 0.43, 1); br.inputs["Color2"].default_value = (0.40, 0.39, 0.38, 1)
    br.inputs["Mortar"].default_value = (0.10, 0.10, 0.10, 1); br.offset = 0.5
    no = n.new("ShaderNodeTexNoise"); no.inputs["Scale"].default_value = 55; no.inputs["Detail"].default_value = 6
    st = n.new("ShaderNodeTexNoise"); st.inputs["Scale"].default_value = 2.2; st.inputs["Detail"].default_value = 3
    mx = n.new("ShaderNodeMixRGB"); mx.blend_type = "MULTIPLY"; mx.inputs["Fac"].default_value = 0.55
    mx2 = n.new("ShaderNodeMixRGB"); mx2.blend_type = "MULTIPLY"; mx2.inputs["Fac"].default_value = 0.45
    for t in (br, no, st): l.new(tc.outputs["Object"], t.inputs["Vector"])
    l.new(br.outputs["Color"], mx.inputs["Color1"]); l.new(no.outputs["Fac"], mx.inputs["Color2"])
    l.new(mx.outputs["Color"], mx2.inputs["Color1"]); l.new(st.outputs["Fac"], mx2.inputs["Color2"])
    l.new(mx2.outputs["Color"], b.inputs["Base Color"]); b.inputs["Roughness"].default_value = 0.85

def asphalt(n, l, b, tc):
    no = n.new("ShaderNodeTexNoise"); no.inputs["Scale"].default_value = 140; no.inputs["Detail"].default_value = 8
    ramp = n.new("ShaderNodeValToRGB")
    ramp.color_ramp.elements[0].color = (0.035, 0.035, 0.04, 1); ramp.color_ramp.elements[1].color = (0.16, 0.16, 0.17, 1)
    l.new(tc.outputs["Object"], no.inputs["Vector"]); l.new(no.outputs["Fac"], ramp.inputs["Fac"])
    l.new(ramp.outputs["Color"], b.inputs["Base Color"]); b.inputs["Roughness"].default_value = 0.7

def photo(name, stem, size):
    # scanned surface (Poly Haven, CC0): colour, bumps and roughness, tiled every `size` meters
    m = bpy.data.materials.new(name); m.use_nodes = True
    n, l = m.node_tree.nodes, m.node_tree.links; b = n["Principled BSDF"]
    tc = n.new("ShaderNodeTexCoord"); mp = n.new("ShaderNodeMapping"); mp.inputs["Scale"].default_value = (1 / size, 1 / size, 1)
    l.new(tc.outputs["Object"], mp.inputs["Vector"])
    def img(kind, colorspace):
        t = n.new("ShaderNodeTexImage"); t.image = bpy.data.images.load(f"{stem}_{kind}_2k.jpg"); t.image.colorspace_settings.name = colorspace
        l.new(mp.outputs["Vector"], t.inputs["Vector"]); return t
    l.new(img("diff", "sRGB").outputs["Color"], b.inputs["Base Color"])
    l.new(img("rough", "Non-Color").outputs["Color"], b.inputs["Roughness"])
    nm = n.new("ShaderNodeNormalMap"); nm.inputs["Strength"].default_value = 1.2
    l.new(img("nor", "Non-Color").outputs["Color"], nm.inputs["Color"]); l.new(nm.outputs["Normal"], b.inputs["Normal"])
    return m
TEX = os.path.expanduser("~/.cache/ac-reel-assets/textures")
random.seed(7)
def scatter(n, radius, colors, x0, x1, squash=(0.45, 0.8), sides=7, z=0.002):
    mats = [material(f"bit{len(bpy.data.materials)}", **{"Base Color": c, "Roughness": 0.7}) for c in colors]
    for i in range(n):
        bpy.ops.mesh.primitive_circle_add(vertices=sides, radius=random.uniform(*radius), fill_type="NGON",
            location=(random.uniform(x0, x1), random.uniform(-1.5, LEN - 4), z + 0.0005 * (i % 5)))
        o = bpy.context.object; o.scale = (1, random.uniform(*squash), 1); o.rotation_euler = (0, 0, random.uniform(0, 6.28))
        o.data.materials.append(mats[i % len(mats)]); o.parent = street
if args.ground == "grass":
    # a lawn: scanned grass, with daisies, dandelions, clover patches and bare spots going by
    have = os.path.exists(f"{TEX}/leafy_grass_diff_2k.jpg")
    lawn = photo("lawn", f"{TEX}/leafy_grass", 1.6) if have else material("lawn", **{"Base Color": (0.10, 0.28, 0.06, 1), "Roughness": 0.9})
    ground("lawn", -5.0, 5.0, 0.0, lawn)
    scatter(170, (0.012, 0.022), [(0.95, 0.95, 0.90, 1)], -0.75, 0.75, squash=(0.8, 1.0), sides=9, z=0.012)          # daisies
    scatter(60, (0.014, 0.024), [(0.98, 0.80, 0.06, 1)], -0.75, 0.75, squash=(0.8, 1.0), sides=10, z=0.014)          # dandelions
    scatter(40, (0.06, 0.16), [(0.07, 0.20, 0.05, 1), (0.16, 0.30, 0.08, 1)], -0.8, 0.8, squash=(0.6, 1.0), sides=11) # clover, shade
    scatter(18, (0.05, 0.12), [(0.30, 0.22, 0.13, 1)], -0.8, 0.8, squash=(0.6, 1.0), sides=9)                         # bare earth
    scatter(50, (0.018, 0.04), [(0.62, 0.30, 0.05, 1), (0.75, 0.52, 0.08, 1)], -0.8, 0.8)                             # fallen leaves
else:
    have_tex = os.path.exists(f"{TEX}/concrete_pavement_diff_2k.jpg") and os.path.exists(f"{TEX}/asphalt_02_diff_2k.jpg")
    if have_tex:
        pavement_mat = photo("pavement", f"{TEX}/concrete_pavement", 2.0); road_mat = photo("road", f"{TEX}/asphalt_02", 2.5)
    else:
        pavement_mat = tex_mat("pavement", pavement); road_mat = tex_mat("road", asphalt)
    CURB_X = 0.20                                            # the kerb runs just right of your path
    ground("pavement", -4.0, CURB_X, 0.0, pavement_mat)
    ground("kerb", CURB_X, CURB_X + 0.14, 0.012, material("kerb", **{"Base Color": (0.58, 0.57, 0.54, 1), "Roughness": 0.8}))
    ground("road", CURB_X + 0.14, 5.0, -0.11, road_mat)
    ground("kerbface", CURB_X + 0.139, CURB_X + 0.141, -0.05, material("kerbface", **{"Base Color": (0.30, 0.30, 0.29, 1), "Roughness": 0.9}))
    paint = material("paint", **{"Base Color": (0.85, 0.72, 0.10, 1), "Roughness": 0.6})
    y = -2.0
    while y < LEN - 2:
        m = bpy.data.meshes.new("dash"); x0 = CURB_X + 0.36
        m.from_pydata([(x0, y, -0.108), (x0 + 0.10, y, -0.108), (x0 + 0.10, y + 1.6, -0.108), (x0, y + 1.6, -0.108)], [], [(0, 1, 2, 3)])
        ob = link(bpy.data.objects.new("dash", m)); ob.parent = street; m.materials.append(paint); y += 3.2
    scatter(150, (0.018, 0.045), [(0.62, 0.30, 0.05, 1), (0.75, 0.52, 0.08, 1), (0.36, 0.20, 0.06, 1), (0.50, 0.12, 0.05, 1), (0.30, 0.38, 0.10, 1)], -0.55, CURB_X - 0.03)
    scatter(26, (0.05, 0.16), [(0.17, 0.165, 0.16, 1)], -0.5, CURB_X - 0.08, squash=(0.6, 1.0), sides=24, z=0.001)

def street_matrix(t):
    # walk forward: the street slides back. The head rises and falls once a
    # step and sways once a stride; the street carries the opposite.
    ph = 2 * math.pi * args.steps * (t - args.step_phase)   # 0 at each footfall
    bob = 0.022 * (0.5 - 0.5 * math.cos(ph))             # lowest as the foot lands, up at mid-stance
    sway = 0.016 * math.sin(ph / 2)
    surge = 0.010 * math.sin(ph + 0.6)                   # pace is not perfectly even
    pos = Vector((-sway, -(args.pace * t + surge) + 1.2, -DROP - bob))
    tilt = Matrix.Rotation(math.radians(0.7) * math.sin(ph / 2 + 0.4), 4, "Y") @ Matrix.Rotation(math.radians(0.5) * math.sin(ph + 1.1), 4, "X")
    return ROT @ tilt @ Matrix.Translation(pos)

def held_matrix(t):
    # the slate lags the head a touch on each step
    ph = 2 * math.pi * args.steps * (t - args.step_phase)
    return (Matrix.Translation((0.0025 * math.sin(ph / 2 + 1.0), 0.0015 * math.sin(ph + 0.3), 0.0030 * math.sin(ph + 2.2)))
            @ Matrix.Rotation(math.radians(0.9) * math.sin(ph / 2 + 0.2), 4, "Y") @ Matrix.Rotation(math.radians(0.7) * math.sin(ph + 1.7), 4, "X"))

# ── your legs and feet ───────────────────────────────────────────────────
# Seen from above, below the slate: each foot lands on its footstep, stays
# planted while the ground carries it back, then swings through.
body = link(bpy.data.objects.new("body", None))
STEP_T = 1.0 / args.steps; STRIDE_T = 2 * STEP_T; STANCE = 0.62
REACH = args.pace * STANCE * STRIDE_T / 2                      # how far ahead the heel lands
BODY_Y = 0.02
def limb(name, r_top, r_bot, mat):
    bpy.ops.mesh.primitive_cone_add(vertices=8, radius1=r_top, radius2=r_bot, depth=1.0, location=(0, 0, 0))
    o = bpy.context.object; o.name = name
    for v in o.data.vertices: v.co.z += 0.5                    # base at the origin, tip one unit up z
    o.data.materials.append(mat); o.rotation_mode = "QUATERNION"; o.parent = body
    return o
def shoe(name, mat, sole):
    L0, L1, Wd = -0.085, 0.195, 0.052                          # heel, toe, half width
    v = [(-Wd, L0, 0), (Wd, L0, 0), (Wd * 1.05, 0.10, 0), (Wd * 0.8, L1, 0), (-Wd * 0.8, L1, 0), (-Wd * 1.05, 0.10, 0),       # sole
         (-Wd * 0.85, L0 + 0.01, 0.085), (Wd * 0.85, L0 + 0.01, 0.085), (Wd * 0.9, 0.07, 0.075), (Wd * 0.65, L1 - 0.02, 0.040),
         (-Wd * 0.65, L1 - 0.02, 0.040), (-Wd * 0.9, 0.07, 0.075)]
    f = [(5, 4, 3, 2, 1, 0), (6, 7, 8, 9, 10, 11)] + [(i, (i + 1) % 6, 6 + (i + 1) % 6, 6 + i) for i in range(6)]
    m = bpy.data.meshes.new(name); m.from_pydata(v, [], f); m.materials.append(mat); m.materials.append(sole)
    m.polygons[0].material_index = 1
    o = link(bpy.data.objects.new(name, m)); o.rotation_mode = "XYZ"; o.parent = body; return o
cloth = material("trousers", **{"Base Color": (0.035, 0.05, 0.11, 1), "Roughness": 0.85})
upper = material("shoe", **{"Base Color": (0.86, 0.85, 0.80, 1), "Roughness": 0.6})
sole = material("sole", **{"Base Color": (0.75, 0.10, 0.08, 1), "Roughness": 0.7})
LEGS = [{"side": s, "thigh": limb(f"thigh{s}", 0.088, 0.062, cloth), "shin": limb(f"shin{s}", 0.060, 0.043, cloth), "shoe": shoe(f"shoe{s}", upper, sole)} for s in (-1, 1)]
HIP_Z, L_THIGH, L_SHIN = 0.93, 0.47, 0.46
def foot_at(side, t):
    # left lands on even footsteps, right on odd
    k = (t - args.step_phase) / STRIDE_T - (0.0 if side < 0 else 0.5)
    u = k - math.floor(k)
    if u < STANCE:
        y = REACH - 2 * REACH * (u / STANCE); z = 0.0; pitch = -8 * max(0.0, 1 - u / 0.12)      # heel first
        if u > STANCE - 0.14: pitch = 28 * (u - (STANCE - 0.14)) / 0.14                          # heel peels up
    else:
        w = (u - STANCE) / (1 - STANCE)
        y = -REACH + 2 * REACH * smooth(w); z = 0.075 * math.sin(math.pi * w); pitch = 28 * (1 - w) - 8 * w
    return Vector((side * 0.105, BODY_Y + y, z)), pitch
def pose_legs(t, frame=None):
    for leg in LEGS:
        pos, pitch = foot_at(leg["side"], t)
        sh = leg["shoe"]; sh.location = pos; sh.rotation_euler = (math.radians(pitch), 0, math.radians(leg["side"] * 6))
        ankle = pos + Vector((0, -0.05 * math.cos(math.radians(pitch)), 0.085 + 0.05 * abs(math.sin(math.radians(pitch)))))
        hip = Vector((leg["side"] * 0.10, BODY_Y - 0.02, HIP_Z))
        d = ankle - hip; dist = min(d.length, L_THIGH + L_SHIN - 0.002); dn = d.normalized()
        a = (L_THIGH ** 2 - L_SHIN ** 2 + dist ** 2) / (2 * dist); h = math.sqrt(max(0.0, L_THIGH ** 2 - a ** 2))
        fwd = Vector((0, 1, 0)); bend = (fwd - fwd.dot(dn) * dn).normalized()
        knee = hip + dn * a + bend * h
        for ob, p0, p1 in ((leg["thigh"], hip, knee), (leg["shin"], knee, ankle)):
            v = p1 - p0; ob.location = p0; ob.rotation_quaternion = v.to_track_quat("Z", "Y"); ob.scale = (1, 1, v.length)
            if frame is not None:
                ob.keyframe_insert("location", frame=frame); ob.keyframe_insert("rotation_quaternion", frame=frame); ob.keyframe_insert("scale", frame=frame)
        if frame is not None:
            sh.keyframe_insert("location", frame=frame); sh.keyframe_insert("rotation_euler", frame=frame)
def body_matrix(t):
    ph = 2 * math.pi * args.steps * (t - args.step_phase)
    bob = 0.022 * (0.5 - 0.5 * math.cos(ph)); sway = 0.016 * math.sin(ph / 2)
    tilt = Matrix.Rotation(math.radians(0.7) * math.sin(ph / 2 + 0.4), 4, "Y") @ Matrix.Rotation(math.radians(0.5) * math.sin(ph + 1.1), 4, "X")
    return ROT @ tilt @ Matrix.Translation(Vector((-sway, 0.0, -DROP - bob)))

sun_d = bpy.data.lights.new("sun", "SUN"); sun_d.energy = 3.2; sun_d.angle = math.radians(2.5)
sun = link(bpy.data.objects.new("sun", sun_d))
sun.rotation_euler = (ROT.to_3x3() @ Vector((0.45, -0.25, -0.85))).to_track_quat("-Z", "Y").to_euler()

# ── liquid ───────────────────────────────────────────────────────────────
if CLAY:
    liq = material("clay", **{"Base Color": (0.9, 0.55, 0.35, 1), "Roughness": 0.6})
else:
    liq = material("liquid", **{"Base Color": rgb(args.liquid), "Metallic": 0.0, "Roughness": 0.16,
                                "IOR": 1.12, "Transmission Weight": 0.86, "Coat Weight": 0.9, "Coat Roughness": 0.03})
    for attr, val in (("use_raytrace_refraction", True), ("surface_render_method", "DITHERED"),
                      ("use_transparency_overlap", False), ("thickness_mode", "SPHERE")):
        if hasattr(liq, attr): setattr(liq, attr, val)
    ln, ll = liq.node_tree.nodes, liq.node_tree.links; lb = ln["Principled BSDF"]
    lw = ln.new("ShaderNodeLayerWeight"); lw.inputs["Blend"].default_value = 0.42
    pw = ln.new("ShaderNodeMath"); pw.operation = "POWER"; pw.inputs[1].default_value = 3.0
    mu = ln.new("ShaderNodeMath"); mu.operation = "MULTIPLY"; mu.inputs[1].default_value = 1.1
    ll.new(lw.outputs["Facing"], pw.inputs[0]); ll.new(pw.outputs[0], mu.inputs[0]); ll.new(mu.outputs[0], lb.inputs["Emission Strength"])
    lb.inputs["Emission Color"].default_value = (0.62, 0.90, 1.0, 1)
    if args.poly > 0: lb.inputs["Roughness"].default_value = 0.07; mu.inputs[1].default_value = 0.7

# ── a hand rig ───────────────────────────────────────────────────────────
def rot_about(pivot, axis, deg):
    return Matrix.Translation(pivot) @ Matrix.Rotation(math.radians(deg), 4, axis) @ Matrix.Translation(-pivot)

class Hand:
    FINGERS = ("index-finger", "middle-finger", "ring-finger", "pinky-finger")
    THUMB = ["thumb-metacarpal", "thumb-phalanx-proximal", "thumb-phalanx-distal", "thumb-tip"]

    def __init__(self, path, name):
        before = set(bpy.data.objects)
        bpy.ops.import_scene.gltf(filepath=path)
        new = [o for o in bpy.data.objects if o not in before]
        self.arm = next(o for o in new if o.type == "ARMATURE")
        self.skin = next(o for o in new if o.type == "MESH" and o.parent == self.arm)
        for o in new:
            if o not in (self.arm, self.skin): bpy.data.objects.remove(o, do_unlink=True)
        self.arm.name = name
        self.arm.location = (0, 0, 0); self.arm.rotation_mode = "QUATERNION"; self.arm.rotation_quaternion = (1, 0, 0, 0); self.arm.scale = (1, 1, 1)
        self.arm.parent = held
        bpy.context.view_layer.update()
        self.REST = {b.name: b.matrix_local.copy() for b in self.arm.data.bones}     # bind frames, armature space
        self.HEAD = {n: m.translation.copy() for n, m in self.REST.items()}
        for pb in self.arm.pose.bones: pb.rotation_mode = "QUATERNION"
        H = self.HEAD
        self.chain = {f: [f"{f}-metacarpal", f"{f}-phalanx-proximal", f"{f}-phalanx-intermediate", f"{f}-phalanx-distal", f"{f}-tip"] for f in self.FINGERS}
        self.F = (H["middle-finger-tip"] - H["wrist"]).normalized()                    # fingers point this way
        side = (H["index-finger-phalanx-proximal"] - H["pinky-finger-phalanx-proximal"]).normalized()
        self.N = self.F.cross(side).normalized()                                       # the way the palm faces
        if (H["thumb-tip"] - H["index-finger-phalanx-proximal"]).dot(self.N) < 0: self.N = -self.N
        self.S = self.N.cross(self.F).normalized()
        self.AX = {}
        for f, c in self.chain.items():
            d = (H[c[-1]] - H[c[1]]).normalized(); ax = d.cross(self.N).normalized()
            moved = rot_about(H[c[1]], ax, 20) @ H[c[-1]]
            self.AX[f] = ax if (moved - H[c[-1]]).dot(self.N) > 0 else -ax
        td = (H[self.THUMB[-1]] - H[self.THUMB[0]]).normalized()
        self.AX_T = td.cross(self.F).normalized()
        self.palm_b = sum((H[self.chain[f][1]] for f in self.FINGERS), Vector()) / 4 * 0.6 + H["wrist"] * 0.4
        self.skin.data.materials.clear(); self.skin.data.materials.append(liq)
        if args.poly > 0:
            # Faceted: thin the mesh before the skeleton bends it (so the facets
            # hold still on the hand) and leave every face flat.
            for p in self.skin.data.polygons: p.use_smooth = False
            dec = self.skin.modifiers.new("facets", "DECIMATE"); dec.ratio = args.poly; dec.use_collapse_triangulate = True
            while self.skin.modifiers.find("facets") > 0: self.skin.modifiers.move(self.skin.modifiers.find("facets"), 0)
        else:
            for p in self.skin.data.polygons: p.use_smooth = True
            sub = self.skin.modifiers.new("smooth", "SUBSURF"); sub.levels = 2; sub.render_levels = 2
        self.grow_forearm()

    def grow_forearm(self):
        # The hand mesh stops at an open ring at the wrist. Carry that ring
        # back along the arm in a few steps, widening as a forearm does, so
        # wrist and arm are one surface. It all rides the wrist joint.
        me = self.skin.data; M = self.skin.matrix_local.copy(); Mi = M.inverted()
        bm = bmesh.new(); bm.from_mesh(me); dl = bm.verts.layers.deform.verify()
        wg = self.skin.vertex_groups["wrist"].index
        wrist = self.HEAD["wrist"]; back = -self.F
        edges = [e for e in bm.edges if e.is_boundary and all(((M @ v.co) - wrist).length < 0.07 for v in e.verts)]
        if not edges: bm.free(); return
        ring = list({v for e in edges for v in e.verts})
        c = sum(((M @ v.co) for v in ring), Vector()) / len(ring)
        STEPS = [(0.025, 1.00), (0.07, 1.10), (0.14, 1.26), (0.22, 1.38), (0.30, 1.44), (0.40, 1.46)]   # (distance back, width)
        prev = {v: v for v in ring}
        for k, (dist, wide) in enumerate(STEPS):
            cur = {}
            for v in ring:
                off = (M @ v.co) - c; along = off.dot(back); perp = off - along * back
                keep_along = along * max(0.0, 1 - (k + 1) / 3)          # the ring flattens to a clean section
                nv = bm.verts.new(Mi @ (c + perp * wide + back * (dist + keep_along)))
                nv[dl][wg] = 1.0; cur[v] = nv
            for e in edges:
                v1, v2 = e.verts
                f = bm.faces.new((prev[v1], prev[v2], cur[v2], cur[v1]))
                mid = sum((M @ x.co for x in f.verts), Vector()) / 4
                f.normal_update()
                out = (mid - c) - (mid - c).dot(back) * back
                if (M.to_3x3() @ f.normal).dot(out) < 0: f.normal_flip()
            prev = cur
        bm.to_mesh(me); bm.free(); me.update()

    def local(self, curls, thumb):
        # curls {finger: (mcp, pip, dip)}; thumb (swing about the finger axis, swing about the palm normal, flex, flex)
        H = self.HEAD; X = {"wrist": Matrix.Identity(4)}
        for f, c in self.chain.items():
            a, b, d = curls[f]; ax = self.AX[f]
            m = Matrix.Identity(4); X[c[0]] = m.copy()
            m = m @ rot_about(H[c[1]], ax, a); X[c[1]] = m.copy()
            m = m @ rot_about(H[c[2]], ax, b); X[c[2]] = m.copy()
            m = m @ rot_about(H[c[3]], ax, d); X[c[3]] = m.copy(); X[c[4]] = m.copy()
        T = self.THUMB
        m = rot_about(H[T[0]], self.F, thumb[0]) @ rot_about(H[T[0]], self.N, thumb[1]); X[T[0]] = m.copy()
        m = m @ rot_about(H[T[1]], self.AX_T, thumb[2]); X[T[1]] = m.copy()
        m = m @ rot_about(H[T[2]], self.AX_T, thumb[3]); X[T[2]] = m.copy(); X[T[3]] = m.copy()
        return X

    def frame_for(self, f_s, n_s):
        # rotation taking the bind hand's finger and palm directions to f_s and n_s
        f_s = f_s.normalized(); n_s = (n_s - n_s.dot(f_s) * f_s).normalized(); s_s = n_s.cross(f_s).normalized()
        Rb = Matrix((self.F, self.N, self.S)).transposed(); Rs = Matrix((f_s, n_s, s_s)).transposed()
        return Rs @ Rb.inverted()

    def set(self, X, Hm, elbow, frame=None):
        for pb in self.arm.pose.bones:
            pb.matrix = Hm @ X[pb.name] @ self.REST[pb.name]
            if frame is not None:
                pb.keyframe_insert("location", frame=frame); pb.keyframe_insert("rotation_quaternion", frame=frame)

# ── the touch track (right hand) ─────────────────────────────────────────
def world_xy(x, y):
    return ((x - 0.5) * SW, (0.5 - y) * SH)

# Everything the finger does, in time order: a touch (press and lift), an
# air dip (a typed key: there is no keyboard on screen to hit), or a drag.
acts = []
tap = EV.get("tap")
if tap: acts.append(("touch", tap["t"], tap["t"] + 0.16, world_xy(tap["x"], tap["y"])))
for tc in EV.get("touches", []): acts.append(("touch", tc["t"] - 0.03, tc["t"] + 0.13, world_xy(tc["x"], tc["y"])))
for t in EV.get("letters", []): acts.append(("air", t, t + 0.08, None))
for t in EV.get("enters", []) + ([EV["enter"]] if "enter" in EV else []): acts.append(("air", t, t + 0.13, None))
for k in EV.get("keys", []): acts.append(("touch", k["t"] - 0.03, max(k["up"] - 0.03, k["t"] + 0.08), world_xy(k["x"], k["y"])))
for d in EV.get("drags", []):
    if len(d) >= 2: acts.append(("drag", d[0][0] - 0.03, d[-1][0], [(t - 0.03, *world_xy(x, y)) for t, x, y in d]))
acts.sort(key=lambda r: r[1])

HOVER = 0.028
TOUCH_Z = SCREEN_Z + 0.0045
HOME = Vector((0.020, -0.050))
OFF = Vector((0.27, -0.44))
ELBOW = Vector((0.40, -0.66))
knots = [(0.0, OFF, 0.10, False), (0.35, OFF, 0.10, False)]     # (time, xy, z, straight: no ease, no arc)
def at(t, xy, z, straight=False): knots.append((max(t, knots[-1][0] + 0.005), Vector(xy), z, straight))
for n, (kind, ta, up, what) in enumerate(acts):
    nxt = acts[n + 1][1] if n + 1 < len(acts) else None
    if kind == "drag":
        p0 = what[0]
        at(max(knots[-1][0] + 0.02, ta - 0.085), (p0[1], p0[2]), HOVER)
        at(ta, (p0[1], p0[2]), TOUCH_Z)
        for (t, x, y) in what[1:]: at(t, (x, y), TOUCH_Z, True)
        end = what[-1]; at(end[0] + 0.10, (end[1], end[2]), HOVER)
        if nxt is None or nxt - end[0] > 1.0: at(end[0] + 0.55, HOME, HOVER + 0.012)
        continue
    air = kind == "air"
    xy = Vector(what) if not air else Vector((HOME.x + 0.004 * ((n * 7) % 5 - 2), HOME.y + 0.003 * ((n * 3) % 4 - 1.5)))
    down = TOUCH_Z if not air else SCREEN_Z + 0.014
    arrive = max(knots[-1][0] + 0.02, ta - 0.085)
    at(arrive, xy, HOVER if not air else 0.024)
    at(max(arrive + 0.02, ta), xy, down)
    at(max(up, ta + 0.05), xy, down)
    lift_t = max(up, ta + 0.05) + 0.10
    if nxt is None or nxt - lift_t > 0.14: at(lift_t, xy, HOVER if not air else 0.024)
    if not air and (nxt is None or nxt - lift_t > 1.0): at(lift_t + 0.45, HOME, HOVER + 0.012)
knots.sort(key=lambda k: k[0])

def tip_at(t):
    if t <= knots[0][0]: k = knots[0]; return Vector((k[1].x, k[1].y, k[2]))
    for a, b in zip(knots, knots[1:]):
        if a[0] <= t <= b[0]:
            u = (t - a[0]) / max(1e-6, b[0] - a[0])
            if not b[3]: u = smooth(u)
            xy = a[1].lerp(b[1], u); z = a[2] + (b[2] - a[2]) * u
            if not b[3]: z += 0.012 * math.sin(math.pi * u) * min(1.0, (a[1] - b[1]).length / 0.05)
            return Vector((xy.x, xy.y, z))
    k = knots[-1]; return Vector((k[1].x, k[1].y, k[2]))

# You bring the slate closer to read what you type, and let it drop back to play.
typing = []
ls = sorted(EV.get("letters", []))
for t in ls:
    if typing and t - typing[-1][1] < 1.2: typing[-1][1] = t
    else: typing.append([t, t])
def closeness(t):
    w = 0.0
    for a, b in typing:
        w = max(w, smooth((t - (a - 0.9)) / 0.8) * (1 - smooth((t - (b + 0.9)) / 0.9)))
    return w

right = Hand(args.hand, "right")
left = Hand(args.hand_left, "left")

def pose_right(t, frame=None):
    tip = tip_at(t); p = max(0.0, min(1.0, (HOVER - tip.z) / (HOVER - TOUCH_Z)))
    br = 2.5 * math.sin(t * 2.1)
    curls = {"index-finger": (4 + 8 * p, 7 + 6 * p, 4 + 3 * p), "middle-finger": (58 + br, 84, 46),
             "ring-finger": (64 + br, 88, 50), "pinky-finger": (66 - br, 86, 48)}
    X = right.local(curls, (-26, 0, 22, 26))
    tip_b = X["index-finger-tip"] @ right.HEAD["index-finger-tip"]
    d = (Vector((tip.x, tip.y)) - ELBOW).normalized(); pitch = math.radians(21 + 6 * p)
    f_s = Vector((d.x * math.cos(pitch), d.y * math.cos(pitch), -math.sin(pitch)))
    n_s = Matrix.Rotation(math.radians(-22), 3, f_s) @ Vector((0, 0, -1))
    R = right.frame_for(f_s, n_s)
    Hm = Matrix.Translation(tip - R @ tip_b) @ R.to_4x4()
    right.set(X, Hm, Vector((ELBOW.x, ELBOW.y, 0.17)), frame)

# The left hand holds the slate by its lower-left: palm against the back,
# fingers lying under it toward the right, thumb wrapped over onto the bezel.
L_F = Vector((0.90, 0.42, -0.10)); L_N = Vector((0.10, 0.0, 1.0))
L_PALM = Vector((-bw / 2 - 0.008, -SH * 0.20, -0.030))
L_THUMB_TARGET = Vector((-bw / 2 + 0.005, -SH * 0.20 + 0.050, SCREEN_Z + 0.009))
L_CURLS = {"index-finger": (-5, 3, 1), "middle-finger": (-5, 3, 1), "ring-finger": (-4, 4, 1), "pinky-finger": (-3, 5, 2)}
R_left = left.frame_for(L_F, L_N)
def left_matrix():
    return Matrix.Translation(L_PALM - R_left @ left.palm_b) @ R_left.to_4x4()
# Find the thumb pose that reaches the bezel from under the edge.
Hm_left = left_matrix(); best = None
for a in range(-150, 151, 15):
    for b in range(-90, 91, 15):
        for c in range(-20, 81, 20):
            for d in range(-20, 81, 20):
                X = left.local(L_CURLS, (a, b, c, d))
                tipw = Hm_left @ X["thumb-tip"] @ left.HEAD["thumb-tip"]
                knw = Hm_left @ X["thumb-phalanx-distal"] @ left.HEAD["thumb-phalanx-distal"]
                err = (tipw - L_THUMB_TARGET).length + max(0.0, SCREEN_Z + 0.006 - knw.z) * 2.0 + 0.0002 * (abs(c) + abs(d))
                if best is None or err < best[0]: best = (err, (a, b, c, d))
print("LEFT thumb", best)
def pose_left(t, frame=None):
    sq = 1.5 * math.sin(t * 1.3)
    c = {f: (v[0] + sq, v[1], v[2]) for f, v in L_CURLS.items()}
    left.set(left.local(c, best[1]), Hm_left, Vector((-0.42, -0.62, -0.10)), frame)

frames = [args.still] if args.still else range(1, args.count + 1)
for f in frames:
    t = (f - 1) / FPS; k = None if args.still else f
    pose_right(t, k); pose_left(t, k)
    street.matrix_world = street_matrix(t); held.matrix_world = held_matrix(t)
    body.matrix_world = body_matrix(t); pose_legs(t, k)
    if k:
        body.keyframe_insert("location", frame=f); body.keyframe_insert("rotation_euler", frame=f)
        street.keyframe_insert("location", frame=f); street.keyframe_insert("rotation_euler", frame=f)
        held.keyframe_insert("location", frame=f); held.keyframe_insert("rotation_euler", frame=f)

# ── camera: your eyes ────────────────────────────────────────────────────
cam_data = bpy.data.cameras.new("cam"); cam_data.lens = 42; cam_data.sensor_width = 36
cam_data.dof.use_dof = True; cam_data.dof.aperture_fstop = 4.0
cam = link(bpy.data.objects.new("cam", cam_data)); scene.camera = cam
aim = link(bpy.data.objects.new("aim", None)); aim.location = (0.012, -0.030, 0.01)
tr = cam.constraints.new("TRACK_TO"); tr.target = aim; tr.track_axis = "TRACK_NEGATIVE_Z"; tr.up_axis = "UP_Y"
CAM0 = Vector((0.02, -0.30, 0.56)); AIM0 = Vector(aim.location)
def cam_at(t):
    d = 1.0 - 0.24 * closeness(t) + 0.035 * math.sin(t * 0.55 + 1.0)        # nearer while typing, a slow drift always
    return AIM0 + (CAM0 - AIM0) * d
cam.location = cam_at(0)
cam_data.dof.focus_object = aim
if not args.still:
    for f in range(1, args.count + 1):
        cam.location = cam_at((f - 1) / FPS); cam.keyframe_insert("location", frame=f)

if args.engine == "cycles":
    scene.render.engine = "CYCLES"
    prefs = bpy.context.preferences.addons["cycles"].preferences
    prefs.compute_device_type = "OPTIX"; prefs.get_devices()
    for d in prefs.devices: d.use = d.type == "OPTIX"
    scene.cycles.device = "GPU"; scene.cycles.samples = 64
    scene.cycles.use_adaptive_sampling = True; scene.cycles.use_denoising = True
    scene.cycles.denoiser = "OPTIX"; scene.render.use_persistent_data = True
else:
    scene.render.engine = "BLENDER_EEVEE"
    scene.eevee.taa_render_samples = 32
    scene.eevee.use_raytracing = True
    scene.render.use_motion_blur = True; scene.render.motion_blur_shutter = 0.4   # the pavement streaks a little, as on a phone camera
    try: scene.eevee.ray_tracing_options.resolution_scale = "1"                    # full-resolution refraction: no blocky hands
    except Exception: pass
    scene.eevee.taa_render_samples = 48

if args.scratch:
    scene.render.resolution_percentage = 50; scene.frame_step = 2
    scene.render.use_motion_blur = False
    if scene.render.engine == "BLENDER_EEVEE": scene.eevee.taa_render_samples = 8
scene.render.image_settings.file_format = "PNG"
scene.render.image_settings.color_mode = "RGB"
os.makedirs(args.out, exist_ok=True)
t0 = time.time()
if args.still:
    scene.frame_set(args.still)
    t = (args.still - 1) / FPS
    pose_right(t); pose_left(t); street.matrix_world = street_matrix(t); held.matrix_world = held_matrix(t); cam.location = cam_at(t)
    body.matrix_world = body_matrix(t); pose_legs(t)
    scene.render.filepath = os.path.join(args.out, f"still-{args.still:05d}.png")
    bpy.ops.render.render(write_still=True)
else:
    scene.render.filepath = os.path.join(args.out, "#####")
    bpy.ops.render.render(animation=True)
print(f"SCENE_DONE {args.engine} {time.time() - t0:.1f}s")
