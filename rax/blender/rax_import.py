# rax → Blender: the room as rax draws it, and a walk replayed through it.
#
# In Blender: Edit ▸ Preferences ▸ Add-ons ▸ Install… this file. Then
# File ▸ Import ▸ "rax scene (.glb)" and "rax walks (.json)".
#
# Headless (Cycles, frames out as PNG):
#   blender -b -P rax_import.py -- --scene lobby.glb[:above] --scene basement.glb[:below]
#       --walks ucla-2026-11-23.json --from 19:07:30 --to 19:10:00   (UTC, the walk file's date) --speed 8 --fps 24
#       --out frames/ --samples 32 [--size 1280x720] [--frames 1-40] [--camera x,y,z --look x,y,z]
#
# Coordinates: rax is y-up metres (x, y, z); Blender is z-up, so a rax point
# lands at (x, -z, y), the same mapping glTF import uses for the scene.
# A walk sample's y is eye height (feet = y - 1.7); facing is (sin yaw, cos yaw)
# in rax's x, z.
bl_info = {"name": "rax import", "author": "Aesthetic Computer", "version": (0, 1, 0),
           "blender": (4, 2, 0), "location": "File ▸ Import", "category": "Import-Export",
           "description": "Import rax scene captures (.glb) and replay rax walk recordings"}

import bpy, bmesh, json, math, os, sys
from datetime import datetime, timezone
from mathutils import Vector

EYE = 1.7

def rax_to_bl(x, y, z):
    return Vector((x, -z, y))

# ── scene ─────────────────────────────────────────────────────────────────

def vertex_material(name="rax-vertex"):
    mat = bpy.data.materials.get(name)
    if mat: return mat
    mat = bpy.data.materials.new(name)
    mat.use_nodes = True
    nt = mat.node_tree; bsdf = nt.nodes.get("Principled BSDF")
    attr = nt.nodes.new("ShaderNodeVertexColor"); attr.layer_name = "Color"
    nt.links.new(attr.outputs["Color"], bsdf.inputs["Base Color"])
    bsdf.inputs["Roughness"].default_value = 0.85
    return mat

def import_scene(path, keep=None, radius=400.0):
    """Import a rax/export/scene.mjs capture. keep: 'above' / 'below' keeps only
    faces whose centre is above / below the lobby floor (to merge a lobby and a
    basement capture without doubling what both saw)."""
    before = set(bpy.data.objects)
    bpy.ops.import_scene.gltf(filepath=path)
    objs = [o for o in bpy.data.objects if o not in before and o.type == "MESH"]
    mat = vertex_material()
    for o in objs:
        o.name = "rax-" + os.path.splitext(os.path.basename(path))[0]
        bm = bmesh.new(); bm.from_mesh(o.data)
        doomed = []
        for f in bm.faces:
            c = o.matrix_world @ f.calc_center_median()
            if math.hypot(c.x, c.y) > radius: doomed.append(f); continue
            if keep == "above" and c.z < -0.6: doomed.append(f)
            if keep == "below" and c.z >= -0.6: doomed.append(f)
        bmesh.ops.delete(bm, geom=doomed, context="FACES")
        bm.to_mesh(o.data); bm.free()
        color = o.data.color_attributes[0] if o.data.color_attributes else None
        if color: color.name = "Color"
        o.data.materials.clear(); o.data.materials.append(mat)
    return objs

# ── walks ─────────────────────────────────────────────────────────────────

def hex_rgba(h):
    h = h.lstrip("#"); return tuple(int(h[i:i + 2], 16) / 255 for i in (0, 2, 4)) + (1.0,)

def flat_material(name, rgba, glow=0.0):
    mat = bpy.data.materials.new(name); mat.use_nodes = True
    b = mat.node_tree.nodes.get("Principled BSDF")
    b.inputs["Base Color"].default_value = rgba
    b.inputs["Roughness"].default_value = 0.45
    if glow:
        b.inputs["Emission Color"].default_value = rgba
        b.inputs["Emission Strength"].default_value = glow
    return mat

def make_body(person):
    """A rax-ish body: a coloured column, a head, a dark visor facing +Y."""
    rgba = hex_rgba(person["color"])
    root = bpy.data.objects.new("rax-" + person["name"], None); bpy.context.collection.objects.link(root)
    mat = flat_material("body-" + person["name"], rgba)
    bpy.ops.mesh.primitive_cylinder_add(radius=0.28, depth=1.25, location=(0, 0, 0.625 + 0.05))
    body = bpy.context.object; body.data.materials.append(mat); body.parent = root
    bpy.ops.mesh.primitive_uv_sphere_add(radius=0.24, location=(0, 0, 1.55))
    head = bpy.context.object; head.data.materials.append(mat); head.parent = root
    bpy.ops.object.shade_smooth()
    bpy.ops.mesh.primitive_cube_add(size=1, location=(0, 0.2, 1.58))
    visor = bpy.context.object; visor.scale = (0.32, 0.08, 0.1)
    visor.data.materials.append(flat_material("visor", (0.05, 0.05, 0.07, 1)))
    visor.parent = root
    curve = bpy.data.curves.new("label-" + person["name"], "FONT"); curve.body = person["name"]
    curve.align_x = "CENTER"; curve.size = 0.42; curve.extrude = 0.01
    label = bpy.data.objects.new("label-" + person["name"], curve); bpy.context.collection.objects.link(label)
    label.data.materials.append(flat_material("label-" + person["name"], rgba, glow=3.0))
    label.location = (0, 0, 2.15); label.parent = root
    return root, label

def sample_at(samples, t):
    lo, hi = 0, len(samples) - 1
    if t < samples[0][0] or t > samples[-1][0]: return None
    while hi - lo > 1:
        mid = (lo + hi) // 2
        if samples[mid][0] <= t: lo = mid
        else: hi = mid
    a, b = samples[lo], samples[hi]
    if b[0] - a[0] > 20000: return a  # a gap: holds its last pose
    u = 0 if b[0] == a[0] else (t - a[0]) / (b[0] - a[0])
    dy = ((b[4] - a[4] + 180) % 360) - 180
    return [a[i] + (b[i] - a[i]) * u for i in range(4)] + [a[4] + dy * u]

def clock(s, day):
    return datetime.fromisoformat(f"{day}T{s}").replace(tzinfo=timezone.utc).timestamp() * 1000

def import_walks(path, start_ms=None, end_ms=None, speed=8.0, fps=24):
    data = json.load(open(path))
    people = data["people"]
    t0 = start_ms or min(p["samples"][0][0] for p in people)
    t1 = end_ms or max(p["samples"][-1][0] for p in people)
    frames = int((t1 - t0) / 1000 / speed * fps) + 1
    scene = bpy.context.scene
    scene.render.fps = fps; scene.frame_start = 1; scene.frame_end = frames
    actors = []
    for p in people:
        root, label = make_body(p)
        actors.append((p, root, label))
        for f in range(1, frames + 1):
            t = t0 + (f - 1) / fps * speed * 1000
            s = sample_at(p["samples"], t)
            visible = s is not None
            if visible:
                root.location = rax_to_bl(s[1], s[2] - EYE, s[3])
                root.rotation_euler = (0, 0, math.pi + math.radians(s[4]))
                root.keyframe_insert("location", frame=f); root.keyframe_insert("rotation_euler", frame=f)
            for o in [root] + list(root.children):
                o.hide_render = not visible; o.keyframe_insert("hide_render", frame=f)
        if root.animation_data and root.animation_data.action:
            for fc in getattr(root.animation_data.action, "fcurves", []):
                for k in fc.keyframe_points: k.interpolation = "LINEAR"
    return actors, (t0, t1, frames)

# ── look ──────────────────────────────────────────────────────────────────

def setup_render(samples=32, size=(1280, 720)):
    scene = bpy.context.scene
    scene.render.engine = "CYCLES"
    scene.cycles.samples = samples
    scene.cycles.use_denoising = True
    scene.render.resolution_x, scene.render.resolution_y = size
    scene.render.resolution_percentage = 100
    scene.render.image_settings.file_format = "PNG"
    try:
        prefs = bpy.context.preferences.addons["cycles"].preferences
        for kind in ("OPTIX", "CUDA", "HIP", "METAL"):
            try:
                prefs.compute_device_type = kind; prefs.get_devices()
                if any(d.type == kind for d in prefs.devices):
                    for d in prefs.devices: d.use = d.type == kind
                    scene.cycles.device = "GPU"; break
            except Exception: pass
    except Exception: pass
    world = scene.world or bpy.data.worlds.new("rax-sky"); scene.world = world
    world.use_nodes = True
    bg = world.node_tree.nodes.get("Background")
    bg.inputs["Color"].default_value = (0.55, 0.68, 0.9, 1); bg.inputs["Strength"].default_value = 0.6
    sun = bpy.data.lights.new("rax-sun", "SUN"); sun.energy = 2.6; sun.angle = math.radians(3)
    sun_obj = bpy.data.objects.new("rax-sun", sun); scene.collection.objects.link(sun_obj)
    sun_obj.rotation_euler = (math.radians(50), 0, math.radians(35))
    fill = bpy.data.lights.new("rax-fill", "AREA"); fill.energy = 900; fill.size = 12
    fo = bpy.data.objects.new("rax-fill", fill); scene.collection.objects.link(fo); fo.location = (0, 0, 10)
    scene.view_settings.view_transform = "AgX" if "AgX" in [i.identifier for i in scene.view_settings.bl_rna.properties["view_transform"].enum_items] else "Filmic"

def setup_camera(actors, position=None, look=None, lens=24):
    scene = bpy.context.scene
    cam = bpy.data.objects.new("rax-camera", bpy.data.cameras.new("rax-camera"))
    scene.collection.objects.link(cam); scene.camera = cam; cam.data.lens = lens
    target = bpy.data.objects.new("rax-target", None); scene.collection.objects.link(target)
    track = cam.constraints.new("TRACK_TO"); track.target = target
    track.track_axis = "TRACK_NEGATIVE_Z"; track.up_axis = "UP_Y"
    cam.location = rax_to_bl(*(position or (-9, 11, -12)))
    if look:
        target.location = rax_to_bl(*look)
    else:  # follow the middle of whoever is in view, eased
        prev = None
        for f in range(scene.frame_start, scene.frame_end + 1):
            scene.frame_set(f)
            pts = [r.matrix_world.translation for _, r, _ in actors if not r.hide_render]
            if not pts: continue
            mid = sum(pts, Vector()) / len(pts); mid.z = 1.0
            prev = mid if prev is None else prev.lerp(mid, 0.08)
            target.location = prev; target.keyframe_insert("location", frame=f)
    for _, _, label in actors:  # labels face the camera
        c = label.constraints.new("TRACK_TO"); c.target = cam
        c.track_axis = "TRACK_Z"; c.up_axis = "UP_Y"
    return cam

# ── a recording (export/recorder.mjs, film/shoot.mjs --record) ────────────
#
# The take's own frames: every model rax drew, the same triangles and corner
# colours, each draw an object keyed only when it moves; the camera rax had,
# its field of view; the sky's colours, its sun and the other planets; and
# the small planet's bend (planet.mjs `bend`), done here by Geometry Nodes
# about the recorded eye with the recorded radius, frame by frame.

C_RAX = ((1, 0, 0), (0, 0, -1), (0, 1, 0))  # rax (x, y, z) → Blender (x, -z, y)

def rax_rotation(yaw, pitch, roll):
    """The 3×3 that draw() applies (roll about z, then pitch about x, then yaw about y), in rax axes."""
    cy, sy, cp, sp, cr, sr = math.cos(yaw), math.sin(yaw), math.cos(pitch), math.sin(pitch), math.cos(roll), math.sin(roll)
    def f(lx, ly, lz):
        rx, ry = lx * cr - ly * sr, lx * sr + ly * cr
        py, pz = ry * cp - lz * sp, ry * sp + lz * cp
        return (rx * cy + pz * sy, py, -rx * sy + pz * cy)
    cols = [f(1, 0, 0), f(0, 1, 0), f(0, 0, 1)]
    return [[cols[c][r] for c in range(3)] for r in range(3)]

def to_blender_matrix(x, y, z, yaw, pitch, scale, roll):
    from mathutils import Matrix
    R = Matrix(rax_rotation(yaw, pitch, roll)); C = Matrix(C_RAX)
    M = (C @ R @ C.inverted()).to_4x4() @ Matrix.Scale(scale, 4)
    M.translation = rax_to_bl(x, y, z)
    return M

def place(o, M, frame, keys=("location", "rotation_quaternion", "scale")):
    """Set an object to a matrix and key it. (In Blender 5 assigning matrix_world
    does not reach location/rotation before a keyframe reads them: set them.)"""
    loc, rot, scale = M.decompose()
    o.rotation_mode = "QUATERNION"
    o.location, o.rotation_quaternion, o.scale = loc, rot, scale
    for k in keys: o.keyframe_insert(k, frame=frame)

def rgb_of(hex_int):
    return ((hex_int >> 16) & 255) / 255, ((hex_int >> 8) & 255) / 255, (hex_int & 255) / 255

def record_material():
    mat = bpy.data.materials.get("rax-record")
    if mat: return mat
    mat = bpy.data.materials.new("rax-record"); mat.use_nodes = True
    nt = mat.node_tree; bsdf = nt.nodes.get("Principled BSDF")
    attr = nt.nodes.new("ShaderNodeVertexColor"); attr.layer_name = "Color"
    nt.links.new(attr.outputs["Color"], bsdf.inputs["Base Color"])
    nt.links.new(attr.outputs["Color"], bsdf.inputs["Emission Color"])
    bsdf.inputs["Emission Strength"].default_value = 0.3  # (rax's colours are its light: some of them glows)
    bsdf.inputs["Roughness"].default_value = 0.8
    return mat

SMALL = 40  # triangles: a model this small made again with new colours only is the same model (rax re-lights a rolling ball every frame; Blender lights it itself)

def canonical(path, mid, shapes, canon):
    """The model a draw shows: small models are matched by their shape alone."""
    if mid in canon: return canon[mid]
    raw = open(os.path.join(path, "meshes", f"{mid}.bin"), "rb").read()
    import array
    a = array.array("f"); a.frombytes(raw)
    # A shadow's solid (shadow.mjs): every corner exactly black, never a colour rax shows (its darkest ink is #181818). Blender casts its own.
    if not any(a[i] for i in range(len(a)) if i % 6 >= 3): canon[mid] = None; return None
    if len(raw) // 72 > SMALL: canon[mid] = mid; return mid
    import hashlib
    shape = hashlib.sha1(array.array("f", (a[i] for i in range(len(a)) if i % 6 < 3)).tobytes()).hexdigest()
    canon[mid] = shapes.setdefault(shape, mid)
    return canon[mid]

def record_mesh(path, mid, cache):
    if mid in cache: return cache[mid]
    import array
    a = array.array("f"); a.frombytes(open(os.path.join(path, "meshes", f"{mid}.bin"), "rb").read())
    n = len(a) // 6
    verts = [tuple(rax_to_bl(a[i * 6], a[i * 6 + 1], a[i * 6 + 2])) for i in range(n)]
    faces = [(i, i + 1, i + 2) for i in range(0, n - n % 3, 3)]
    me = bpy.data.meshes.new(f"rax-model-{mid}"); me.from_pydata(verts, [], faces); me.update()
    col = me.color_attributes.new("Color", "FLOAT_COLOR", "CORNER")
    vals = []
    for poly in me.polygons:
        for li in poly.loop_indices:
            v = me.loops[li].vertex_index
            r, g, b = a[v * 6 + 3], a[v * 6 + 4], a[v * 6 + 5]
            if r > 1.5: r -= 2  # (see-through / screen / texture marks)
            if g > 3.5: g -= 4
            if b > 7.5: b -= 8
            vals.extend((min(1, max(0, r)), min(1, max(0, g)), min(1, max(0, b)), 1.0))
    col.data.foreach_set("color", vals)
    me.materials.append(record_material())
    cache[mid] = me
    return me

def bend_group():
    """Geometry Nodes: bend the land round the small planet as rax's projector
    does (planet.mjs bend), about the eye (`rax-eye`'s location) with radius
    and ground from `rax-planet` (location x = radius, y = ground; radius 0 is flat)."""
    g = bpy.data.node_groups.get("rax-bend")
    if g: return g
    g = bpy.data.node_groups.new("rax-bend", "GeometryNodeTree")
    g.interface.new_socket("Geometry", in_out="INPUT", socket_type="NodeSocketGeometry")
    g.interface.new_socket("Geometry", in_out="OUTPUT", socket_type="NodeSocketGeometry")
    N, L = g.nodes, g.links
    gin, gout = N.new("NodeGroupInput"), N.new("NodeGroupOutput")
    def node(kind, **props):
        n = N.new(kind)
        for k, v in props.items(): setattr(n, k, v)
        return n
    def math_(op, a, b=None):
        n = node("ShaderNodeMath", operation=op)
        for i, v in enumerate([a, b]):
            if v is None: continue
            if isinstance(v, (int, float)): n.inputs[i].default_value = v
            else: L.new(v, n.inputs[i])
        return n.outputs[0]
    self_ = node("GeometryNodeSelfObject")
    info = node("GeometryNodeObjectInfo"); info.transform_space = "ORIGINAL"; L.new(self_.outputs[0], info.inputs["Object"])
    eye = node("GeometryNodeObjectInfo"); eye.inputs["Object"].default_value = bpy.data.objects["rax-eye"]
    pl = node("GeometryNodeObjectInfo"); pl.inputs["Object"].default_value = bpy.data.objects["rax-planet"]
    pos = node("GeometryNodeInputPosition")
    world = node("FunctionNodeTransformPoint"); L.new(pos.outputs[0], world.inputs["Vector"]); L.new(info.outputs["Transform"], world.inputs["Transform"])
    W, E, P = node("ShaderNodeSeparateXYZ"), node("ShaderNodeSeparateXYZ"), node("ShaderNodeSeparateXYZ")
    L.new(world.outputs[0], W.inputs[0]); L.new(eye.outputs["Location"], E.inputs[0]); L.new(pl.outputs["Location"], P.inputs[0])
    R, G = P.outputs["X"], P.outputs["Y"]
    dx, dy = math_("SUBTRACT", W.outputs["X"], E.outputs["X"]), math_("SUBTRACT", W.outputs["Y"], E.outputs["Y"])
    d = math_("SQRT", math_("ADD", math_("MULTIPLY", dx, dx), math_("MULTIPLY", dy, dy)))
    Rs = math_("MAXIMUM", R, 0.001)
    turn = math_("MINIMUM", math_("DIVIDE", d, Rs), math.pi)
    h = math_("SUBTRACT", W.outputs["Z"], G)
    out = math_("ADD", Rs, h)
    along = math_("DIVIDE", math_("MULTIPLY", out, math_("SINE", turn)), math_("MAXIMUM", d, 1e-6))
    nx = math_("ADD", E.outputs["X"], math_("MULTIPLY", dx, along))
    ny = math_("ADD", E.outputs["Y"], math_("MULTIPLY", dy, along))
    nz = math_("SUBTRACT", math_("ADD", G, math_("MULTIPLY", out, math_("COSINE", turn))), Rs)
    comb = node("ShaderNodeCombineXYZ"); L.new(nx, comb.inputs[0]); L.new(ny, comb.inputs[1]); L.new(nz, comb.inputs[2])
    # Flat (radius under half a metre): the point as it was.
    flat = node("GeometryNodeSwitch", input_type="VECTOR"); L.new(math_("LESS_THAN", R, 0.5), flat.inputs["Switch"])
    L.new(comb.outputs[0], flat.inputs["False"]); L.new(world.outputs[0], flat.inputs["True"])
    inv = node("FunctionNodeInvertMatrix"); L.new(info.outputs["Transform"], inv.inputs[0])
    back = node("FunctionNodeTransformPoint"); L.new(flat.outputs[0], back.inputs["Vector"]); L.new(inv.outputs[0], back.inputs["Transform"])
    setp = node("GeometryNodeSetPosition"); L.new(gin.outputs[0], setp.inputs["Geometry"]); L.new(back.outputs[0], setp.inputs["Position"])
    L.new(setp.outputs[0], gout.inputs[0])
    return g

def fcurves_of(o):
    """An object's F-curves, from a layered action (Blender 4.4+) or an old one."""
    ad = o.animation_data
    if not ad or not ad.action: return []
    a = ad.action
    if getattr(a, "is_action_layered", False) and a.layers:
        out = []
        for layer in a.layers:
            for strip in layer.strips:
                cb = strip.channelbag(ad.action_slot) if ad.action_slot else None
                if cb: out.extend(cb.fcurves)
        return out
    return list(getattr(a, "fcurves", []))

def import_record(path, fps=24, start=None, end=None, bend=True, size=(1280, 720)):
    from mathutils import Matrix, Vector as V
    meta = json.load(open(os.path.join(path, "meta.json")))
    frames = sorted((json.loads(l) for l in open(os.path.join(path, "frames.jsonl")) if l.strip()), key=lambda fr: fr["n"])  # (they arrive out of order: the page sends as it goes)
    if not frames: raise SystemExit("rax: the recording has no frames")
    t0 = frames[0]["t"]
    span = (frames[-1]["t"] - t0) / 1000
    a = start or 0.0; b = min(end or span, span)
    count = int((b - a) * fps) + 1
    scene = bpy.context.scene
    scene.render.fps = fps; scene.frame_start = 1; scene.frame_end = count
    # Which recorded frame each output frame shows (nearest in time).
    picks, j = [], 0
    for f in range(count):
        t = t0 + (a + f / fps) * 1000
        while j + 1 < len(frames) and abs(frames[j + 1]["t"] - t) <= abs(frames[j]["t"] - t): j += 1
        picks.append(frames[j])
    eye = bpy.data.objects.new("rax-eye", None); scene.collection.objects.link(eye)
    planet = bpy.data.objects.new("rax-planet", None); scene.collection.objects.link(planet)
    coll = bpy.data.collections.new("rax-take"); scene.collection.children.link(coll)
    cache, objs, last, shapes, canon = {}, {}, {}, {}, {}
    group = bend_group() if bend else None
    cam = bpy.data.objects.new("rax-camera", bpy.data.cameras.new("rax-camera")); scene.collection.objects.link(cam); scene.camera = cam
    cam.data.sensor_fit = "VERTICAL"; cam.data.clip_start = 0.05; cam.data.clip_end = 20000
    C = Matrix(C_RAX)
    for f, fr in enumerate(picks, start=1):
        c = fr["camera"]; e = c["eye"]
        E = rax_to_bl(e["x"], e["y"], e["z"])
        F = (C @ V(c["forward"])).normalized(); U = (C @ V(c["up"])).normalized(); Rt = F.cross(U).normalized()
        M = Matrix((Rt, U, -F)).transposed().to_4x4(); M.translation = E
        place(cam, M, f, ("location", "rotation_quaternion"))
        cam.data.angle_y = 2 * math.atan(1 / max(1e-3, c["sy"])); cam.data.keyframe_insert("lens", frame=f)
        eye.location = E; eye.keyframe_insert("location", frame=f)
        planet.location = (fr.get("planet") or 0, fr.get("ground") or 0, 0); planet.keyframe_insert("location", frame=f)  # (x: the bend's radius; y: the ground's height, Blender z)
        seen = {}
        for d in fr["draws"]:
            mid = canonical(path, d[0], shapes, canon)
            if mid is None: continue
            k = seen.get(mid, 0); seen[mid] = k + 1; key = (mid, k)
            o = objs.get(key)
            if o is None:
                o = bpy.data.objects.new(f"rax-{mid}-{k}", record_mesh(path, mid, cache)); coll.objects.link(o)
                if group: mod = o.modifiers.new("rax-bend", "NODES"); mod.node_group = group
                objs[key] = o
                if f > 1: o.hide_render = True; o.keyframe_insert("hide_render", frame=f - 1)
            T = tuple(round(v, 5) for v in d[1:])
            if last.get(key, (None,))[0] != T:
                place(o, to_blender_matrix(*d[1:]), f)
            if last.get(key, (None, True))[1] is not True or key not in last:
                o.hide_render = False; o.keyframe_insert("hide_render", frame=f)
            last[key] = (T, True, f)
        for key, (T, shown, lf) in list(last.items()):
            if shown and lf != f:
                o = objs[key]; o.hide_render = True; o.keyframe_insert("hide_render", frame=f); last[key] = (T, False, lf)
    for o in list(objs.values()) + [cam, eye, planet]:
        for fc in fcurves_of(o):
            for kp in fc.keyframe_points: kp.interpolation = "CONSTANT"  # (each key is a frame rax drew: hold it, do not blend)
    for o in objs.values(): o.hide_viewport = False
    record_sky(picks[0]["sky"], picks, fps)
    print(f"rax: {len(picks)} frames, {len(cache)} models, {len(objs)} objects", flush=True)
    return cam

def record_sky(sky, picks, fps):
    """The sky as rax had it: the day's three colours bottom to top, its sun as
    a lamp, and the other planets as lit balls far off along their directions."""
    scene = bpy.context.scene
    world = scene.world or bpy.data.worlds.new("rax-sky"); scene.world = world; world.use_nodes = True
    nt = world.node_tree; bg = nt.nodes.get("Background")
    tc = nt.nodes.new("ShaderNodeTexCoord"); sep = nt.nodes.new("ShaderNodeSeparateXYZ"); ramp = nt.nodes.new("ShaderNodeValToRGB")
    nt.links.new(tc.outputs["Generated"], sep.inputs[0]); nt.links.new(sep.outputs["Z"], ramp.inputs[0])
    els = ramp.color_ramp.elements
    els[0].position = 0.0; els[0].color = rgb_of(sky["horizon"]) + (1,)
    els[1].position = 1.0; els[1].color = rgb_of(sky["zenith"]) + (1,)
    mid = els.new(0.35); mid.color = rgb_of(sky["mid"]) + (1,)
    nt.links.new(ramp.outputs["Color"], bg.inputs["Color"]); bg.inputs["Strength"].default_value = 0.55
    # rax's colours are already the colours on a screen (sRGB): show them as they are, not through a film curve.
    scene.view_settings.view_transform = "Standard"; scene.view_settings.look = "None"
    s = sky["sun"]; sun_dir = rax_to_bl(s["x"], s["y"], s["z"]).normalized()
    lamp = bpy.data.lights.new("rax-sun", "SUN"); lamp.energy = 3.0 if s["y"] > 0 else 0.4; lamp.angle = math.radians(2)
    lo = bpy.data.objects.new("rax-sun", lamp); scene.collection.objects.link(lo)
    lo.rotation_euler = (-sun_dir).to_track_quat("-Z", "Y").to_euler()
    cam = scene.camera
    for p in sky["planets"]:
        bpy.ops.mesh.primitive_uv_sphere_add(segments=48, ring_count=24, radius=1)
        ball = bpy.context.object; ball.name = "rax-planet-" + p["id"]
        mat = bpy.data.materials.new("planet-" + p["id"]); mat.use_nodes = True
        bsdf = mat.node_tree.nodes.get("Principled BSDF")
        bsdf.inputs["Base Color"].default_value = rgb_of(p["ink"]) + (1,)
        bsdf.inputs["Emission Color"].default_value = rgb_of(p["ink"]) + (1,); bsdf.inputs["Emission Strength"].default_value = 0.6
        ball.data.materials.append(mat); bpy.ops.object.shade_smooth()
        far = 4000
        for f, fr in enumerate(picks, start=1):
            if (f - 1) % max(1, fps // 2) and f != len(picks): continue  # (every half second: they move slowly)
            q = next((x for x in fr["sky"]["planets"] if x["id"] == p["id"]), p)
            dvec = rax_to_bl(q["dir"]["x"], q["dir"]["y"], q["dir"]["z"]).normalized()
            e = fr["camera"]["eye"]; at = rax_to_bl(e["x"], e["y"], e["z"])
            ball.location = at + dvec * far; ball.scale = [far * math.tan(q["size"])] * 3
            ball.keyframe_insert("location", frame=f); ball.keyframe_insert("scale", frame=f)

# ── UI ────────────────────────────────────────────────────────────────────

from bpy_extras.io_utils import ImportHelper

class RAX_OT_scene(bpy.types.Operator, ImportHelper):
    bl_idname = "import_scene.rax_scene"; bl_label = "rax scene (.glb)"
    filename_ext = ".glb"
    def execute(self, context):
        import_scene(self.filepath); return {"FINISHED"}

class RAX_OT_walks(bpy.types.Operator, ImportHelper):
    bl_idname = "import_scene.rax_walks"; bl_label = "rax walks (.json)"
    filename_ext = ".json"
    def execute(self, context):
        actors, _ = import_walks(self.filepath); setup_camera(actors); return {"FINISHED"}

def menu(self, context):
    self.layout.operator(RAX_OT_scene.bl_idname); self.layout.operator(RAX_OT_walks.bl_idname)

def register():
    bpy.utils.register_class(RAX_OT_scene); bpy.utils.register_class(RAX_OT_walks)
    bpy.types.TOPBAR_MT_file_import.append(menu)

def unregister():
    bpy.types.TOPBAR_MT_file_import.remove(menu)
    bpy.utils.unregister_class(RAX_OT_walks); bpy.utils.unregister_class(RAX_OT_scene)

# ── headless ──────────────────────────────────────────────────────────────

def main(argv):
    import argparse
    ap = argparse.ArgumentParser(prog="rax_import")
    ap.add_argument("--scene", action="append", default=[])
    ap.add_argument("--walks"); ap.add_argument("--out", default="frames/")
    ap.add_argument("--from", dest="start"); ap.add_argument("--to", dest="end")
    ap.add_argument("--speed", type=float, default=8); ap.add_argument("--fps", type=int, default=24)
    ap.add_argument("--samples", type=int, default=32); ap.add_argument("--size", default="1280x720")
    ap.add_argument("--frames"); ap.add_argument("--camera"); ap.add_argument("--look")
    ap.add_argument("--lens", type=float, default=24); ap.add_argument("--blend")
    ap.add_argument("--record", help="a take's .raxrec folder (film/shoot.mjs --record)")
    ap.add_argument("--start", type=float); ap.add_argument("--end", type=float)
    ap.add_argument("--flat", action="store_true", help="leave the land flat (no small-planet bend)")
    ap.add_argument("--engine", default="CYCLES")
    a = ap.parse_args(argv)
    bpy.ops.wm.read_factory_settings(use_empty=True)
    if a.record:
        w, h = (int(v) for v in a.size.split("x"))
        setup_render(a.samples, (w, h))
        for o in [o for o in bpy.data.objects if o.name.startswith("rax-sun") or o.name.startswith("rax-fill")]: bpy.data.objects.remove(o)
        import_record(a.record, a.fps, a.start, a.end, not a.flat)
        scene = bpy.context.scene
        if a.engine != "CYCLES": scene.render.engine = a.engine
        if a.frames:
            lo, _, hi = a.frames.partition("-"); scene.frame_start = int(lo); scene.frame_end = int(hi or lo)
        if a.blend: bpy.ops.wm.save_as_mainfile(filepath=os.path.abspath(a.blend))
        scene.render.filepath = os.path.join(os.path.abspath(a.out), "f")
        bpy.ops.render.render(animation=True)
        return
    for spec in a.scene:
        path, _, keep = spec.partition(":"); import_scene(path, keep or None)
    actors, span = [], None
    if a.walks:
        data = json.load(open(a.walks)); day = data.get("date", "2026-01-01")
        actors, span = import_walks(a.walks, a.start and clock(a.start, day), a.end and clock(a.end, day), a.speed, a.fps)
    w, h = (int(v) for v in a.size.split("x"))
    setup_render(a.samples, (w, h))
    vec = lambda s: tuple(float(v) for v in s.split(",")) if s else None
    setup_camera(actors, vec(a.camera), vec(a.look), a.lens)
    scene = bpy.context.scene
    if a.frames:
        lo, _, hi = a.frames.partition("-"); scene.frame_start = int(lo); scene.frame_end = int(hi or lo)
    if a.blend: bpy.ops.wm.save_as_mainfile(filepath=os.path.abspath(a.blend))
    scene.render.filepath = os.path.join(os.path.abspath(a.out), "f")
    print(f"rax: {len(actors)} people, frames {scene.frame_start}-{scene.frame_end}", flush=True)
    bpy.ops.render.render(animation=True)

if __name__ == "__main__":
    if "--" in sys.argv: main(sys.argv[sys.argv.index("--") + 1:])
    else: register()
