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
    a = ap.parse_args(argv)
    bpy.ops.wm.read_factory_settings(use_empty=True)
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
