# phone-scene.py — a diegetic reel shot, built and rendered headless:
#   blender -b --gpu-backend vulkan -P phone-scene.py -- --frames DIR --count N
#       --out DIR --hdri FILE --table FILE [--engine eevee|cycles] [--still]
#
# A procedurally modelled, unbranded phone (rounded slab, hole-punch camera)
# leans on a small stand on a CC0 Poly Haven table, lit by a CC0 room HDRI.
# The capture plays on its screen as an image sequence, one PNG per frame,
# so screen content and audio stay frame-exact. The view transform is
# Standard and the screen is pure emission at strength 1, so AC's colours
# come out exactly as captured. The camera pushes in slowly with a little
# handheld drift. Units are meters.

import bpy, bmesh, math, sys, argparse, os, time
from mathutils import Vector

argv = sys.argv[sys.argv.index("--") + 1:] if "--" in sys.argv else []
ap = argparse.ArgumentParser()
ap.add_argument("--frames", required=True)        # dir of 00001.png …
ap.add_argument("--count", type=int, required=True)
ap.add_argument("--out", required=True)
ap.add_argument("--hdri", required=True)
ap.add_argument("--table", required=True)
ap.add_argument("--engine", default="eevee")
ap.add_argument("--fps", type=int, default=60)
ap.add_argument("--width", type=int, default=1080)
ap.add_argument("--height", type=int, default=1920)
ap.add_argument("--aspect", type=float, default=1000 / 1840)  # capture w/h
ap.add_argument("--still", type=int, default=0)   # render just this frame
args = ap.parse_args(argv)

# ── scene reset ──────────────────────────────────────────────────────────
bpy.ops.wm.read_factory_settings(use_empty=True)
scene = bpy.context.scene
scene.unit_settings.system = "METRIC"
scene.render.fps = args.fps
scene.frame_start, scene.frame_end = 1, args.count
scene.render.resolution_x, scene.render.resolution_y = args.width, args.height
scene.view_settings.view_transform = "Standard"
scene.view_settings.look = "None"

def material(name, **inputs):
    m = bpy.data.materials.new(name); m.use_nodes = True
    bsdf = m.node_tree.nodes["Principled BSDF"]
    for k, v in inputs.items(): bsdf.inputs[k].default_value = v
    return m

def rounded_rect(w, h, r, seg=12):
    # Outline of a w×h rectangle (bottom edge at y=0, centered in x) with
    # corner radius r.
    pts = []
    corners = [(w / 2 - r, h - r, 0), (-w / 2 + r, h - r, 90),
               (-w / 2 + r, r, 180), (w / 2 - r, r, 270)]
    for cx, cy, a0 in corners:
        for i in range(seg + 1):
            a = math.radians(a0 + 90 * i / seg)
            pts.append((cx + r * math.cos(a), cy + r * math.sin(a)))
    return pts

def slab(name, w, h, r, depth):
    # Rounded slab: front face at z=0 facing +z, extruded back by depth.
    me = bpy.data.meshes.new(name); bm = bmesh.new()
    verts = [bm.verts.new((x, y, 0)) for x, y in rounded_rect(w, h, r)]
    face = bm.faces.new(verts)
    ext = bmesh.ops.extrude_face_region(bm, geom=[face])
    for v in ext["geom"]:
        if isinstance(v, bmesh.types.BMVert): v.co.z -= depth
    bmesh.ops.recalc_face_normals(bm, faces=bm.faces)
    bm.to_mesh(me); bm.free()
    ob = bpy.data.objects.new(name, me); scene.collection.objects.link(ob)
    return ob

def plate(name, w, h, r):
    # Flat rounded plate with UVs spanning its bounds 0..1 (the screen).
    me = bpy.data.meshes.new(name); bm = bmesh.new()
    verts = [bm.verts.new((x, y, 0)) for x, y in rounded_rect(w, h, r)]
    bm.faces.new(verts)
    uv = bm.loops.layers.uv.new()
    for f in bm.faces:
        for loop in f.loops:
            loop[uv].uv = ((loop.vert.co.x + w / 2) / w, loop.vert.co.y / h)
    bm.to_mesh(me); bm.free()
    ob = bpy.data.objects.new(name, me); scene.collection.objects.link(ob)
    return ob

# ── room: HDRI world ─────────────────────────────────────────────────────
world = bpy.data.worlds.new("room"); scene.world = world; world.use_nodes = True
wn = world.node_tree.nodes; wl = world.node_tree.links
env = wn.new("ShaderNodeTexEnvironment"); env.image = bpy.data.images.load(args.hdri)
mapping = wn.new("ShaderNodeMapping"); coord = wn.new("ShaderNodeTexCoord")
mapping.inputs["Rotation"].default_value[2] = math.radians(200)
wl.new(coord.outputs["Generated"], mapping.inputs["Vector"])
wl.new(mapping.outputs["Vector"], env.inputs["Vector"])
wl.new(env.outputs["Color"], wn["Background"].inputs["Color"])
wn["Background"].inputs["Strength"].default_value = 1.0

# ── table: CC0 glTF, top surface moved to z=0, front edge near the camera ─
bpy.ops.import_scene.gltf(filepath=args.table)
table = [o for o in bpy.context.selected_objects]
mins = Vector((1e9, 1e9, 1e9)); maxs = Vector((-1e9, -1e9, -1e9))
for o in table:
    if o.type != "MESH": continue
    for c in o.bound_box:
        wc = o.matrix_world @ Vector(c)
        mins = Vector(map(min, mins, wc)); maxs = Vector(map(max, maxs, wc))
shift = Vector((-(mins.x + maxs.x) / 2, -mins.y - 0.09, -maxs.z))
for o in table:
    if o.parent is None: o.location += shift

# ── phone: sized so the screen is exactly the capture's aspect ───────────
screen_w = 0.066; screen_h = screen_w / args.aspect
bezel = 0.0035; chin = 0.0045
phone_w = screen_w + 2 * bezel; phone_h = screen_h + 2 * chin; depth = 0.0082
lean = math.radians(14)                               # top tips away from camera

body = slab("phone", phone_w, phone_h, 0.0095, depth)
bev = body.modifiers.new("soften", "BEVEL"); bev.width = 0.0011; bev.segments = 4
bev.limit_method = "ANGLE"
body.data.materials.append(material("phone-body", **{
    "Base Color": (0.018, 0.018, 0.022, 1), "Metallic": 0.35, "Roughness": 0.32}))

screen = plate("screen", screen_w, screen_h, 0.0075)
screen.parent = body; screen.location = (0, chin, 0.00025)
sm = bpy.data.materials.new("screen"); sm.use_nodes = True
nodes = sm.node_tree.nodes; links = sm.node_tree.links
bsdf = nodes["Principled BSDF"]
bsdf.inputs["Base Color"].default_value = (0, 0, 0, 1)
bsdf.inputs["Roughness"].default_value = 0.2
bsdf.inputs["Coat Weight"].default_value = 0.08       # the glass
bsdf.inputs["Coat Roughness"].default_value = 0.04
bsdf.inputs["Emission Strength"].default_value = 1.0
tex = nodes.new("ShaderNodeTexImage")
img = bpy.data.images.load(os.path.join(args.frames, "00001.png"))
img.source = "SEQUENCE"; img.colorspace_settings.name = "sRGB"
tex.image = img; tex.interpolation = "Closest"         # AC pixels stay square
tex.image_user.frame_duration = args.count
tex.image_user.frame_start = 1; tex.image_user.frame_offset = 0
tex.image_user.use_auto_refresh = True
links.new(tex.outputs["Color"], bsdf.inputs["Emission Color"])
screen.data.materials.append(sm)

# Hole-punch camera, centered in the top bezel region of the screen.
bpy.ops.mesh.primitive_cylinder_add(radius=0.0017, depth=0.0004, vertices=32)
punch = bpy.context.active_object; punch.parent = body
punch.location = (0, chin + screen_h - 0.006, 0.0004)
punch.data.materials.append(material("punch", **{"Base Color": (0, 0, 0, 1), "Roughness": 0.1}))

# Stand/lean: rotate the upright phone back by `lean` about its bottom edge.
body.rotation_euler = (math.radians(90) - lean, 0, 0)
body.location = (0, 0.0, depth * math.sin(lean) + 0.0005)

# A small matte wedge stand behind it, so the lean is physical.
bpy.ops.mesh.primitive_cube_add(size=1)
stand = bpy.context.active_object
# Its front face meets the phone's back where the phone crosses stand height.
stand_h, stand_d = 0.04, 0.03
back_y = stand_h * math.tan(lean) + depth / math.cos(lean)
stand.scale = (0.05, stand_d, stand_h)
stand.location = (0, back_y + stand_d / 2 + 0.0008, stand_h / 2)
stand.data.materials.append(material("stand", **{"Base Color": (0.55, 0.5, 0.45, 1), "Roughness": 0.8}))

# Rim light from behind-right: a sheen along the phone's edges. From the
# front it mirrored in the screen glass as a hard white bar.
bpy.ops.object.light_add(type="AREA", location=(0.35, 0.3, 0.32))
key = bpy.context.active_object; key.data.energy = 14; key.data.size = 0.6
aim_key = key.constraints.new("TRACK_TO"); aim_key.target = body
aim_key.track_axis = "TRACK_NEGATIVE_Z"; aim_key.up_axis = "UP_Y"

# ── camera: frames the phone, pushes in, breathes ───────────────────────
cam_data = bpy.data.cameras.new("cam"); cam = bpy.data.objects.new("cam", cam_data)
scene.collection.objects.link(cam); scene.camera = cam
cam_data.lens = 50; cam_data.sensor_fit = "VERTICAL"; cam_data.sensor_height = 36
bpy.context.view_layer.update()
center = body.matrix_world @ Vector((0, phone_h / 2, 0))
fill = 0.78                                            # phone height / frame height
half_fov = math.atan(18 / 50)
dist = (phone_h / fill) / 2 / math.tan(half_fov)
normal = (body.matrix_world.to_3x3() @ Vector((0, 0, 1))).normalized()
pitch_up = math.radians(6)                             # camera sits slightly above

def place(d):
    view = (normal * math.cos(pitch_up) + Vector((0, 0, 1)) * math.sin(pitch_up)).normalized()
    return center + view * d

cam_data.dof.use_dof = True; cam_data.dof.aperture_fstop = 2.2
cam_data.dof.focus_distance = dist
aim = bpy.data.objects.new("aim", None); scene.collection.objects.link(aim); aim.location = center
track = cam.constraints.new("TRACK_TO"); track.target = aim
track.track_axis = "TRACK_NEGATIVE_Z"; track.up_axis = "UP_Y"

cam.location = place(dist * 1.12); cam.keyframe_insert("location", frame=1)
cam.location = place(dist * 0.97); cam.keyframe_insert("location", frame=args.count)
cam_data.dof.focus_distance = dist * 1.12; cam_data.dof.keyframe_insert("focus_distance", frame=1)
cam_data.dof.focus_distance = dist * 0.97; cam_data.dof.keyframe_insert("focus_distance", frame=args.count)

def fcurves_of(ob):
    # Blender 5 actions are layered: fcurves live in the slot's channelbag.
    from bpy_extras import anim_utils
    ad = ob.animation_data
    bag = anim_utils.action_get_channelbag_for_slot(ad.action, ad.action_slot)
    return bag.fcurves if bag else ad.action.fcurves

# Handheld drift: low-amplitude noise on location and on the aim point.
aim.keyframe_insert("location", frame=1)
for ob, strength, scale in ((cam, 0.0022, 110), (aim, 0.0012, 140)):
    for i, fc in enumerate(fcurves_of(ob)):
        if fc.data_path != "location": continue
        n = fc.modifiers.new("NOISE"); n.strength = strength; n.scale = scale; n.phase = 7 * i + (3 if ob is aim else 0)

# ── render settings ─────────────────────────────────────────────────────
if args.engine == "cycles":
    scene.render.engine = "CYCLES"
    prefs = bpy.context.preferences.addons["cycles"].preferences
    prefs.compute_device_type = "OPTIX"; prefs.get_devices()
    for d in prefs.devices: d.use = d.type == "OPTIX"
    scene.cycles.device = "GPU"; scene.cycles.samples = 48
    scene.cycles.use_adaptive_sampling = True; scene.cycles.use_denoising = True
    scene.cycles.denoiser = "OPTIX"; scene.render.use_persistent_data = True
    scene.cycles.max_bounces = 4
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
    scene.render.filepath = os.path.join(args.out, f"still-{args.still:05d}.png")
    bpy.ops.render.render(write_still=True)
else:
    scene.render.filepath = os.path.join(args.out, "#####")
    bpy.ops.render.render(animation=True)
print(f"SCENE_DONE {args.engine} {time.time() - t0:.1f}s")
