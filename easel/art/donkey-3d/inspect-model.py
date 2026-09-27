"""Blender background inspection; deterministic views and mesh statistics."""
import bpy, sys, json, math
from pathlib import Path
from mathutils import Vector
lane = Path(__file__).resolve().parent
bpy.ops.object.select_all(action='SELECT'); bpy.ops.object.delete(use_global=False)
bpy.ops.import_scene.gltf(filepath=str(lane/'donkey.glb'))
meshes=[o for o in bpy.context.scene.objects if o.type=='MESH']
coords=[o.matrix_world @ v.co for o in meshes for v in o.data.vertices]
lo=Vector(tuple(min(v[i] for v in coords) for i in range(3))); hi=Vector(tuple(max(v[i] for v in coords) for i in range(3)))
center=(lo+hi)/2
stats={'bounds':{'min':list(lo),'max':list(hi)},'meshes':[{'name':o.name,'vertices':len(o.data.vertices),'polygons':len(o.data.polygons),'bounds':[list(o.matrix_world @ Vector(v)) for v in o.bound_box]} for o in meshes]}
(lane/'mesh-inspection.json').write_text(json.dumps(stats,indent=2))
print(json.dumps(stats))
scene=bpy.context.scene; scene.render.engine='CYCLES'; scene.cycles.samples=24; scene.render.resolution_x=768;scene.render.resolution_y=768;scene.render.resolution_percentage=100
scene.world.color=(0.65,0.65,0.65)
scene.view_settings.view_transform='Standard'
size=max(hi-lo)
bpy.ops.object.camera_add(); camera=bpy.context.object; camera.data.type='ORTHO';camera.data.ortho_scale=size*1.25;scene.camera=camera
for name,pos,power,scale in [('Key',(2,-3,4),450,5),('Fill',(-3,-2,2),250,4),('Rim',(1,3,3),350,3)]:
 bpy.ops.object.light_add(type='AREA',location=center+Vector(pos)*size);o=bpy.context.object;o.name=name;o.data.energy=power*size*size;o.data.shape='DISK';o.data.size=scale*size;o.rotation_euler=(center-o.location).to_track_quat('-Z','Y').to_euler()
views={'front':(0,-3,.2),'right':(3,0,.2),'back':(0,3,.2),'left':(-3,0,.2),'three-quarter':(2,-3,1.1)}
for name,offset in views.items():
 camera.location=center+Vector(offset)*size;camera.rotation_euler=(center-camera.location).to_track_quat('-Z','Y').to_euler();scene.render.filepath=str(lane/f'view-{name}.png');bpy.ops.render.render(write_still=True)
bpy.ops.wm.save_as_mainfile(filepath=str(lane/'donkey-inspection.blend'))
