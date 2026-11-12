// sceneApi 4 bindings (SCENE-API-4.md). Included inside QuickJsEngine's
// anonymous namespace after ScenePrimitives.inc. Everything the piece hands
// over is validated here and reaches the host as plain scene4 structs; a
// malformed argument throws, a refusal (budget, no GPU) returns -1 or 0.

// Any typed array, held for the length of the native call.
struct SceneView {
  JSContext* ctx; JSValue buffer = JS_UNDEFINED; const std::uint8_t* data = nullptr;
  std::size_t bytes = 0; int type = -1;
  explicit SceneView(JSContext* c) : ctx(c) {}
  ~SceneView() { JS_FreeValue(ctx, buffer); }
  bool read(JSValueConst value) {
    type = JS_GetTypedArrayType(value);
    if (type < 0) return false;
    std::size_t off = 0, len = 0, stride = 0, total = 0;
    buffer = JS_GetTypedArrayBuffer(ctx, value, &off, &len, &stride);
    if (JS_IsException(buffer)) return false;
    auto* base = JS_GetArrayBuffer(ctx, &total, buffer);
    if (!base || off > total || len > total - off) return false;
    data = base + off; bytes = len; return true;
  }
  template <class T> const T* as() const { return reinterpret_cast<const T*>(data); }
  std::size_t floats() const { return type == JS_TYPED_ARRAY_FLOAT32 ? bytes / 4 : 0; }
};

// One optional typed array off the descriptor: absent is fine, the wrong kind
// or the wrong length is the piece's bug.
static const char* Scene4Field(JSContext* ctx, JSValueConst desc, const char* name, SceneView& view,
    std::initializer_list<int> types, std::size_t elements, bool& present) {
  JSValue value = JS_GetPropertyStr(ctx, desc, name);
  present = !JS_IsUndefined(value) && !JS_IsNull(value);
  if (!present) { JS_FreeValue(ctx, value); return nullptr; }
  const bool ok = view.read(value);
  JS_FreeValue(ctx, value);  // the view holds the buffer
  if (!ok || std::find(types.begin(), types.end(), view.type) == types.end()) return name;
  static const std::size_t sizes[]{1, 1, 1, 2, 2, 4, 4, 8, 8, 2, 4, 8};
  if (elements && view.bytes / sizes[view.type] != elements) return name;
  return nullptr;
}

static JSValue Scene4Refuse(JSContext* ctx, const char* call, const char* what) {
  return JS_ThrowTypeError(ctx, "%s: %s", call, what);
}

// gpuMesh({positions, indices, normals?, colors?, uvs?, uvs2?, joints?, weights?, bounds?}) → handle | -1
JSValue GpuMesh(JSContext* ctx, JSValueConst, int argc, JSValueConst* args) {
  auto* scope = static_cast<CallScope*>(JS_GetContextOpaque(ctx));
  if (!scope || !scope->api) return JS_ThrowTypeError(ctx, "gpuMesh outside a lifecycle call");
  if (argc < 1 || !JS_IsObject(args[0])) return Scene4Refuse(ctx, "gpuMesh", "needs a descriptor object");
  SceneView positions(ctx), indices(ctx), normals(ctx), colors(ctx), uvs(ctx), uvs2(ctx), joints(ctx), weights(ctx), bounds(ctx);
  bool has = false, hasNormals = false, hasColors = false, hasUvs = false, hasUvs2 = false, hasJoints = false, hasWeights = false, hasBounds = false;
  const char* bad = Scene4Field(ctx, args[0], "positions", positions, {JS_TYPED_ARRAY_FLOAT32}, 0, has);
  if (bad || !has || positions.bytes % 12) return Scene4Refuse(ctx, "gpuMesh", "positions must be a Float32Array of xyz");
  const std::size_t n = positions.bytes / 12;
  bad = Scene4Field(ctx, args[0], "indices", indices, {JS_TYPED_ARRAY_UINT16, JS_TYPED_ARRAY_UINT32}, 0, has);
  if (bad || !has) return Scene4Refuse(ctx, "gpuMesh", "indices must be a Uint16Array or Uint32Array");
  if ((bad = Scene4Field(ctx, args[0], "normals", normals, {JS_TYPED_ARRAY_FLOAT32}, n * 3, hasNormals)) ||
      (bad = Scene4Field(ctx, args[0], "colors", colors, {JS_TYPED_ARRAY_UINT8, JS_TYPED_ARRAY_UINT8C}, n * 4, hasColors)) ||
      (bad = Scene4Field(ctx, args[0], "uvs", uvs, {JS_TYPED_ARRAY_FLOAT32}, n * 2, hasUvs)) ||
      (bad = Scene4Field(ctx, args[0], "uvs2", uvs2, {JS_TYPED_ARRAY_FLOAT32}, n * 2, hasUvs2)) ||
      (bad = Scene4Field(ctx, args[0], "joints", joints, {JS_TYPED_ARRAY_UINT8, JS_TYPED_ARRAY_UINT8C}, n * 4, hasJoints)) ||
      (bad = Scene4Field(ctx, args[0], "weights", weights, {JS_TYPED_ARRAY_FLOAT32, JS_TYPED_ARRAY_UINT8, JS_TYPED_ARRAY_UINT8C}, n * 4, hasWeights)) ||
      (bad = Scene4Field(ctx, args[0], "bounds", bounds, {JS_TYPED_ARRAY_FLOAT32}, 6, hasBounds)))
    return JS_ThrowTypeError(ctx, "gpuMesh: %s has the wrong type or length", bad);
  scene4::MeshInput in;
  in.positions = positions.as<float>(); in.vertex_count = n;
  if (indices.type == JS_TYPED_ARRAY_UINT16) { in.indices16 = indices.as<std::uint16_t>(); in.index_count = indices.bytes / 2; }
  else { in.indices32 = indices.as<std::uint32_t>(); in.index_count = indices.bytes / 4; }
  if (hasNormals) in.normals = normals.as<float>();
  if (hasColors) in.colors = colors.data;
  if (hasUvs) in.uvs = uvs.as<float>();
  if (hasUvs2) in.uvs2 = uvs2.as<float>();
  if (hasJoints) in.joints = joints.data;
  if (hasWeights) { if (weights.type == JS_TYPED_ARRAY_FLOAT32) in.weights = weights.as<float>(); else in.weight_bytes = weights.data; }
  if (hasBounds) in.bounds = bounds.as<float>();
  scene4::MeshData data;
  if (const char* why = scene4::pack(in, data)) return Scene4Refuse(ctx, "gpuMesh", why);
  auto& store = scope->scene;
  const int handle = scope->api->graphics.gpu_mesh(data);
  if (handle > 0) {
    store.gpu = &scope->api->graphics;
    store.gpuMeshes[handle] = {data.max_joint, !data.skin.empty()};
  }
  return JS_NewInt32(ctx, handle > 0 ? handle : -1);
}

JSValue GpuMeshFree(JSContext* ctx, JSValueConst, int argc, JSValueConst* args) {
  auto* scope = static_cast<CallScope*>(JS_GetContextOpaque(ctx));
  int32_t handle = 0;
  if (!scope || !scope->api || argc < 1 || JS_ToInt32(ctx, &handle, args[0])) return JS_FALSE;
  auto& store = scope->scene;
  if (!store.gpuMeshes.erase(handle)) return JS_FALSE;
  return JS_NewBool(ctx, scope->api->graphics.gpu_mesh_free(handle));
}

// gpuTexture(width, height, rgba Uint8Array, flags = 0) → handle | -1
JSValue GpuTexture(JSContext* ctx, JSValueConst, int argc, JSValueConst* args) {
  auto* scope = static_cast<CallScope*>(JS_GetContextOpaque(ctx));
  int32_t w = 0, h = 0, flags = 0;
  SceneView pixels(ctx);
  if (!scope || !scope->api || argc < 3 || JS_ToInt32(ctx, &w, args[0]) || JS_ToInt32(ctx, &h, args[1]) ||
      !pixels.read(args[2]) || (pixels.type != JS_TYPED_ARRAY_UINT8 && pixels.type != JS_TYPED_ARRAY_UINT8C) ||
      (argc > 3 && JS_ToInt32(ctx, &flags, args[3])))
    return Scene4Refuse(ctx, "gpuTexture", "needs width, height and a Uint8Array of rgba");
  const auto pow2 = [](int v) { return v > 0 && (v & (v - 1)) == 0; };
  if (!pow2(w) || !pow2(h) || w > scene4::kMaxTextureSide || h > scene4::kMaxTextureSide)
    return JS_ThrowRangeError(ctx, "gpuTexture: sides must be powers of two up to %d", scene4::kMaxTextureSide);
  if (pixels.bytes != std::size_t(w) * h * 4) return JS_ThrowRangeError(ctx, "gpuTexture: rgba must be width*height*4 bytes");
  if (flags < 0 || flags > 7) return JS_ThrowRangeError(ctx, "gpuTexture: unknown flags");
  const int handle = scope->api->graphics.gpu_texture(w, h, pixels.data, unsigned(flags));
  if (handle > 0) { scope->scene.gpu = &scope->api->graphics; scope->scene.gpuTextures.insert(handle); }
  return JS_NewInt32(ctx, handle > 0 ? handle : -1);
}

JSValue GpuTextureFree(JSContext* ctx, JSValueConst, int argc, JSValueConst* args) {
  auto* scope = static_cast<CallScope*>(JS_GetContextOpaque(ctx));
  int32_t handle = 0;
  if (!scope || !scope->api || argc < 1 || JS_ToInt32(ctx, &handle, args[0])) return JS_FALSE;
  if (!scope->scene.gpuTextures.erase(handle)) return JS_FALSE;
  return JS_NewBool(ctx, scope->api->graphics.gpu_texture_free(handle));
}

// Up to `count` numbers off an array or typed array property.
static bool Scene4Numbers(JSContext* ctx, JSValueConst desc, const char* name, float* out, int count, bool& bad) {
  JSValue value = JS_GetPropertyStr(ctx, desc, name);
  if (JS_IsUndefined(value) || JS_IsNull(value)) { JS_FreeValue(ctx, value); return false; }
  for (int i = 0; i < count; ++i) {
    JSValue item = JS_GetPropertyUint32(ctx, value, uint32_t(i));
    double v = 0;
    if (JS_ToFloat64(ctx, &v, item) || !std::isfinite(v)) bad = true;
    else out[i] = float(v);
    JS_FreeValue(ctx, item);
  }
  JS_FreeValue(ctx, value);
  return true;
}
static bool Scene4Bool(JSContext* ctx, JSValueConst desc, const char* name, bool fallback) {
  JSValue value = JS_GetPropertyStr(ctx, desc, name);
  const bool result = JS_IsUndefined(value) ? fallback : JS_ToBool(ctx, value) > 0;
  JS_FreeValue(ctx, value);
  return result;
}

// gpuMaterial({color, emissive, texture, lightmap, lightmapScale, alphaCutoff,
// cull, unlit, fog, depthWrite}) → handle. Materials are parameter blocks the
// binding keeps; nothing is allocated on the GPU.
JSValue GpuMaterial(JSContext* ctx, JSValueConst, int argc, JSValueConst* args) {
  auto* scope = static_cast<CallScope*>(JS_GetContextOpaque(ctx));
  if (!scope || !scope->api) return JS_ThrowTypeError(ctx, "gpuMaterial outside a lifecycle call");
  scene4::Material m;
  if (argc > 0 && !JS_IsUndefined(args[0])) {
    if (!JS_IsObject(args[0])) return Scene4Refuse(ctx, "gpuMaterial", "needs a descriptor object");
    bool bad = false;
    float scalar[4]{};
    Scene4Numbers(ctx, args[0], "color", m.color, 4, bad);
    Scene4Numbers(ctx, args[0], "emissive", m.emissive, 3, bad);
    const char* names[4]{"texture", "lightmap", "lightmapScale", "alphaCutoff"};
    for (int i = 0; i < 4; ++i) {
      JSValue value = JS_GetPropertyStr(ctx, args[0], names[i]);
      double v = i == 2 ? 1 : 0;
      if (!JS_IsUndefined(value) && (JS_ToFloat64(ctx, &v, value) || !std::isfinite(v))) bad = true;
      JS_FreeValue(ctx, value);
      scalar[i] = float(v);
    }
    JSValue cull = JS_GetPropertyStr(ctx, args[0], "cull");
    if (JS_IsString(cull)) {
      const char* text = JS_ToCString(ctx, cull);
      m.cull = !text ? -1 : !std::strcmp(text, "none") ? 0 : !std::strcmp(text, "back") ? 1 : !std::strcmp(text, "front") ? 2 : -1;
      if (text) JS_FreeCString(ctx, text);
    } else if (!JS_IsUndefined(cull)) bad = true;
    JS_FreeValue(ctx, cull);
    if (bad || m.cull < 0) return Scene4Refuse(ctx, "gpuMaterial", "non-finite number or unknown cull");
    m.texture = int(scalar[0]); m.lightmap = int(scalar[1]);
    m.lightmap_scale = std::clamp(scalar[2], 0.f, 16.f); m.alpha_cutoff = std::clamp(scalar[3], 0.f, 1.f);
    for (auto& c : m.color) c = std::clamp(c, 0.f, 16.f);
    for (auto& c : m.emissive) c = std::clamp(c, 0.f, 16.f);
    m.unlit = Scene4Bool(ctx, args[0], "unlit", false);
    m.fog = Scene4Bool(ctx, args[0], "fog", true);
    m.depth_write = Scene4Bool(ctx, args[0], "depthWrite", true);
  }
  auto& list = scope->scene.materials;
  for (std::size_t i = 0; i < list.size(); ++i)
    if (!list[i].live) { list[i] = {m, true}; return JS_NewInt32(ctx, int32_t(i + 1)); }
  if (list.size() >= std::size_t(scene4::kMaxMaterials)) return JS_NewInt32(ctx, -1);
  list.push_back({m, true});
  return JS_NewInt32(ctx, int32_t(list.size()));
}

JSValue GpuMaterialFree(JSContext* ctx, JSValueConst, int argc, JSValueConst* args) {
  auto* scope = static_cast<CallScope*>(JS_GetContextOpaque(ctx));
  int32_t handle = 0;
  if (!scope || argc < 1 || JS_ToInt32(ctx, &handle, args[0])) return JS_FALSE;
  auto& list = scope->scene.materials;
  if (handle < 1 || std::size_t(handle) > list.size() || !list[handle - 1].live) return JS_FALSE;
  list[handle - 1].live = false;
  return JS_TRUE;
}

// gpuEnvironment(Float32Array(≥13)): ambient rgb, sun rgb, fog rgb, fog start,
// fog end, fog max, draw distance. Holds until set again.
JSValue GpuEnvironment(JSContext* ctx, JSValueConst, int argc, JSValueConst* args) {
  auto* scope = static_cast<CallScope*>(JS_GetContextOpaque(ctx));
  SceneFloats values(ctx);
  scene4::Environment env;
  if (!scope || !scope->api || argc < 1 || !values.read(args[0]) || !scene4::read_environment(values.data, values.count, env))
    return JS_ThrowRangeError(ctx, "gpuEnvironment needs a finite Float32Array of 13 floats");
  scope->api->graphics.gpu_environment(env);
  return JS_UNDEFINED;
}

// gpuLights(Float32Array, count): count × 8 floats (x, y, z, radius, r, g, b, 0).
// Holds until set again; gpuLights(null, 0) clears.
JSValue GpuLights(JSContext* ctx, JSValueConst, int argc, JSValueConst* args) {
  auto* scope = static_cast<CallScope*>(JS_GetContextOpaque(ctx));
  int32_t count = 0;
  if (!scope || !scope->api || argc < 2 || JS_ToInt32(ctx, &count, args[1]) || count < 0 || count > scene4::kMaxLights)
    return JS_ThrowRangeError(ctx, "gpuLights needs a Float32Array and a count up to %d", scene4::kMaxLights);
  static std::vector<scene4::Light> lights;
  lights.clear();
  if (count > 0) {
    SceneFloats values(ctx);
    if (!values.read(args[0]) || values.count < std::size_t(count) * 8 ||
        !scene4::read_lights(values.data, std::size_t(count), lights))
      return JS_ThrowRangeError(ctx, "gpuLights: count × 8 finite floats");
  }
  scope->api->graphics.gpu_lights(lights);
  return JS_UNDEFINED;
}

// The shared tail of gpuDraw and gpuDrawInstances.
static JSValue Scene4Submit(JSContext* ctx, CallScope* scope, const char* call, JSValueConst* args,
    int bonesArg, int argc, scene4::Draw& draw) {
  int32_t mesh = 0, material = 0;
  if (JS_ToInt32(ctx, &mesh, args[0]) || JS_ToInt32(ctx, &material, args[1]))
    return Scene4Refuse(ctx, call, "mesh and material are handles");
  auto& store = scope->scene;
  const auto found = store.gpuMeshes.find(mesh);
  if (found == store.gpuMeshes.end() || material < 1 || std::size_t(material) > store.materials.size() ||
      !store.materials[material - 1].live) return JS_NewInt32(ctx, 0);
  SceneFloats camera(ctx);
  if (!camera.read(args[2]) || camera.count != 27 || !scene4::camera_valid(camera.data))
    return JS_ThrowRangeError(ctx, "%s needs a 27-float camera", call);
  std::memcpy(draw.camera, camera.data, sizeof draw.camera);
  draw.mesh = mesh;
  draw.material = store.materials[material - 1].value;
  SceneFloats bones(ctx);
  if (found->second.skinned) {
    if (argc <= bonesArg || !bones.read(args[bonesArg]) || bones.count % 12)
      return JS_ThrowTypeError(ctx, "%s: a skinned mesh needs bones (12 floats each)", call);
    const std::size_t count = bones.count / 12;
    if (count > std::size_t(scene4::kMaxBones) || int(count) <= found->second.maxJoint)
      return JS_ThrowRangeError(ctx, "%s: %d bones for joints up to %d (cap %d)", call, int(count), found->second.maxJoint, scene4::kMaxBones);
    if (!scene4::finite_all(bones.data, bones.count)) return JS_ThrowRangeError(ctx, "%s: non-finite bone", call);
    draw.bones = bones.data; draw.bone_count = count;
  }
  return JS_NewInt64(ctx, int64_t(scope->api->graphics.gpu_draw(draw)));
}

// gpuDraw(mesh, material, camera, model = identity, bones) → triangles queued.
JSValue GpuDraw(JSContext* ctx, JSValueConst, int argc, JSValueConst* args) {
  auto* scope = static_cast<CallScope*>(JS_GetContextOpaque(ctx));
  if (!scope || !scope->api || argc < 3) return Scene4Refuse(ctx, "gpuDraw", "needs mesh, material and camera");
  scene4::Instance one{};
  const float* columns = scene4::kIdentityColumns;
  SceneFloats model(ctx);
  if (argc > 3 && !JS_IsUndefined(args[3]) && !JS_IsNull(args[3])) {
    if (!model.read(args[3]) || model.count != 12 || !scene4::finite_all(model.data, 12))
      return JS_ThrowRangeError(ctx, "gpuDraw: model is 12 finite floats (x axis, y axis, z axis, origin)");
    columns = model.data;
  }
  scene4::rows_from_columns(columns, one.rows);
  for (auto& t : one.tint) t = 1;
  scene4::Draw draw;
  draw.instances = &one; draw.instance_count = 1;
  return Scene4Submit(ctx, scope, "gpuDraw", args, 4, argc, draw);
}

// gpuDrawInstances(mesh, material, camera, instances, count, bones) →
// triangles queued. instances: count × 16 floats, 12 of model then rgba tint.
JSValue GpuDrawInstances(JSContext* ctx, JSValueConst, int argc, JSValueConst* args) {
  auto* scope = static_cast<CallScope*>(JS_GetContextOpaque(ctx));
  int32_t count = 0;
  SceneFloats values(ctx);
  if (!scope || !scope->api || argc < 5 || !values.read(args[3]) || JS_ToInt32(ctx, &count, args[4]))
    return Scene4Refuse(ctx, "gpuDrawInstances", "needs mesh, material, camera, instances and count");
  if (count < 0 || std::size_t(count) > scene4::kMaxInstances || values.count < std::size_t(count) * 16 ||
      !scene4::finite_all(values.data, std::size_t(count) * 16))
    return JS_ThrowRangeError(ctx, "gpuDrawInstances: count × 16 finite floats, up to %d", int(scene4::kMaxInstances));
  if (count == 0) return JS_NewInt32(ctx, 0);
  auto& list = scope->scene.instanceScratch;
  list.resize(std::size_t(count));
  for (int32_t i = 0; i < count; ++i) {
    const float* v = values.data + std::size_t(i) * 16;
    scene4::rows_from_columns(v, list[i].rows);
    for (int k = 0; k < 4; ++k) list[i].tint[k] = std::clamp(v[12 + k], 0.f, 16.f);
  }
  scene4::Draw draw;
  draw.instances = list.data(); draw.instance_count = list.size();
  return Scene4Submit(ctx, scope, "gpuDrawInstances", args, 5, argc, draw);
}

JSValue GpuStats(JSContext* ctx, JSValueConst, int, JSValueConst*) {
  auto* scope = static_cast<CallScope*>(JS_GetContextOpaque(ctx));
  if (!scope || !scope->api) return JS_EXCEPTION;
  const auto s = scope->api->graphics.gpu_stats();
  JSValue out = JS_NewObject(ctx);
  JS_SetPropertyStr(ctx, out, "meshes", JS_NewInt32(ctx, s.meshes));
  JS_SetPropertyStr(ctx, out, "meshBytes", JS_NewInt64(ctx, int64_t(s.mesh_bytes)));
  JS_SetPropertyStr(ctx, out, "textures", JS_NewInt32(ctx, s.textures));
  JS_SetPropertyStr(ctx, out, "textureBytes", JS_NewInt64(ctx, int64_t(s.texture_bytes)));
  JS_SetPropertyStr(ctx, out, "draws", JS_NewInt64(ctx, int64_t(s.draws)));
  JS_SetPropertyStr(ctx, out, "culled", JS_NewInt64(ctx, int64_t(s.culled)));
  JS_SetPropertyStr(ctx, out, "triangles", JS_NewInt64(ctx, int64_t(s.triangles)));
  JS_SetPropertyStr(ctx, out, "dropped", JS_NewInt64(ctx, int64_t(s.dropped)));
  JS_SetPropertyStr(ctx, out, "materials", JS_NewInt32(ctx, int32_t(std::count_if(scope->scene.materials.begin(),
    scope->scene.materials.end(), [](const auto& m) { return m.live; }))));
  return out;
}

static JSValue Scene4Limits(JSContext* ctx) {
  JSValue out = JS_NewObject(ctx);
  const auto set = [&](const char* name, double v) { JS_SetPropertyStr(ctx, out, name, JS_NewFloat64(ctx, v)); };
  set("maxVertices", double(scene4::kMaxVertices)); set("maxIndices", double(scene4::kMaxIndices));
  set("meshBytes", double(scene4::kMaxMeshBytes)); set("textureBytes", double(scene4::kMaxTextureBytes));
  set("maxTextureSide", scene4::kMaxTextureSide); set("maxMeshes", scene4::kMaxMeshes);
  set("maxTextures", scene4::kMaxTextures); set("maxMaterials", scene4::kMaxMaterials);
  set("maxBones", scene4::kMaxBones); set("maxLights", scene4::kMaxLights);
  set("maxDraws", double(scene4::kMaxDraws)); set("maxInstances", double(scene4::kMaxInstances));
  set("maxFrameBones", double(scene4::kMaxFrameBones));
  return out;
}

static void Scene4Install(JSContext* ctx, JSValueConst global) {
  JS_SetPropertyStr(ctx, global, "gpuMesh", JS_NewCFunction(ctx, GpuMesh, "gpuMesh", 1));
  JS_SetPropertyStr(ctx, global, "gpuMeshFree", JS_NewCFunction(ctx, GpuMeshFree, "gpuMeshFree", 1));
  JS_SetPropertyStr(ctx, global, "gpuTexture", JS_NewCFunction(ctx, GpuTexture, "gpuTexture", 4));
  JS_SetPropertyStr(ctx, global, "gpuTextureFree", JS_NewCFunction(ctx, GpuTextureFree, "gpuTextureFree", 1));
  JS_SetPropertyStr(ctx, global, "gpuMaterial", JS_NewCFunction(ctx, GpuMaterial, "gpuMaterial", 1));
  JS_SetPropertyStr(ctx, global, "gpuMaterialFree", JS_NewCFunction(ctx, GpuMaterialFree, "gpuMaterialFree", 1));
  JS_SetPropertyStr(ctx, global, "gpuEnvironment", JS_NewCFunction(ctx, GpuEnvironment, "gpuEnvironment", 1));
  JS_SetPropertyStr(ctx, global, "gpuLights", JS_NewCFunction(ctx, GpuLights, "gpuLights", 2));
  JS_SetPropertyStr(ctx, global, "gpuDraw", JS_NewCFunction(ctx, GpuDraw, "gpuDraw", 5));
  JS_SetPropertyStr(ctx, global, "gpuDrawInstances", JS_NewCFunction(ctx, GpuDrawInstances, "gpuDrawInstances", 6));
  JS_SetPropertyStr(ctx, global, "gpuStats", JS_NewCFunction(ctx, GpuStats, "gpuStats", 0));
  JS_SetPropertyStr(ctx, global, "gpuLimits", Scene4Limits(ctx));
}
