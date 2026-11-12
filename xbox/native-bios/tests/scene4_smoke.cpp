// sceneApi 4: the shared scene4.hpp math and the QuickJS bindings, against a
// probe that stands in for the GPU. Runs on any desk (xbox/test-native.sh).
#include <cassert>
#include <cstdio>
#include <map>
#include <set>
#include <string>
#include "QuickJsEngine.hpp"
#include "ac/scene4.hpp"
using namespace ac::xbox;

namespace {
class GpuProbe final : public Graphics {
 public:
  struct Mesh { std::size_t vertices, triangles; bool skinned; bool bounded; float bounds[6]; };
  std::map<int, Mesh> meshes;
  std::set<int> textures;
  int next = 1, draws = 0, culled = 0;
  scene4::Draw last{};
  std::size_t lastInstances = 0, lastBones = 0;
  scene4::Environment env{};
  std::vector<scene4::Light> lights;
  bool gpu_ready() const override { return true; }
  int gpu_mesh(const scene4::MeshData& d) override {
    Mesh m{d.vertices.size(), d.indices.size() / 3, !d.skin.empty(), d.bounded, {}};
    std::copy(d.bounds, d.bounds + 6, m.bounds);
    meshes[next] = m;
    return next++;
  }
  bool gpu_mesh_free(int h) override { return meshes.erase(h) > 0; }
  int gpu_texture(int, int, const std::uint8_t*, unsigned) override { textures.insert(next); return next++; }
  bool gpu_texture_free(int h) override { return textures.erase(h) > 0; }
  void gpu_environment(const scene4::Environment& e) override { env = e; }
  void gpu_lights(const std::vector<scene4::Light>& l) override { lights = l; }
  std::size_t gpu_draw(const scene4::Draw& d) override {
    const auto found = meshes.find(d.mesh);
    if (found == meshes.end()) return 0;
    last = d; lastInstances = d.instance_count; lastBones = d.bone_count;
    std::size_t seen = 0;
    for (std::size_t i = 0; i < d.instance_count; ++i)
      seen += !found->second.bounded || scene4::visible(d.camera, d.instances[i].rows, found->second.bounds, env.draw_distance);
    ++draws; culled += seen == 0;
    return seen * found->second.triangles;
  }
  scene4::Stats gpu_stats() const override {
    scene4::Stats s; s.meshes = int(meshes.size()); s.textures = int(textures.size()); s.draws = std::size_t(draws); return s;
  }
  void wipe(Color) override {} void box(const Rect&) override {} void line(const Line&) override {}
  void write(const Text&) override {}
};
class Quiet final : public Sound { public:
  void synth(const SynthVoice&) override {} void stop_all() override {} int sample_rate() const override { return 48000; } };

// A camera at (0, 0, -10) looking down +z, 1920×1080, focal 1000.
void Camera(float* m, float blend = 1) {
  const float c[27]{0, 0, -10, 1, 0, 0, 0, 1, 0, 0, 0, 1, 960, 540, 1, 1000, blend, .1f, 0, 0, 1920, 1080, -1.4f, .01f, 0, -1, 0};
  std::copy(c, c + 27, m);
}
}

int main() {
  // pack: smooth normals, white default colour, 16-bit indices widen, bounds.
  {
    const float p[]{0, 0, 0, 1, 0, 0, 0, 1, 0, 1, 1, 0};
    const std::uint16_t idx[]{0, 1, 2, 2, 1, 3};
    scene4::MeshInput in; in.positions = p; in.vertex_count = 4; in.indices16 = idx; in.index_count = 6;
    scene4::MeshData d;
    assert(!scene4::pack(in, d));
    assert(d.vertices.size() == 4 && d.indices.size() == 6 && d.indices[5] == 3);
    for (const auto& v : d.vertices) { assert(v.normal[2] == 127 && v.normal[0] == 0); assert(v.color[0] == 255 && v.color[3] == 255); }
    assert(d.bounded && d.bounds[0] == 0 && d.bounds[3] == 1 && d.bounds[5] == 0);
    assert(!d.wide() && d.bytes() == 4 * 36 + 6 * 2);
    const std::uint16_t bad[]{0, 1, 4};
    in.indices16 = bad; in.index_count = 3;
    assert(scene4::pack(in, d) != nullptr);
    in.indices16 = idx; in.index_count = 5;
    assert(scene4::pack(in, d) != nullptr);
  }
  // skin: weights normalise to 255, joint cap, unbounded unless told.
  {
    const float p[]{0, 0, 0, 1, 0, 0, 0, 1, 0};
    const std::uint32_t idx[]{0, 1, 2};
    const std::uint8_t joints[]{0, 1, 0, 0, 2, 0, 0, 0, 3, 3, 3, 3};
    const float weights[]{1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0};
    scene4::MeshInput in; in.positions = p; in.vertex_count = 3; in.indices32 = idx; in.index_count = 3;
    in.joints = joints; in.weights = weights;
    scene4::MeshData d;
    assert(!scene4::pack(in, d));
    assert(d.skin.size() == 3 && !d.bounded && d.max_joint == 3);
    for (const auto& s : d.skin) assert(s.weights[0] + s.weights[1] + s.weights[2] + s.weights[3] == 255);
    assert(d.skin[0].weights[0] >= 127 && d.skin[0].weights[1] >= 127);
    const std::uint8_t over[]{64, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
    in.joints = over;
    assert(scene4::pack(in, d) != nullptr);
  }
  // visible: in front, behind, off each side, past the draw distance.
  {
    float m[27]; Camera(m);
    float rows[12]; scene4::rows_from_columns(scene4::kIdentityColumns, rows);
    const float box[6]{-1, -1, -1, 1, 1, 1};
    assert(scene4::visible(m, rows, box, 0));
    const float behind[6]{-1, -1, -30, 1, 1, -20};
    assert(!scene4::visible(m, rows, behind, 0));
    const float left[6]{-200, -1, 0, -100, 1, 2};   // 960 - 100/10*1000 is far off the left edge
    assert(!scene4::visible(m, rows, left, 0));
    const float right[6]{100, -1, 0, 200, 1, 2};
    assert(!scene4::visible(m, rows, right, 0));
    const float straddle[6]{-1, -1, -20, 1, 1, 5};  // half behind the lens, still seen
    assert(scene4::visible(m, rows, straddle, 0));
    const float far[6]{-1, -1, 500, 1, 1, 502};
    assert(scene4::visible(m, rows, far, 0) && !scene4::visible(m, rows, far, 300));
    float world[6];
    float moved[12]{1, 0, 0, 0, 1, 0, 0, 0, 1, 50, 0, 0};
    scene4::rows_from_columns(moved, rows);
    scene4::visible(m, rows, box, 0, world);
    assert(world[0] == 49 && world[3] == 51);
    // orthographic: the projected-corner test
    Camera(m, 0);
    scene4::rows_from_columns(scene4::kIdentityColumns, rows);
    assert(scene4::visible(m, rows, box, 0));
    const float offOrtho[6]{2000, -1, 0, 2100, 1, 2};
    assert(!scene4::visible(m, rows, offOrtho, 0));
  }
  // light masks: a light reaches a box when its sphere touches it.
  {
    std::vector<scene4::Light> lights(3);
    lights[0] = {{0, 0, 0}, 1, {1, 1, 1}, 0};
    lights[1] = {{10, 0, 0}, 2, {1, 0, 0}, 0};
    lights[2] = {{3, 0, 0}, 2.5f, {0, 0, 0}, 0};  // black: never on
    const float box[6]{-1, -1, -1, 1, 1, 1};
    assert(scene4::light_mask(lights, box) == 1u);
    const float wide[6]{-1, -1, -1, 9, 1, 1};
    assert(scene4::light_mask(lights, wide) == 3u);
    assert(scene4::all_lights(lights) == 7u);
  }

  QuickJsEngine engine; std::string error;
  auto piece = engine.compile({"scene4", "test", R"JS(
    let mesh = 0, skinned = 0, tex = 0, mat = 0, skinMat = 0;
    const cam = new Float32Array([0,0,-10, 1,0,0, 0,1,0, 0,0,1, 960,540, 1,1000,1,.1, 0,0,1920,1080, -1.4,.01, 0,-1,0]);
    const throws = (f) => { try { f(); } catch (e) { return true; } return false; };
    function boot() {
      if (capabilities().sceneApi !== 4) throw Error('level ' + capabilities().sceneApi);
      if (gpuLimits.maxBones !== 64 || gpuLimits.maxLights !== 32) throw Error('limits');
      mesh = gpuMesh({ positions: new Float32Array([-1,-1,0, 1,-1,0, 1,1,0, -1,1,0]),
        indices: new Uint16Array([0,1,2, 0,2,3]),
        colors: new Uint8Array(16).fill(200), uvs: new Float32Array(8), uvs2: new Float32Array(8) });
      if (mesh < 1) throw Error('mesh ' + mesh);
      skinned = gpuMesh({ positions: new Float32Array([0,0,0, 1,0,0, 0,1,0]), indices: new Uint32Array([0,1,2]),
        joints: new Uint8Array([0,0,0,0, 1,0,0,0, 1,0,0,0]), weights: new Uint8Array([255,0,0,0, 255,0,0,0, 128,0,0,0]),
        bounds: new Float32Array([-2,-2,-2,2,2,2]) });
      if (skinned < 1) throw Error('skinned');
      if (!throws(() => gpuMesh({ positions: new Float32Array([0,0,0]), indices: new Uint16Array([0,0,1]) }))) throw Error('index check');
      if (!throws(() => gpuMesh({ positions: new Float32Array([0,0,0]), indices: new Uint16Array([0,0,0]), colors: new Uint8Array(3) }))) throw Error('colour length');
      if (!throws(() => gpuMesh({ positions: [0,0,0], indices: new Uint16Array([0,0,0]) }))) throw Error('plain array');
      tex = gpuTexture(4, 4, new Uint8Array(64).fill(255), 1);
      if (tex < 1) throw Error('texture');
      if (!throws(() => gpuTexture(3, 4, new Uint8Array(48)))) throw Error('pow2');
      if (!throws(() => gpuTexture(4, 4, new Uint8Array(10)))) throw Error('byte count');
      mat = gpuMaterial({ texture: tex, color: [1, .5, .5, 1], cull: 'back', fog: false, alphaCutoff: .5 });
      skinMat = gpuMaterial();
      if (mat < 1 || skinMat < 1 || mat === skinMat) throw Error('materials');
      if (!throws(() => gpuMaterial({ cull: 'sideways' }))) throw Error('cull check');
      if (!throws(() => gpuMaterial({ color: [NaN, 1, 1, 1] }))) throw Error('finite colour');
      gpuEnvironment(new Float32Array([.2,.2,.3, .8,.8,.7, .1,.1,.2, 10,80,1, 300]));
      if (!throws(() => gpuEnvironment(new Float32Array(4)))) throw Error('env length');
      gpuLights(new Float32Array([0,2,0, 5, 0,1,0, 0]), 1);
      if (!throws(() => gpuLights(new Float32Array(8), 33))) throw Error('light cap');
    }
    function sim() {}
    let frame = 0;
    function paint() {
      frame++;
      if (gpuDraw(mesh, mat, cam) !== 2) throw Error('draw');
      if (gpuDraw(mesh, mat, cam, new Float32Array([1,0,0, 0,1,0, 0,0,1, 0,0,-50])) !== 0) throw Error('behind lens');
      if (gpuDraw(999, mat, cam) !== 0 || gpuDraw(mesh, 999, cam) !== 0) throw Error('stale handles');
      if (!throws(() => gpuDraw(mesh, mat, new Float32Array(26)))) throw Error('camera length');
      if (!throws(() => gpuDraw(mesh, mat, cam, new Float32Array(11)))) throw Error('model length');
      const inst = new Float32Array(3 * 16);
      for (let i = 0; i < 3; i++) inst.set([1,0,0, 0,1,0, 0,0,1, i * 400 - 400,0,0, 1,1,1,1], i * 16);
      if (gpuDrawInstances(mesh, mat, cam, inst, 3) !== 2) throw Error('instance culling');
      if (!throws(() => gpuDrawInstances(mesh, mat, cam, inst, 4))) throw Error('instance length');
      if (!throws(() => gpuDraw(skinned, skinMat, cam))) throw Error('bones required');
      if (!throws(() => gpuDraw(skinned, skinMat, cam, null, new Float32Array(12)))) throw Error('bone count');
      const bones = new Float32Array(24); bones.set([1,0,0, 0,1,0, 0,0,1, 0,0,0, 1,0,0, 0,1,0, 0,0,1, 0,1,0]);
      if (gpuDraw(skinned, skinMat, cam, null, bones) !== 1) throw Error('skinned draw');
      const s = gpuStats();
      if (s.meshes !== 2 || s.textures !== 1 || s.materials !== 2) throw Error('stats ' + JSON.stringify(s));
      if (frame === 2) {
        if (!gpuMeshFree(mesh) || gpuMeshFree(mesh) || gpuDraw(mesh, mat, cam) !== 0) throw Error('free');
        if (!gpuMaterialFree(mat) || gpuMaterialFree(mat)) throw Error('material free');
        if (!gpuTextureFree(tex) || gpuTextureFree(tex)) throw Error('texture free');
      }
    }
  )JS", "test"}, {}, error);
  if (!piece) { std::fprintf(stderr, "%s\n", error.c_str()); return 1; }
  {
    GpuProbe gpu; Quiet sound; Api api{{}, {}, {}, {}, gpu, sound, {}};
    try {
      piece->boot(api);
      assert(gpu.meshes.size() == 2 && gpu.textures.size() == 1);
      assert(gpu.env.draw_distance == 300 && gpu.env.fog_end == 80 && gpu.lights.size() == 1 && gpu.lights[0].radius == 5);
      piece->paint(api);
      assert(gpu.last.mesh == 2 && gpu.lastBones == 2 && gpu.lastInstances == 1);
      assert(gpu.last.material.color[0] == 1 && gpu.last.material.cull == 0 && gpu.last.material.fog);
      piece->paint(api);
    } catch (const std::exception& e) { std::fprintf(stderr, "scene4 piece: %s\n", e.what()); return 1; }
    assert(gpu.meshes.size() == 1 && gpu.textures.empty());
    piece.reset();  // the context hands back what it still holds
    assert(gpu.meshes.empty());
  }
  // A host without the pass: level 3, uploads refused, nothing thrown.
  auto bare = engine.compile({"scene4-bare", "test", R"JS(
    function boot() {
      if (capabilities().sceneApi !== 3) throw Error('level');
      if (gpuMesh({ positions: new Float32Array([0,0,0, 1,0,0, 0,1,0]), indices: new Uint16Array([0,1,2]) }) !== -1) throw Error('refusal');
      if (gpuTexture(1, 1, new Uint8Array(4)) !== -1) throw Error('texture refusal');
    }
  )JS", "test"}, {}, error);
  assert(bare);
  {
    class Bare final : public Graphics { public:
      void wipe(Color) override {} void box(const Rect&) override {} void line(const Line&) override {} void write(const Text&) override {} } g;
    Quiet sound; Api api{{}, {}, {}, {}, g, sound, {}};
    try { bare->boot(api); } catch (const std::exception& e) { std::fprintf(stderr, "bare: %s\n", e.what()); return 1; }
  }
  std::puts("scene4 smoke: ok");
  return 0;
}
