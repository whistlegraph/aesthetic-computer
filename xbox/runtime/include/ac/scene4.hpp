#pragma once

// sceneApi 4: GPU-resident meshes (xbox/native-bios/SCENE-API-4.md is the
// contract). This header is the part every host shares — validation, the
// interleaved vertex layout, bounds, culling and per-draw light masks — so
// the Xbox (QuickJsEngine + App.cpp) and the Mac shell (Scene4Bridge) agree
// to the byte. It owns no GPU object; a host turns MeshData into buffers.

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <cstring>
#include <vector>

namespace ac::xbox::scene4 {

constexpr std::size_t kMaxVertices = 1u << 20;      // per mesh
constexpr std::size_t kMaxIndices = 3u << 20;       // per mesh (1M triangles)
constexpr std::size_t kMaxMeshBytes = 192u << 20;   // every live mesh together
constexpr std::size_t kMaxTextureBytes = 128u << 20;
constexpr int kMaxTextureSide = 2048;
constexpr int kMaxMeshes = 4096, kMaxTextures = 1024, kMaxMaterials = 4096;
constexpr int kMaxBones = 64, kMaxLights = 32;
constexpr std::size_t kMaxDraws = 4096;             // per frame
constexpr std::size_t kMaxInstances = 65536;        // per frame, all draws
constexpr std::size_t kMaxFrameBones = 16384;       // per frame, all skinned draws
constexpr float kMaxCoordinate = 1e6f;

// Texture flags (gpuTexture's fourth argument).
constexpr unsigned kTextureMips = 1, kTextureClamp = 2, kTextureNearest = 4;
// Material flags as the shaders read them.
constexpr unsigned kUnlit = 1, kFog = 2, kTextured = 4, kLightmapped = 8, kSkinned = 16;

// 36 bytes, the one vertex every mesh uploads. Normals are snorm8, colours
// unorm8; the shaders see -1..1 and 0..1.
struct Vertex {
  float position[3];
  std::int8_t normal[4];
  std::uint8_t color[4];
  float uv[2];
  float uv2[2];
};
static_assert(sizeof(Vertex) == 36, "Vertex is the GPU layout");
// A second stream, only for skinned meshes: four joints, weights summing to 255.
struct Skin { std::uint8_t joints[4]; std::uint8_t weights[4]; };
static_assert(sizeof(Skin) == 8, "Skin is the GPU layout");

// Borrowed views of the piece's typed arrays, read once at upload.
struct MeshInput {
  const float* positions = nullptr; std::size_t vertex_count = 0;
  const float* normals = nullptr;
  const std::uint8_t* colors = nullptr;      // 4 per vertex
  const float* uvs = nullptr;
  const float* uvs2 = nullptr;
  const std::uint8_t* joints = nullptr;      // 4 per vertex
  const float* weights = nullptr;            // 4 per vertex, or
  const std::uint8_t* weight_bytes = nullptr;
  const std::uint16_t* indices16 = nullptr;
  const std::uint32_t* indices32 = nullptr; std::size_t index_count = 0;
  const float* bounds = nullptr;             // 6: min xyz, max xyz
};

struct MeshData {
  std::vector<Vertex> vertices;
  std::vector<Skin> skin;                    // empty, or one per vertex
  std::vector<std::uint32_t> indices;
  float bounds[6]{};
  bool bounded = true;                       // false: never culled
  int max_joint = -1;
  [[nodiscard]] bool wide() const { return vertices.size() > 65535; }
  [[nodiscard]] std::size_t bytes() const {
    return vertices.size() * sizeof(Vertex) + skin.size() * sizeof(Skin) +
      indices.size() * (wide() ? 4 : 2);
  }
};

inline bool finite_all(const float* v, std::size_t n, float limit = kMaxCoordinate) {
  for (std::size_t i = 0; i < n; ++i) if (!std::isfinite(v[i]) || std::abs(v[i]) > limit) return false;
  return true;
}

// Validates and interleaves. Returns nullptr, or the reason the data is wrong.
inline const char* pack(const MeshInput& in, MeshData& out) {
  const std::size_t n = in.vertex_count;
  if (!in.positions || n == 0) return "positions required";
  if (n > kMaxVertices) return "too many vertices";
  if (in.index_count == 0 || in.index_count % 3) return "indices must be whole triangles";
  if (in.index_count > kMaxIndices) return "too many indices";
  if (!finite_all(in.positions, n * 3)) return "non-finite position";
  if (in.normals && !finite_all(in.normals, n * 3)) return "non-finite normal";
  if (in.uvs && !finite_all(in.uvs, n * 2)) return "non-finite uv";
  if (in.uvs2 && !finite_all(in.uvs2, n * 2)) return "non-finite uv2";
  if (in.weights && !finite_all(in.weights, n * 4)) return "non-finite weight";
  if (in.joints && !in.weights && !in.weight_bytes) return "joints need weights";
  if (in.bounds && !finite_all(in.bounds, 6)) return "non-finite bounds";
  out.indices.resize(in.index_count);
  for (std::size_t i = 0; i < in.index_count; ++i) {
    const std::uint32_t id = in.indices32 ? in.indices32[i] : in.indices16 ? in.indices16[i] : 0;
    if (id >= n) return "index past the last vertex";
    out.indices[i] = id;
  }
  out.vertices.assign(n, Vertex{});
  // Smooth normals when the piece sends none: area-weighted face normals.
  std::vector<float> made;
  const float* normals = in.normals;
  if (!normals) {
    made.assign(n * 3, 0.f);
    for (std::size_t i = 0; i < in.index_count; i += 3) {
      const float* a = in.positions + out.indices[i] * 3;
      const float* b = in.positions + out.indices[i + 1] * 3;
      const float* c = in.positions + out.indices[i + 2] * 3;
      const float u[3]{b[0] - a[0], b[1] - a[1], b[2] - a[2]}, v[3]{c[0] - a[0], c[1] - a[1], c[2] - a[2]};
      const float f[3]{u[1] * v[2] - u[2] * v[1], u[2] * v[0] - u[0] * v[2], u[0] * v[1] - u[1] * v[0]};
      for (int k = 0; k < 3; ++k) for (int j = 0; j < 3; ++j) made[out.indices[i + k] * 3 + j] += f[j];
    }
    normals = made.data();
  }
  for (std::size_t i = 0; i < n; ++i) {
    auto& v = out.vertices[i];
    std::memcpy(v.position, in.positions + i * 3, 12);
    const float* m = normals + i * 3;
    const float length = std::sqrt(m[0] * m[0] + m[1] * m[1] + m[2] * m[2]);
    for (int j = 0; j < 3; ++j)
      v.normal[j] = static_cast<std::int8_t>(length > 1e-12f ? std::lround(m[j] / length * 127.f) : j == 1 ? 127 : 0);
    if (in.colors) std::memcpy(v.color, in.colors + i * 4, 4);
    else std::memset(v.color, 255, 4);
    if (in.uvs) std::memcpy(v.uv, in.uvs + i * 2, 8);
    if (in.uvs2) std::memcpy(v.uv2, in.uvs2 + i * 2, 8);
  }
  if (in.joints) {
    out.skin.resize(n);
    for (std::size_t i = 0; i < n; ++i) {
      float w[4];
      for (int j = 0; j < 4; ++j) w[j] = in.weights ? (std::max)(0.f, in.weights[i * 4 + j]) : in.weight_bytes[i * 4 + j];
      const float sum = w[0] + w[1] + w[2] + w[3];
      int given = 0;
      for (int j = 0; j < 4; ++j) {
        const auto joint = in.joints[i * 4 + j];
        if (joint >= kMaxBones) return "joint past the bone cap";
        out.skin[i].joints[j] = joint;
        const int byte = sum > 0 ? static_cast<int>(std::lround(w[j] / sum * 255.f)) : j == 0 ? 255 : 0;
        out.skin[i].weights[j] = static_cast<std::uint8_t>(std::clamp(byte, 0, 255 - given));
        given += out.skin[i].weights[j];
        if (out.skin[i].weights[j]) out.max_joint = (std::max<int>)(out.max_joint, joint);
      }
      out.skin[i].weights[0] = static_cast<std::uint8_t>(out.skin[i].weights[0] + (255 - given));
      if (out.skin[i].weights[0]) out.max_joint = (std::max<int>)(out.max_joint, out.skin[i].joints[0]);
    }
  }
  if (in.bounds) { std::memcpy(out.bounds, in.bounds, 24); out.bounded = true; }
  else if (in.joints) out.bounded = false;  // a pose can leave the bind-pose box
  else {
    float* b = out.bounds;
    b[0] = b[1] = b[2] = 1e30f; b[3] = b[4] = b[5] = -1e30f;
    for (std::size_t i = 0; i < n; ++i) for (int j = 0; j < 3; ++j) {
      b[j] = (std::min)(b[j], in.positions[i * 3 + j]);
      b[j + 3] = (std::max)(b[j + 3], in.positions[i * 3 + j]);
    }
  }
  return nullptr;
}

// What a material says, minus the texture objects (handles here, resolved by
// the host at draw time; a freed or missing texture draws as white).
struct Material {
  float color[4]{1, 1, 1, 1};
  float emissive[3]{0, 0, 0};
  float lightmap_scale = 1;
  float alpha_cutoff = 0;
  int texture = 0, lightmap = 0;
  int cull = 0;                    // 0 none, 1 back, 2 front (front = CCW on screen)
  bool unlit = false, fog = true, depth_write = true;
};

struct Environment {
  float ambient[3]{.72f, .72f, .72f};
  float sun[3]{.28f, .28f, .28f};
  float fog[3]{0, 0, 0};
  float fog_start = 0, fog_end = 0, fog_max = 1;
  float draw_distance = 0;         // 0: unlimited
};
// gpuEnvironment's Float32Array: at least 13 floats, laid out as above.
inline bool read_environment(const float* v, std::size_t count, Environment& out) {
  if (count < 13 || !finite_all(v, 13)) return false;
  Environment e;
  std::memcpy(e.ambient, v, 12); std::memcpy(e.sun, v + 3, 12); std::memcpy(e.fog, v + 6, 12);
  e.fog_start = v[9]; e.fog_end = v[10]; e.fog_max = std::clamp(v[11], 0.f, 1.f);
  e.draw_distance = (std::max)(0.f, v[12]);
  out = e;
  return true;
}

struct Light { float position[3]; float radius; float color[3]; float pad; };
static_assert(sizeof(Light) == 32, "Light is 8 floats");
inline bool read_lights(const float* v, std::size_t count, std::vector<Light>& out) {
  if (count > static_cast<std::size_t>(kMaxLights) || !finite_all(v, count * 8)) return false;
  out.resize(count);
  for (std::size_t i = 0; i < count; ++i) {
    std::memcpy(&out[i], v + i * 8, 32);
    out[i].radius = (std::max)(0.f, out[i].radius);
    out[i].pad = 0;
  }
  return true;
}

// One instance as the shaders read it: the 3×4 model matrix as three rows,
// then an rgba tint. The piece sends columns (x axis, y axis, z axis, origin)
// — 12 floats — so a row is (ax, bx, cx, tx).
struct Instance { float rows[12]; float tint[4]; };
static_assert(sizeof(Instance) == 64, "Instance is 16 floats");
inline void rows_from_columns(const float* c, float* rows) {
  for (int r = 0; r < 3; ++r) for (int k = 0; k < 4; ++k) rows[r * 4 + k] = c[k * 3 + r];
}
constexpr float kIdentityColumns[12]{1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0};

inline void transform(const float* rows, const float* p, float* out) {
  for (int r = 0; r < 3; ++r)
    out[r] = rows[r * 4] * p[0] + rows[r * 4 + 1] * p[1] + rows[r * 4 + 2] * p[2] + rows[r * 4 + 3];
}

// The 27-float camera sceneMesh and meshDraw take.
inline bool camera_valid(const float* m) {
  if (!finite_all(m, 27, 1e9f)) return false;
  return !(m[17] <= 0 || m[18] >= m[20] || m[19] >= m[21] ||
           m[18] < -32200 || m[19] < -32200 || m[20] > 32200 || m[21] > 32200);
}
inline bool perspective(const float* m) { return m[16] > .999f; }

// The box (local bounds under `rows`) against the camera. Exact half-space
// tests for a perspective camera (they hold behind the lens too); for an
// orthographic or blended one, meshDraw's projected-corner test.
inline bool visible(const float* m, const float* rows, const float* bounds, float draw_distance,
                    float* world_box = nullptr) {
  float view[8][3], lo[3]{1e30f, 1e30f, 1e30f}, hi[3]{-1e30f, -1e30f, -1e30f};
  for (int c = 0; c < 8; ++c) {
    const float p[3]{bounds[c & 1 ? 3 : 0], bounds[c & 2 ? 4 : 1], bounds[c & 4 ? 5 : 2]};
    float w[3];
    transform(rows, p, w);
    for (int j = 0; j < 3; ++j) { lo[j] = (std::min)(lo[j], w[j]); hi[j] = (std::max)(hi[j], w[j]); }
    const float d[3]{w[0] - m[0], w[1] - m[1], w[2] - m[2]};
    view[c][0] = d[0] * m[3] + d[1] * m[4] + d[2] * m[5];
    view[c][1] = d[0] * m[6] + d[1] * m[7] + d[2] * m[8];
    view[c][2] = d[0] * m[9] + d[1] * m[10] + d[2] * m[11];
  }
  if (world_box) { std::memcpy(world_box, lo, 12); std::memcpy(world_box + 3, hi, 12); }
  const auto outside = [&](auto plane) { for (auto& v : view) if (plane(v) >= 0) return false; return true; };
  if (outside([&](const float* v) { return v[2] - m[17]; })) return false;
  if (draw_distance > 0 && outside([&](const float* v) { return draw_distance - v[2]; })) return false;
  if (perspective(m)) {
    const float f = m[15], cx = m[12], cy = m[13];
    if (outside([&](const float* v) { return v[0] * f - (m[18] - cx) * v[2]; })) return false;
    if (outside([&](const float* v) { return (m[20] - cx) * v[2] - v[0] * f; })) return false;
    if (outside([&](const float* v) { return (cy - m[19]) * v[2] - v[1] * f; })) return false;
    if (outside([&](const float* v) { return v[1] * f + (m[21] - cy) * v[2]; })) return false;
    return true;
  }
  int left = 0, right = 0, above = 0, below = 0;
  for (auto& v : view) {
    if (v[2] < m[17]) return true;  // straddles the lens: keep it, the clip plane decides
    const float k = m[14] + (m[15] / v[2] - m[14]) * m[16];
    const float x = m[12] + v[0] * k, y = m[13] - v[1] * k;
    left += x < m[18]; right += x > m[20]; above += y < m[19]; below += y > m[21];
  }
  return !(left == 8 || right == 8 || above == 8 || below == 8);
}

// Which of the frame's lights can reach a world box: bit i set for light i.
inline std::uint32_t light_mask(const std::vector<Light>& lights, const float* box) {
  std::uint32_t mask = 0;
  for (std::size_t i = 0; i < lights.size() && i < 32; ++i) {
    const auto& l = lights[i];
    if (l.radius <= 0 || (l.color[0] <= 0 && l.color[1] <= 0 && l.color[2] <= 0)) continue;
    float d2 = 0;
    for (int j = 0; j < 3; ++j) {
      const float c = std::clamp(l.position[j], box[j], box[j + 3]) - l.position[j];
      d2 += c * c;
    }
    if (d2 <= l.radius * l.radius) mask |= 1u << i;
  }
  return mask;
}
inline std::uint32_t all_lights(const std::vector<Light>& lights) {
  return lights.size() >= 32 ? 0xffffffffu : (1u << lights.size()) - 1;
}

// One queued draw, as the binding hands it to the host after validation.
struct Draw {
  int mesh = 0;
  Material material;
  float camera[27]{};
  const Instance* instances = nullptr; std::size_t instance_count = 0;
  const float* bones = nullptr; std::size_t bone_count = 0;  // 12 floats each, columns
};

struct Stats {
  int meshes = 0, textures = 0;
  std::size_t mesh_bytes = 0, texture_bytes = 0;
  std::size_t draws = 0, culled = 0, triangles = 0, dropped = 0;  // last frame
};

}  // namespace ac::xbox::scene4
