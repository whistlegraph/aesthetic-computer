#include "Scene4Bridge.h"
#include "ac/scene4.hpp"

namespace s4 = ac::xbox::scene4;

struct ac_scene4_mesh {
  s4::MeshData data;
  std::vector<std::uint16_t> narrow;
};

ac_scene4_mesh* ac_scene4_pack(const ac_scene4_input* input, const char** error) {
  s4::MeshInput in;
  in.positions = input->positions; in.vertex_count = input->vertex_count;
  in.normals = input->normals; in.colors = input->colors;
  in.uvs = input->uvs; in.uvs2 = input->uvs2;
  in.joints = input->joints; in.weights = input->weights; in.weight_bytes = input->weight_bytes;
  in.indices16 = input->indices16; in.indices32 = input->indices32; in.index_count = input->index_count;
  in.bounds = input->bounds;
  auto* mesh = new ac_scene4_mesh;
  if (const char* why = s4::pack(in, mesh->data)) {
    if (error) *error = why;
    delete mesh;
    return nullptr;
  }
  if (!mesh->data.wide()) mesh->narrow.assign(mesh->data.indices.begin(), mesh->data.indices.end());
  return mesh;
}
void ac_scene4_mesh_free(ac_scene4_mesh* mesh) { delete mesh; }
const void* ac_scene4_vertices(const ac_scene4_mesh* mesh, size_t* bytes) {
  *bytes = mesh->data.vertices.size() * sizeof(s4::Vertex);
  return mesh->data.vertices.data();
}
const void* ac_scene4_skin(const ac_scene4_mesh* mesh, size_t* bytes) {
  *bytes = mesh->data.skin.size() * sizeof(s4::Skin);
  return mesh->data.skin.empty() ? nullptr : mesh->data.skin.data();
}
const void* ac_scene4_indices(const ac_scene4_mesh* mesh, size_t* bytes, int* wide) {
  *wide = mesh->data.wide();
  if (*wide) { *bytes = mesh->data.indices.size() * 4; return mesh->data.indices.data(); }
  *bytes = mesh->narrow.size() * 2;
  return mesh->narrow.data();
}
size_t ac_scene4_index_count(const ac_scene4_mesh* mesh) { return mesh->data.indices.size(); }
size_t ac_scene4_bytes(const ac_scene4_mesh* mesh) { return mesh->data.bytes(); }
int ac_scene4_bounds(const ac_scene4_mesh* mesh, float* out6) {
  std::copy(mesh->data.bounds, mesh->data.bounds + 6, out6);
  return mesh->data.bounded;
}
int ac_scene4_max_joint(const ac_scene4_mesh* mesh) { return mesh->data.max_joint; }

int ac_scene4_camera_valid(const float* camera) { return s4::camera_valid(camera); }
int ac_scene4_perspective(const float* camera) { return s4::perspective(camera); }
int ac_scene4_visible(const float* camera, const float* rows, const float* bounds,
                      float draw_distance, float* world_box) {
  return s4::visible(camera, rows, bounds, draw_distance, world_box);
}
void ac_scene4_rows(const float* columns, float* rows) { s4::rows_from_columns(columns, rows); }
int ac_scene4_environment(const float* values, size_t count, float* out13) {
  s4::Environment env;
  if (!s4::read_environment(values, count, env)) return 0;
  const float flat[13]{env.ambient[0], env.ambient[1], env.ambient[2], env.sun[0], env.sun[1], env.sun[2],
    env.fog[0], env.fog[1], env.fog[2], env.fog_start, env.fog_end, env.fog_max, env.draw_distance};
  std::copy(flat, flat + 13, out13);
  return 1;
}
int ac_scene4_lights(const float* values, size_t count, float* out) {
  std::vector<s4::Light> lights;
  if (!s4::read_lights(values, count, lights)) return 0;
  std::memcpy(out, lights.data(), lights.size() * sizeof(s4::Light));
  return 1;
}
uint32_t ac_scene4_light_mask(const float* lights, size_t count, const float* box) {
  std::vector<s4::Light> list(count);
  std::memcpy(list.data(), lights, count * sizeof(s4::Light));
  return s4::light_mask(list, box);
}
int ac_scene4_finite(const float* values, size_t count) { return s4::finite_all(values, count); }
