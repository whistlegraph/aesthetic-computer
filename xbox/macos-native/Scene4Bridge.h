#pragma once
// sceneApi 4 for Swift: the shared xbox/runtime/include/ac/scene4.hpp behind
// a C face, so the Mac validates, interleaves and culls exactly as the Xbox.
#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct ac_scene4_mesh ac_scene4_mesh;
typedef struct {
  const float* positions; size_t vertex_count;
  const float* normals;
  const uint8_t* colors;
  const float* uvs;
  const float* uvs2;
  const uint8_t* joints;
  const float* weights;
  const uint8_t* weight_bytes;
  const uint16_t* indices16;
  const uint32_t* indices32; size_t index_count;
  const float* bounds;
} ac_scene4_input;

// NULL, with *error naming the problem, when the data is wrong.
ac_scene4_mesh* ac_scene4_pack(const ac_scene4_input* input, const char** error);
void ac_scene4_mesh_free(ac_scene4_mesh* mesh);
const void* ac_scene4_vertices(const ac_scene4_mesh* mesh, size_t* bytes);
const void* ac_scene4_skin(const ac_scene4_mesh* mesh, size_t* bytes);       // NULL when unskinned
const void* ac_scene4_indices(const ac_scene4_mesh* mesh, size_t* bytes, int* wide);
size_t ac_scene4_index_count(const ac_scene4_mesh* mesh);
size_t ac_scene4_bytes(const ac_scene4_mesh* mesh);
int ac_scene4_bounds(const ac_scene4_mesh* mesh, float* out6);              // 0: never culled
int ac_scene4_max_joint(const ac_scene4_mesh* mesh);

int ac_scene4_camera_valid(const float* camera);
int ac_scene4_perspective(const float* camera);
int ac_scene4_visible(const float* camera, const float* rows, const float* bounds,
                      float draw_distance, float* world_box);
void ac_scene4_rows(const float* columns, float* rows);
// out: ambient rgb, sun rgb, fog rgb, fog start, fog end, fog max, draw distance.
int ac_scene4_environment(const float* values, size_t count, float* out13);
// lights: 8 floats each; 0 when over the cap or not finite.
int ac_scene4_lights(const float* values, size_t count, float* out);
uint32_t ac_scene4_light_mask(const float* lights, size_t count, const float* box);
int ac_scene4_finite(const float* values, size_t count);

#ifdef __cplusplus
}
#endif
