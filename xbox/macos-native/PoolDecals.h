#pragma once
#include <stdint.h>
#include <stdbool.h>
#ifdef __cplusplus
extern "C" {
#endif
void* ac_pool_create(void);
void ac_pool_destroy(void*);
void ac_pool_clear(void*);
bool ac_pool_stamp(void*, const float*);
bool ac_pool_tint(void*, const float*);
const uint8_t* ac_pool_pixels(void*, int*);
const uint8_t* ac_pool_all_pixels(void*);
void ac_pool_clean(void*);
bool ac_pool_mesh(void*, const float*, int, const float*, int);
const float* ac_pool_draw(void*, const float*, const float*, int*);
#ifdef __cplusplus
}
#endif
