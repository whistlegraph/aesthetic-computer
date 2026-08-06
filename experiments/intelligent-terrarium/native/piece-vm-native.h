#ifndef PIECE_VM_NATIVE_H
#define PIECE_VM_NATIVE_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

typedef struct PieceVmNative PieceVmNative;

PieceVmNative *piece_vm_native_create(int resolution, const uint8_t *bytecode, size_t byte_count);
bool piece_vm_native_set_senses(PieceVmNative *runtime, const uint8_t *senses, size_t count);
bool piece_vm_native_step(PieceVmNative *runtime, int *fuel_used);
const uint8_t *piece_vm_native_front(const PieceVmNative *runtime);
int piece_vm_native_resolution(const PieceVmNative *runtime);
void piece_vm_native_destroy(PieceVmNative *runtime);

#endif
