#include <errno.h>
#include <math.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "piece-vm-native.h"

#define VM_FIXED 65536
#define VM_REGS 32
#define VM_DATA 16384
#define VM_MAX_CODE 512
#define VM_MAX_STACK 256
#define VM_MAX_CALL 32
#define VM_MAX_XFORM 32
#define VM_FUEL 1000000
#define VM_SENSES 8
#define VM_PI 3.14159265358979323846264338327950288

typedef struct { uint8_t op, a, b, c; int32_t imm; } Instruction;
typedef struct {
    int side, front, back, frame;
    int32_t registers[VM_REGS];
    uint8_t data[VM_DATA];
    uint8_t senses[VM_SENSES];
    uint8_t *buffers[2];
    double perspective;
} Vm;

static const char *glyphs[128] = {
    [' ']="00000/00000/00000/00000/00000/00000/00000",
    ['0']="01110/10001/10011/10101/11001/10001/01110", ['1']="00100/01100/00100/00100/00100/00100/01110",
    ['2']="01110/10001/00001/00010/00100/01000/11111", ['3']="11110/00001/00001/01110/00001/00001/11110",
    ['4']="00010/00110/01010/10010/11111/00010/00010", ['5']="11111/10000/10000/11110/00001/00001/11110",
    ['6']="01110/10000/10000/11110/10001/10001/01110", ['7']="11111/00001/00010/00100/01000/01000/01000",
    ['8']="01110/10001/10001/01110/10001/10001/01110", ['9']="01110/10001/10001/01111/00001/00001/01110",
    ['A']="01110/10001/10001/11111/10001/10001/10001", ['B']="11110/10001/10001/11110/10001/10001/11110",
    ['C']="01111/10000/10000/10000/10000/10000/01111", ['D']="11110/10001/10001/10001/10001/10001/11110",
    ['E']="11111/10000/10000/11110/10000/10000/11111", ['F']="11111/10000/10000/11110/10000/10000/10000",
    ['G']="01111/10000/10000/10111/10001/10001/01111", ['H']="10001/10001/10001/11111/10001/10001/10001",
    ['I']="01110/00100/00100/00100/00100/00100/01110", ['J']="00001/00001/00001/00001/10001/10001/01110",
    ['K']="10001/10010/10100/11000/10100/10010/10001", ['L']="10000/10000/10000/10000/10000/10000/11111",
    ['M']="10001/11011/10101/10101/10001/10001/10001", ['N']="10001/11001/10101/10011/10001/10001/10001",
    ['O']="01110/10001/10001/10001/10001/10001/01110", ['P']="11110/10001/10001/11110/10000/10000/10000",
    ['Q']="01110/10001/10001/10001/10101/10010/01101", ['R']="11110/10001/10001/11110/10100/10010/10001",
    ['S']="01111/10000/10000/01110/00001/00001/11110", ['T']="11111/00100/00100/00100/00100/00100/00100",
    ['U']="10001/10001/10001/10001/10001/10001/01110", ['V']="10001/10001/10001/10001/10001/01010/00100",
    ['W']="10001/10001/10001/10101/10101/10101/01010", ['X']="10001/10001/01010/00100/01010/10001/10001",
    ['Y']="10001/10001/01010/00100/00100/00100/00100", ['Z']="11111/00001/00010/00100/01000/10000/11111",
};

static bool valid_region(int32_t immediate) {
    uint32_t packed = (uint32_t)immediate;
    uint32_t offset = packed & 0xffffu, length = packed >> 16;
    return length > 0 && offset + length <= VM_DATA;
}

#ifndef PIECE_VM_LIBRARY
static int nibble(char value) {
    if (value >= '0' && value <= '9') return value - '0';
    if (value >= 'a' && value <= 'f') return value - 'a' + 10;
    if (value >= 'A' && value <= 'F') return value - 'A' + 10;
    return -1;
}

static bool decode_hex(const char *hex, Instruction *code, int *count) {
    size_t length = strlen(hex);
    if (!length || length % 16 || length / 16 > VM_MAX_CODE) return false;
    *count = (int)(length / 16);
    for (int index = 0; index < *count; index++) {
        uint8_t bytes[8];
        for (int byte = 0; byte < 8; byte++) {
            int high = nibble(hex[index * 16 + byte * 2]), low = nibble(hex[index * 16 + byte * 2 + 1]);
            if (high < 0 || low < 0) return false;
            bytes[byte] = (uint8_t)(high * 16 + low);
        }
        code[index] = (Instruction){bytes[0], bytes[1], bytes[2], bytes[3],
            (int32_t)((uint32_t)bytes[4] | (uint32_t)bytes[5] << 8 | (uint32_t)bytes[6] << 16 | (uint32_t)bytes[7] << 24)};
        if (code[index].op > 33 || code[index].a >= VM_REGS || code[index].b >= VM_REGS || code[index].c >= VM_REGS ||
            (code[index].op == 31 && (code[index].imm < 0 || code[index].imm >= VM_SENSES)) ||
            ((code[index].op == 32 || code[index].op == 33) && !valid_region(code[index].imm))) return false;
    }
    return true;
}
#endif

static bool decode_bytes(const uint8_t *bytes, size_t length, Instruction *code, int *count) {
    if (!bytes || !length || length % 8 || length / 8 > VM_MAX_CODE) return false;
    *count = (int)(length / 8);
    for (int index = 0; index < *count; index++) {
        const uint8_t *at = bytes + index * 8;
        code[index] = (Instruction){at[0], at[1], at[2], at[3],
            (int32_t)((uint32_t)at[4] | (uint32_t)at[5] << 8 | (uint32_t)at[6] << 16 | (uint32_t)at[7] << 24)};
        if (code[index].op > 33 || code[index].a >= VM_REGS || code[index].b >= VM_REGS || code[index].c >= VM_REGS ||
            (code[index].op == 31 && (code[index].imm < 0 || code[index].imm >= VM_SENSES)) ||
            ((code[index].op == 32 || code[index].op == 33) && !valid_region(code[index].imm))) return false;
    }
    return true;
}

static void identity(double *matrix) {
    memset(matrix, 0, sizeof(double) * 16);
    matrix[0] = matrix[5] = matrix[10] = matrix[15] = 1;
}

static void multiply(double *output, const double *left, const double *right) {
    double next[16] = {0};
    for (int row = 0; row < 4; row++) for (int column = 0; column < 4; column++)
        for (int k = 0; k < 4; k++) next[row * 4 + column] += left[row * 4 + k] * right[k * 4 + column];
    memcpy(output, next, sizeof(next));
}

static void transform(const double *matrix, double x, double y, double z, double *output) {
    double point[4] = {x, y, z, 1};
    for (int row = 0; row < 4; row++) {
        output[row] = 0;
        for (int k = 0; k < 4; k++) output[row] += matrix[row * 4 + k] * point[k];
    }
}

static uint8_t *logical_buffer(Vm *vm, int selector) { return vm->buffers[selector == 0 ? vm->front : vm->back]; }

static void paint(Vm *vm, int selector, int x, int y, uint32_t color) {
    if (x < 0 || x >= vm->side || y < 0 || y >= vm->side) return;
    int at = (y * vm->side + x) * 3;
    uint8_t *buffer = logical_buffer(vm, selector);
    buffer[at] = (uint8_t)(color >> 16); buffer[at + 1] = (uint8_t)(color >> 8); buffer[at + 2] = (uint8_t)color;
}

static void project(Vm *vm, const double *matrix, int base, int *x, int *y) {
    double point[4];
    transform(matrix, vm->registers[base] / (double)VM_FIXED, vm->registers[base + 1] / (double)VM_FIXED,
        vm->registers[base + 2] / (double)VM_FIXED, point);
    double depth = fmax(.1, vm->perspective + point[2]);
    *x = (int)lround(vm->side / 2.0 + point[0] / depth * vm->side / 2.0);
    *y = (int)lround(vm->side / 2.0 - point[1] / depth * vm->side / 2.0);
}

static int region(int side, double x, double y) {
    return (x < 0 ? 1 : x >= side ? 2 : 0) | (y < 0 ? 4 : y >= side ? 8 : 0);
}

static bool clip_line(int side, int *ix0, int *iy0, int *ix1, int *iy1) {
    double x0 = *ix0, y0 = *iy0, x1 = *ix1, y1 = *iy1;
    for (int guard = 0; guard < 8; guard++) {
        int c0 = region(side, x0, y0), c1 = region(side, x1, y1);
        if (!(c0 | c1)) { *ix0 = (int)lround(x0); *iy0 = (int)lround(y0); *ix1 = (int)lround(x1); *iy1 = (int)lround(y1); return true; }
        if (c0 & c1) return false;
        int outside = c0 ? c0 : c1; double x, y;
        if (outside & 8) { x = x0 + (x1 - x0) * (side - 1 - y0) / (y1 - y0); y = side - 1; }
        else if (outside & 4) { x = x0 + (x1 - x0) * -y0 / (y1 - y0); y = 0; }
        else if (outside & 2) { y = y0 + (y1 - y0) * (side - 1 - x0) / (x1 - x0); x = side - 1; }
        else { y = y0 + (y1 - y0) * -x0 / (x1 - x0); x = 0; }
        if (outside == c0) { x0 = x; y0 = y; } else { x1 = x; y1 = y; }
    }
    return false;
}

static bool line(Vm *vm, int selector, int x0, int y0, int x1, int y1, uint32_t color, int *fuel) {
    if (!clip_line(vm->side, &x0, &y0, &x1, &y1)) return true;
    int dx = abs(x1 - x0), sx = x0 < x1 ? 1 : -1, dy = -abs(y1 - y0), sy = y0 < y1 ? 1 : -1, error = dx + dy;
    for (int guard = 0; guard <= vm->side * 2; guard++) {
        if (--*fuel < 0) return false;
        paint(vm, selector, x0, y0, color);
        if (x0 == x1 && y0 == y1) return true;
        int twice = 2 * error;
        if (twice >= dy) { error += dy; x0 += sx; }
        if (twice <= dx) { error += dx; y0 += sy; }
    }
    return false;
}

static int32_t safe_fixed(double value, bool *ok) {
    if (!isfinite(value)) { *ok = false; return 0; }
    if (value > INT32_MAX) return INT32_MAX;
    if (value < INT32_MIN) return INT32_MIN;
    return (int32_t)llround(value);
}

static void default_senses(Vm *vm) {
    int frame = vm->frame;
    vm->senses[0] = (uint8_t)(frame * 7);
    vm->senses[1] = (uint8_t)((frame / 16 % 4) * 85);
    vm->senses[2] = 255; vm->senses[3] = 128; vm->senses[4] = 192;
    vm->senses[5] = 96; vm->senses[6] = 160; vm->senses[7] = (uint8_t)(frame * 29 + 17);
}

static bool run_frame(Vm *vm, const Instruction *code, int count, int *fuel_used) {
    int pc = 0, fuel = VM_FUEL, calls[VM_MAX_CALL], call_count = 0, stack[VM_MAX_STACK], stack_count = 0, transform_count = 1;
    double matrices[VM_MAX_XFORM][16]; bool halted = false, swapped = false, ok = true;
    identity(matrices[0]);
    while (!halted && ok) {
        if (--fuel < 0 || pc < 0 || pc >= count) return false;
        Instruction in = code[pc]; bool advance = true; int32_t *r = vm->registers;
        switch (in.op) {
            case 0: halted = true; break;
            case 1: r[in.a] = in.imm; break;
            case 2: r[in.a] = r[in.b]; break;
            case 3: r[in.a] = safe_fixed((double)r[in.b] + r[in.c], &ok); break;
            case 4: r[in.a] = safe_fixed((double)r[in.b] - r[in.c], &ok); break;
            case 5: r[in.a] = safe_fixed((double)r[in.b] * r[in.c] / VM_FIXED, &ok); break;
            case 6: if (!r[in.c]) return false; r[in.a] = safe_fixed((double)r[in.b] * VM_FIXED / r[in.c], &ok); break;
            case 7: r[in.a] = r[in.b] < r[in.c] ? VM_FIXED : 0; break;
            case 8: pc = in.imm; advance = false; break;
            case 9: if (r[in.a]) { pc = in.imm; advance = false; } break;
            case 10: if (call_count >= VM_MAX_CALL) return false; calls[call_count++] = pc + 1; pc = in.imm; advance = false; break;
            case 11: if (!call_count) return false; pc = calls[--call_count]; advance = false; break;
            case 12: {
                if (in.a > 1 || fuel < vm->side * vm->side) return false;
                fuel -= vm->side * vm->side; uint8_t *target = logical_buffer(vm, in.a);
                for (int at = 0; at < vm->side * vm->side * 3; at += 3) { target[at] = in.imm >> 16; target[at + 1] = in.imm >> 8; target[at + 2] = in.imm; }
                break;
            }
            case 13: paint(vm, in.a, r[in.b] / VM_FIXED, r[in.c] / VM_FIXED, (uint32_t)in.imm); break;
            case 14: {
                int character = (int)((uint32_t)in.imm >> 24); const char *pattern = character < 128 && glyphs[character] ? glyphs[character] : glyphs[' '];
                int start_x = r[in.b] / VM_FIXED, start_y = r[in.c] / VM_FIXED;
                if (fuel < 35) return false;
                fuel -= 35;
                for (int y = 0; y < 7; y++) for (int x = 0; x < 5; x++) if (pattern[y * 6 + x] == '1') paint(vm, in.a, start_x + x, start_y + y, (uint32_t)in.imm & 0xffffffu);
                break;
            }
            case 15: if (swapped) return false; swapped = true; break;
            case 16: identity(matrices[transform_count - 1]); break;
            case 17: if (transform_count >= VM_MAX_XFORM) return false; memcpy(matrices[transform_count], matrices[transform_count - 1], sizeof(double) * 16); transform_count++; break;
            case 18: if (transform_count <= 1) return false; transform_count--; break;
            case 19: {
                double matrix[16]; identity(matrix); matrix[3] = r[in.a] / (double)VM_FIXED; matrix[7] = r[in.b] / (double)VM_FIXED; matrix[11] = r[in.c] / (double)VM_FIXED;
                multiply(matrices[transform_count - 1], matrices[transform_count - 1], matrix); break;
            }
            case 20: case 21: case 22: {
                double angle = r[in.a] / (double)VM_FIXED * VM_PI * 2;
                double sine = sin(angle), cosine = cos(angle), matrix[16];
                identity(matrix);
                if (in.op == 20) { matrix[5] = cosine; matrix[6] = -sine; matrix[9] = sine; matrix[10] = cosine; }
                else if (in.op == 21) { matrix[0] = cosine; matrix[2] = sine; matrix[8] = -sine; matrix[10] = cosine; }
                else { matrix[0] = cosine; matrix[1] = -sine; matrix[4] = sine; matrix[5] = cosine; }
                multiply(matrices[transform_count - 1], matrices[transform_count - 1], matrix); break;
            }
            case 23: vm->perspective = fmax(.05, fabs(r[in.a] / (double)VM_FIXED)); break;
            case 24: {
                int z = (int)((uint32_t)in.imm >> 24); if (z >= VM_REGS) return false; double point[4];
                transform(matrices[transform_count - 1], r[in.b] / (double)VM_FIXED, r[in.c] / (double)VM_FIXED, r[z] / (double)VM_FIXED, point);
                double depth = fmax(.1, vm->perspective + point[2]);
                paint(vm, in.a, (int)lround(vm->side / 2.0 + point[0] / depth * vm->side / 2.0), (int)lround(vm->side / 2.0 - point[1] / depth * vm->side / 2.0), (uint32_t)in.imm & 0xffffffu); break;
            }
            case 25: {
                if (in.b + 2 >= VM_REGS || in.c + 2 >= VM_REGS) return false;
                int x0, y0, x1, y1;
                project(vm, matrices[transform_count - 1], in.b, &x0, &y0);
                project(vm, matrices[transform_count - 1], in.c, &x1, &y1);
                if (!line(vm, in.a, x0, y0, x1, y1, (uint32_t)in.imm, &fuel)) return false;
                break;
            }
            case 26: {
                int third = (int)((uint32_t)in.imm >> 24); if (in.b + 2 >= VM_REGS || in.c + 2 >= VM_REGS || third + 2 >= VM_REGS) return false;
                int x[3], y[3];
                project(vm, matrices[transform_count - 1], in.b, &x[0], &y[0]);
                project(vm, matrices[transform_count - 1], in.c, &x[1], &y[1]);
                project(vm, matrices[transform_count - 1], third, &x[2], &y[2]);
                for (int edge = 0; edge < 3; edge++) {
                    if (!line(vm, in.a, x[edge], y[edge], x[(edge + 1) % 3], y[(edge + 1) % 3], (uint32_t)in.imm & 0xffffffu, &fuel)) return false;
                }
                break;
            }
            case 27: if (stack_count >= VM_MAX_STACK) return false; stack[stack_count++] = r[in.a]; break;
            case 28: if (!stack_count) return false; r[in.a] = stack[--stack_count]; break;
            case 29: { int address = r[in.b] / VM_FIXED; if (address < 0 || address >= VM_DATA) return false; r[in.a] = vm->data[address] * VM_FIXED; break; }
            case 30: { int address = r[in.a] / VM_FIXED; if (address < 0 || address >= VM_DATA) return false; vm->data[address] = (uint8_t)(r[in.b] / VM_FIXED); break; }
            case 31: r[in.a] = (int32_t)(((uint32_t)vm->senses[in.imm] * VM_FIXED + 127) / 255); break;
            case 32: {
                uint32_t packed = (uint32_t)in.imm, offset = packed & 0xffffu, length = packed >> 16;
                int index = r[in.b] / VM_FIXED;
                if (index < 0 || (uint32_t)index >= length) return false;
                r[in.a] = vm->data[offset + (uint32_t)index] * VM_FIXED;
                break;
            }
            case 33: {
                uint32_t packed = (uint32_t)in.imm, offset = packed & 0xffffu, length = packed >> 16;
                int index = r[in.a] / VM_FIXED;
                if (index < 0 || (uint32_t)index >= length) return false;
                vm->data[offset + (uint32_t)index] = (uint8_t)(r[in.b] / VM_FIXED);
                break;
            }
            default: return false;
        }
        if (advance) pc++;
    }
    if (!ok || !halted || !swapped || call_count || stack_count || transform_count != 1) return false;
    int old_front = vm->front; vm->front = vm->back; vm->back = old_front; vm->frame++;
    *fuel_used = VM_FUEL - fuel; return true;
}

struct PieceVmNative {
    Vm vm;
    Instruction code[VM_MAX_CODE];
    int instruction_count;
    bool external_senses;
};

PieceVmNative *piece_vm_native_create(int resolution, const uint8_t *bytecode, size_t byte_count) {
    if (resolution != 32 && resolution != 64 && resolution != 128 && resolution != 256) return NULL;
    PieceVmNative *runtime = calloc(1, sizeof(*runtime));
    if (!runtime || !decode_bytes(bytecode, byte_count, runtime->code, &runtime->instruction_count)) { free(runtime); return NULL; }
    runtime->vm.side = resolution; runtime->vm.front = 0; runtime->vm.back = 1; runtime->vm.perspective = 1;
    size_t bytes = (size_t)resolution * resolution * 3;
    runtime->vm.buffers[0] = calloc(bytes, 1); runtime->vm.buffers[1] = calloc(bytes, 1);
    if (!runtime->vm.buffers[0] || !runtime->vm.buffers[1]) { piece_vm_native_destroy(runtime); return NULL; }
    return runtime;
}

bool piece_vm_native_step(PieceVmNative *runtime, int *fuel_used) {
    if (!runtime || !fuel_used) return false;
    if (!runtime->external_senses) default_senses(&runtime->vm);
    runtime->external_senses = false;
    return run_frame(&runtime->vm, runtime->code, runtime->instruction_count, fuel_used);
}

bool piece_vm_native_set_senses(PieceVmNative *runtime, const uint8_t *senses, size_t count) {
    if (!runtime || !senses || count != VM_SENSES) return false;
    memcpy(runtime->vm.senses, senses, VM_SENSES);
    runtime->external_senses = true;
    return true;
}

const uint8_t *piece_vm_native_front(const PieceVmNative *runtime) {
    return runtime ? runtime->vm.buffers[runtime->vm.front] : NULL;
}

int piece_vm_native_resolution(const PieceVmNative *runtime) { return runtime ? runtime->vm.side : 0; }

void piece_vm_native_destroy(PieceVmNative *runtime) {
    if (!runtime) return;
    free(runtime->vm.buffers[0]); free(runtime->vm.buffers[1]); free(runtime);
}

#ifndef PIECE_VM_LIBRARY
int main(int argc, char **argv) {
    if (argc != 5) { fprintf(stderr, "usage: %s RESOLUTION FRAMES BYTECODE_HEX OUTPUT_RGB\n", argv[0]); return 2; }
    int side = atoi(argv[1]), frames = atoi(argv[2]);
    if ((side != 32 && side != 64 && side != 128 && side != 256) || frames < 1 || frames > 256) return 2;
    Instruction code[VM_MAX_CODE]; int count = 0;
    if (!decode_hex(argv[3], code, &count)) { fprintf(stderr, "invalid PieceVM bytecode\n"); return 2; }
    Vm vm = {.side = side, .front = 0, .back = 1, .perspective = 1};
    size_t bytes = (size_t)side * side * 3;
    vm.buffers[0] = calloc(bytes, 1); vm.buffers[1] = calloc(bytes, 1);
    if (!vm.buffers[0] || !vm.buffers[1]) return 2;
    int fuel = 0;
    for (int frame = 0; frame < frames; frame++) {
        default_senses(&vm);
        if (!run_frame(&vm, code, count, &fuel)) { fprintf(stderr, "PieceVM fault on frame %d\n", frame + 1); return 1; }
    }
    FILE *output = fopen(argv[4], "wb");
    if (!output || fwrite(vm.buffers[vm.front], 1, bytes, output) != bytes || fclose(output)) { fprintf(stderr, "write failed: %s\n", strerror(errno)); return 2; }
    fprintf(stdout, "piecevm-native instructions=%d frames=%d fuel=%d bytes=%zu\n", count, frames, fuel, bytes);
    free(vm.buffers[0]); free(vm.buffers[1]); return 0;
}
#endif
