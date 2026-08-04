#include "TrackpadBridge.h"
#include <CoreFoundation/CoreFoundation.h>
#include <dlfcn.h>

typedef void *MTDeviceRef;
typedef struct { float x, y; } MTPoint;
typedef struct { MTPoint position, velocity; } MTVector;
typedef struct {
  int frame; double timestamp; int identifier, state, fingerID, handID;
  MTVector normalized; float size; int zero1; float angle, majorAxis, minorAxis;
  MTVector absolute; int zero2, zero3; float density;
} MTTouch;
typedef int (*MTCallback)(MTDeviceRef, MTTouch *, int, double, int);

static void *library;
static CFArrayRef devices;
static MFCornerPoseCallback client;
static void (*registerCallback)(MTDeviceRef, MTCallback);
static void (*unregisterCallback)(MTDeviceRef, MTCallback);
static void (*startDevice)(MTDeviceRef, int);
static void (*stopDevice)(MTDeviceRef);

static int contacts(MTDeviceRef device, MTTouch *touches, int count, double time, int frame) {
  (void)device; (void)time; (void)frame;
  bool corner[4] = {false, false, false, false};
  int active = 0;
  for (int i = 0; i < count; i++) {
    float x = touches[i].normalized.position.x, y = touches[i].normalized.position.y;
    if (touches[i].state == 7) continue;
    active++;
    if (x < .30f && y < .30f) corner[0] = true;
    if (x > .70f && y < .30f) corner[1] = true;
    if (x < .30f && y > .70f) corner[2] = true;
    if (x > .70f && y > .70f) corner[3] = true;
  }
  if (client) client(active == 4 && corner[0] && corner[1] && corner[2] && corner[3]);
  return 0;
}

bool MFStartCornerPoseWatcher(MFCornerPoseCallback callback) {
  library = dlopen("/System/Library/PrivateFrameworks/MultitouchSupport.framework/MultitouchSupport", RTLD_NOW);
  if (!library) return false;
  CFArrayRef (*createList)(void) = dlsym(library, "MTDeviceCreateList");
  registerCallback = dlsym(library, "MTRegisterContactFrameCallback");
  unregisterCallback = dlsym(library, "MTUnregisterContactFrameCallback");
  startDevice = dlsym(library, "MTDeviceStart"); stopDevice = dlsym(library, "MTDeviceStop");
  if (!createList || !registerCallback || !startDevice) return false;
  client = callback; devices = createList();
  for (CFIndex i = 0; i < CFArrayGetCount(devices); i++) {
    MTDeviceRef device = (MTDeviceRef)CFArrayGetValueAtIndex(devices, i);
    registerCallback(device, contacts); startDevice(device, 0);
  }
  return CFArrayGetCount(devices) > 0;
}

void MFStopCornerPoseWatcher(void) {
  if (devices) for (CFIndex i = 0; i < CFArrayGetCount(devices); i++) {
    MTDeviceRef device = (MTDeviceRef)CFArrayGetValueAtIndex(devices, i);
    if (unregisterCallback) unregisterCallback(device, contacts); if (stopDevice) stopDevice(device);
  }
  if (devices) CFRelease(devices); devices = 0; client = 0;
  if (library) dlclose(library); library = 0;
}
