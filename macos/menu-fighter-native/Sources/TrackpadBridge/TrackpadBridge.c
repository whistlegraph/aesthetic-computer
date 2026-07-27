#include "TrackpadBridge.h"
#include <CoreFoundation/CoreFoundation.h>
#include <dlfcn.h>
#include <os/lock.h>
#include <stdlib.h>

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
static MFCornerPoseCallback cornerClient;
static MFTrackpadInputCallback inputClient;
static void (*registerCallback)(MTDeviceRef, MTCallback);
static void (*unregisterCallback)(MTDeviceRef, MTCallback);
static void (*startDevice)(MTDeviceRef, int);
static void (*stopDevice)(MTDeviceRef);
static bool *devicePoses;
static CFIndex deviceCount;
static bool reportedPose;
static os_unfair_lock poseLock = OS_UNFAIR_LOCK_INIT;

static int contacts(MTDeviceRef device, MTTouch *touches, int count, double time, int frame) {
  (void)device; (void)time; (void)frame;
  bool corner[4] = {false, false, false, false};
  int active = 0;
  float centroidX = 0, centroidY = 0;
  for (int i = 0; i < count; i++) {
    float x = touches[i].normalized.position.x, y = touches[i].normalized.position.y;
    if (touches[i].state == 7) continue;
    active++;
    centroidX += x; centroidY += y;
    if (x < .30f && y < .30f) corner[0] = true;
    if (x > .70f && y < .30f) corner[1] = true;
    if (x < .30f && y > .70f) corner[2] = true;
    if (x > .70f && y > .70f) corner[3] = true;
  }
  const bool pose = active >= 4 && corner[0] && corner[1] && corner[2] && corner[3];
  bool combined = pose;
  bool notify = false;

  os_unfair_lock_lock(&poseLock);
  if (devicePoses && devices) {
    combined = false;
    for (CFIndex i = 0; i < deviceCount; i++) {
      if ((MTDeviceRef)CFArrayGetValueAtIndex(devices, i) == device) devicePoses[i] = pose;
      combined = combined || devicePoses[i];
    }
  }
  if (combined != reportedPose) {
    reportedPose = combined;
    notify = true;
  }
  os_unfair_lock_unlock(&poseLock);

  if (active > 0) { centroidX /= active; centroidY /= active; }
  if (notify && cornerClient) cornerClient(combined);
  if (inputClient) inputClient(centroidX, centroidY, active);
  return 0;
}

static bool startWatcher(void) {
  library = dlopen("/System/Library/PrivateFrameworks/MultitouchSupport.framework/MultitouchSupport", RTLD_NOW);
  if (!library) return false;
  CFArrayRef (*createList)(void) = dlsym(library, "MTDeviceCreateList");
  registerCallback = dlsym(library, "MTRegisterContactFrameCallback");
  unregisterCallback = dlsym(library, "MTUnregisterContactFrameCallback");
  startDevice = dlsym(library, "MTDeviceStart"); stopDevice = dlsym(library, "MTDeviceStop");
  if (!createList || !registerCallback || !startDevice) return false;
  devices = createList();
  deviceCount = CFArrayGetCount(devices);
  devicePoses = calloc((size_t)deviceCount, sizeof(bool));
  reportedPose = false;
  for (CFIndex i = 0; i < deviceCount; i++) {
    MTDeviceRef device = (MTDeviceRef)CFArrayGetValueAtIndex(devices, i);
    registerCallback(device, contacts); startDevice(device, 0);
  }
  return deviceCount > 0;
}

bool MFStartCornerPoseWatcher(MFCornerPoseCallback callback) {
  cornerClient = callback;
  return startWatcher();
}

bool MFStartTrackpadInputWatcher(MFTrackpadInputCallback callback) {
  inputClient = callback;
  return startWatcher();
}

void MFStopCornerPoseWatcher(void) {
  if (devices) for (CFIndex i = 0; i < CFArrayGetCount(devices); i++) {
    MTDeviceRef device = (MTDeviceRef)CFArrayGetValueAtIndex(devices, i);
    if (unregisterCallback) unregisterCallback(device, contacts); if (stopDevice) stopDevice(device);
  }
  free(devicePoses); devicePoses = 0; deviceCount = 0; reportedPose = false;
  if (devices) CFRelease(devices); devices = 0; cornerClient = 0; inputClient = 0;
  if (library) dlclose(library); library = 0;
}
