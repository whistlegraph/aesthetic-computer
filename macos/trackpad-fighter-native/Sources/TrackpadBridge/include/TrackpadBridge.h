#pragma once
#include <stdbool.h>

typedef void (*MFCornerPoseCallback)(bool active);
typedef void (*MFTrackpadInputCallback)(float x, float y, int contacts);
bool MFStartCornerPoseWatcher(MFCornerPoseCallback callback);
bool MFStartTrackpadInputWatcher(MFTrackpadInputCallback callback);
void MFStopCornerPoseWatcher(void);
