#pragma once
#include <stdbool.h>

typedef void (*MFCornerPoseCallback)(bool active);
bool MFStartCornerPoseWatcher(MFCornerPoseCallback callback);
void MFStopCornerPoseWatcher(void);
