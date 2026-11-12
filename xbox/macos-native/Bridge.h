// The desk build's one bridging header (xbox/tools/build-macos-app.sh). The
// App Store project still bridges PoolDecals.h alone and builds without
// SCENE4, so it never needs Scene4Bridge.
#include "PoolDecals.h"
#include "Scene4Bridge.h"
