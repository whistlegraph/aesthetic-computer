#ifndef NAPI_UTILS_H
#define NAPI_UTILS_H

#include <napi.h>

// Utility macro to get property from object
#define NAPI_GET_PROPERTY_VALUE(obj, name) \
  obj.As<Napi::Object>().Get(name)

// Utility macro to check if property exists
#define NAPI_HAS_PROPERTY(obj, name) \
  obj.As<Napi::Object>().Has(name)

#endif // NAPI_UTILS_H
