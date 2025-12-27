{
  "targets": [
    {
      "target_name": "osr-gpu",
      "sources": ["src/napi_utils.h"],
      "include_dirs": [
        "<!@(node -p \"require('node-addon-api').include\")"
      ],
      "defines": ["NAPI_DISABLE_CPP_EXCEPTIONS"],
      "conditions": [
        ["OS=='mac'", {
          "sources": ["src/binding_mac.mm"],
          "xcode_settings": {
            "GCC_ENABLE_CPP_EXCEPTIONS": "YES",
            "CLANG_CXX_LANGUAGE_STANDARD": "c++17",
            "MACOSX_DEPLOYMENT_TARGET": "10.15"
          },
          "link_settings": {
            "libraries": [
              "-framework OpenGL",
              "-framework IOSurface",
              "-framework Cocoa"
            ]
          }
        }],
        ["OS=='win'", {
          "sources": ["src/binding_win.cc"],
          "link_settings": {
            "libraries": ["dxgi.lib", "d3d11.lib", "dxguid.lib"]
          }
        }]
      ]
    }
  ]
}
