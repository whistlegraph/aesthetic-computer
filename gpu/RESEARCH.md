# WebGPU Renderer Research

## Current Rendering Pipeline

The current rendering pipeline in `aesthetic.computer` is a distributed system split between the "Disk" (Worker) and the "BIOS" (Main Thread).

### 1. Disk (Client/Worker)
Located in `system/public/aesthetic.computer/lib/disk.mjs`.

- **Role:** Acts as the application logic or "kernel". It generates graphics commands but does not execute them directly.
- **Mechanism:**
    - **`gpu` Object:** Exposes a `message` function (Line ~2936) that sends `gpu-event` messages to the host.
    - **`graph` Function:** (Line ~4460) Serializes 3D forms (vertices, colors, transforms) and sends them via a `forms` message.
    - **`send` Function:** (Line ~7742) Wraps `postMessage` to transmit data to the main thread.

### 2. BIOS (Host/Main Thread)
Located in `system/public/aesthetic.computer/bios.mjs`.

- **Role:** The host environment that manages the display and I/O.
- **Mechanism:**
    - **`receivedChange`:** (Line ~2938) A massive switch statement that handles incoming messages from the worker.
    - **`gpu-event` Handler:** (Line ~10310) Routes `gpu-event` messages to the `ThreeD` renderer:
      ```javascript
      if (type === "gpu-event") {
        ThreeD?.handleEvent(content);
        return;
      }
      ```
    - **Renderers:**
        - **`TwoD`:** Currently disabled/undefined.
        - **`ThreeD`:** Dynamically imported from `./lib/3d.mjs`.

## Proposed WebGPU Architecture

To implement a WebGPU renderer, we should follow the existing message-passing pattern but create a dedicated pipeline for WebGPU commands.

### 1. New Renderer Module
Create a new module `system/public/aesthetic.computer/lib/webgpu.mjs` (or similar) that initializes the WebGPU context and handles rendering.

- **Initialization:** Request `navigator.gpu.requestAdapter()` and `adapter.requestDevice()`.
- **Canvas:** It will need access to a canvas element (likely passed from `bios.mjs`).

### 2. Disk Extensions (`disk.mjs`)
Extend the API available to pieces to include WebGPU commands.

- **`webgpu` Object:** Add a `webgpu` object to the API exposed to pieces.
- **Command Serialization:** Implement methods to serialize WebGPU commands (buffers, shaders, pipelines) into a format suitable for `postMessage`.
    - *Note:* WebGPU relies heavily on `ArrayBuffer`s. These are transferable objects, which is efficient for `postMessage`.

### 3. BIOS Integration (`bios.mjs`)
Update the host to handle WebGPU messages.

- **Import:** Import the new `WebGPU` renderer.
- **Message Handler:** Add a handler for `webgpu-command` (or similar) in `receivedChange`.
  ```javascript
  if (type === "webgpu-command") {
    WebGPU?.handleCommand(content);
    return;
  }
  ```
- **Initialization:** Initialize the `WebGPU` renderer in the `boot` sequence, passing the necessary canvas/context.

## Implementation Plan

1.  **Scaffold `webgpu.mjs`:** Create the basic class/module structure for the renderer.
2.  **Update `bios.mjs`:**
    - Import `webgpu.mjs`.
    - Initialize it (request device, configure context).
    - Add the message handler.
3.  **Update `disk.mjs`:**
    - Add the `webgpu` API surface.
    - Implement the `send` logic for WebGPU commands.
4.  **Proof of Concept:** Create a simple piece that sends a "clear screen" command via the new pipeline.

## Considerations

- **Transferables:** Ensure that large data buffers (vertices, textures) are sent as "transferables" in `postMessage` to avoid copying overhead.
- **State Management:** The `WebGPU` renderer on the main thread will need to maintain the state (pipelines, buffers, textures) that the worker refers to by ID or handle.
- **Async Nature:** WebGPU initialization is async. The `disk` may need to wait for a "ready" signal from the `bios` before issuing commands.
