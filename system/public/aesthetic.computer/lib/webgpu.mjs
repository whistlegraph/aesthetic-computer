// 🎨 WebGPU Renderer
// A 2D GPU-accelerated renderer for aesthetic.computer

/* #region 📚 Notes
  - Message-based architecture matching existing disk → bios pattern
  - Handles basic 2D primitives: clear, line, circle, rect
  - Uses WebGPU for hardware acceleration
  - Falls back gracefully if WebGPU unavailable
#endregion */

let device = null;
let context = null;
let canvasFormat = null;
let renderPipeline = null;
let linePipeline = null;
let isInitialized = false;
let canvas = null;

// Command queue for batching
const commandQueue = [];

// Current drawing color
let currentColor = [255, 255, 255, 255];

// 🔧 Initialize WebGPU
async function init(targetCanvas) {
  if (isInitialized) return true;
  
  console.log("🎨 Initializing WebGPU renderer...");
  
  // Check for WebGPU support
  if (!navigator.gpu) {
    console.warn("⚠️ WebGPU not supported in this browser");
    return false;
  }
  
  try {
    // Request adapter
    const adapter = await navigator.gpu.requestAdapter();
    if (!adapter) {
      console.warn("⚠️ No WebGPU adapter found");
      return false;
    }
    
    // Request device
    device = await adapter.requestDevice();
    
    // Store canvas reference
    canvas = targetCanvas;
    
    // Configure canvas context
    context = canvas.getContext("webgpu");
    if (!context) {
      console.warn("⚠️ Failed to get WebGPU context from canvas");
      return false;
    }
    
    canvasFormat = navigator.gpu.getPreferredCanvasFormat();
    
    context.configure({
      device,
      format: canvasFormat,
      alphaMode: "premultiplied",
    });
    
    // Create basic render pipeline for clear operations
    await createClearPipeline();
    
    // Create line rendering pipeline
    await createLinePipeline();
    
    isInitialized = true;
    console.log("✅ WebGPU renderer initialized");
    return true;
  } catch (error) {
    console.error("❌ Failed to initialize WebGPU:", error);
    return false;
  }
}

// 🎨 Create pipeline for clear operations
async function createClearPipeline() {
  // Simple shader that fills the screen with a color
  const shaderModule = device.createShaderModule({
    label: "Clear shader",
    code: `
      struct VertexOutput {
        @builtin(position) position: vec4f,
      }
      
      @vertex
      fn vertexMain(@builtin(vertex_index) vertexIndex: u32) -> VertexOutput {
        var output: VertexOutput;
        
        // Full-screen triangle
        var pos = array<vec2f, 3>(
          vec2f(-1.0, -1.0),
          vec2f(3.0, -1.0),
          vec2f(-1.0, 3.0)
        );
        
        output.position = vec4f(pos[vertexIndex], 0.0, 1.0);
        return output;
      }
      
      @group(0) @binding(0) var<uniform> color: vec4f;
      
      @fragment
      fn fragmentMain() -> @location(0) vec4f {
        return color;
      }
    `,
  });
  
  renderPipeline = device.createRenderPipeline({
    label: "Clear pipeline",
    layout: "auto",
    vertex: {
      module: shaderModule,
      entryPoint: "vertexMain",
    },
    fragment: {
      module: shaderModule,
      entryPoint: "fragmentMain",
      targets: [{
        format: canvasFormat,
        blend: {
          color: {
            srcFactor: "src-alpha",
            dstFactor: "one-minus-src-alpha",
            operation: "add",
          },
          alpha: {
            srcFactor: "one",
            dstFactor: "one-minus-src-alpha",
            operation: "add",
          },
        },
      }],
    },
  });
}

// 🎨 Create pipeline for line rendering
async function createLinePipeline() {
  const shaderModule = device.createShaderModule({
    label: "Line shader",
    code: `
      struct Uniforms {
        color: vec4f,
        resolution: vec2f,
        _padding: vec2f,  // Padding to meet 32-byte minimum
      }
      
      struct VertexInput {
        @location(0) position: vec2f,
      }
      
      struct VertexOutput {
        @builtin(position) position: vec4f,
      }
      
      @group(0) @binding(0) var<uniform> uniforms: Uniforms;
      
      @vertex
      fn vertexMain(input: VertexInput) -> VertexOutput {
        var output: VertexOutput;
        
        // Convert from pixel coordinates to clip space (-1 to 1)
        let clipX = (input.position.x / uniforms.resolution.x) * 2.0 - 1.0;
        let clipY = 1.0 - (input.position.y / uniforms.resolution.y) * 2.0;
        
        output.position = vec4f(clipX, clipY, 0.0, 1.0);
        return output;
      }
      
      @fragment
      fn fragmentMain() -> @location(0) vec4f {
        return uniforms.color;
      }
    `,
  });
  
  linePipeline = device.createRenderPipeline({
    label: "Line pipeline",
    layout: "auto",
    vertex: {
      module: shaderModule,
      entryPoint: "vertexMain",
      buffers: [{
        arrayStride: 8, // 2 floats * 4 bytes
        attributes: [{
          shaderLocation: 0,
          offset: 0,
          format: "float32x2",
        }],
      }],
    },
    fragment: {
      module: shaderModule,
      entryPoint: "fragmentMain",
      targets: [{
        format: canvasFormat,
        blend: {
          color: {
            srcFactor: "src-alpha",
            dstFactor: "one-minus-src-alpha",
            operation: "add",
          },
          alpha: {
            srcFactor: "one",
            dstFactor: "one-minus-src-alpha",
            operation: "add",
          },
        },
      }],
    },
    primitive: {
      topology: "line-list",
      stripIndexFormat: undefined,
    },
  });
}

// 🎨 Handle incoming commands from disk
function handleCommand(command) {
  if (!isInitialized) {
    console.warn("⚠️ WebGPU not initialized, ignoring command:", command);
    return;
  }
  
  switch (command.type) {
    case "clear":
      executeClear(command.color);
      break;
    case "line":
      executeLine(command.x1, command.y1, command.x2, command.y2, command.color);
      break;
    case "render":
      render();
      break;
    default:
      console.warn("⚠️ Unknown WebGPU command:", command.type);
  }
}

// 🎨 Execute clear command
function executeClear(color) {
  // Make canvas visible and ensure it's sized correctly
  if (canvas.style.display === "none") {
    canvas.style.display = "block";
    console.log("🎨 WebGPU canvas now visible");
  }
  
  // Normalize color to [0, 1] range
  const r = color[0] / 255;
  const g = color[1] / 255;
  const b = color[2] / 255;
  const a = color[3] !== undefined ? color[3] / 255 : 1.0;
  
  console.log(`🎨 WebGPU clear: rgba(${color[0]}, ${color[1]}, ${color[2]}, ${color[3] || 255})`);
  
  // Simple clear using render pass clearValue
  const encoder = device.createCommandEncoder();
  
  const pass = encoder.beginRenderPass({
    colorAttachments: [{
      view: context.getCurrentTexture().createView(),
      loadOp: "clear",
      storeOp: "store",
      clearValue: { r, g, b, a },
    }],
  });
  
  pass.end();
  
  // Submit commands
  device.queue.submit([encoder.finish()]);
}

// 🎨 Execute line command
function executeLine(x1, y1, x2, y2, color) {
  // Make canvas visible
  if (canvas.style.display === "none") {
    canvas.style.display = "block";
  }
  
  // Normalize color to [0, 1] range
  const r = color[0] / 255;
  const g = color[1] / 255;
  const b = color[2] / 255;
  const a = color[3] !== undefined ? color[3] / 255 : 1.0;
  
  // Create vertex buffer with line endpoints in pixel coordinates
  const vertices = new Float32Array([
    x1, y1,
    x2, y2,
  ]);
  
  const vertexBuffer = device.createBuffer({
    size: vertices.byteLength,
    usage: GPUBufferUsage.VERTEX | GPUBufferUsage.COPY_DST,
  });
  device.queue.writeBuffer(vertexBuffer, 0, vertices);
  
  // Create uniform buffer for color and resolution
  const uniformData = new Float32Array([
    r, g, b, a,                    // color (16 bytes)
    canvas.width, canvas.height,   // resolution (8 bytes)
    0, 0,                          // padding (8 bytes)
  ]);
  
  const uniformBuffer = device.createBuffer({
    size: 32, // Fixed size to match minimum binding requirement
    usage: GPUBufferUsage.UNIFORM | GPUBufferUsage.COPY_DST,
  });
  device.queue.writeBuffer(uniformBuffer, 0, uniformData);
  
  // Create bind group
  const bindGroup = device.createBindGroup({
    layout: linePipeline.getBindGroupLayout(0),
    entries: [{
      binding: 0,
      resource: { buffer: uniformBuffer },
    }],
  });
  
  // Create command encoder
  const encoder = device.createCommandEncoder();
  
  const pass = encoder.beginRenderPass({
    colorAttachments: [{
      view: context.getCurrentTexture().createView(),
      loadOp: "load", // Preserve existing content
      storeOp: "store",
    }],
  });
  
  pass.setPipeline(linePipeline);
  pass.setVertexBuffer(0, vertexBuffer);
  pass.setBindGroup(0, bindGroup);
  pass.draw(2); // 2 vertices for a line
  pass.end();
  
  // Submit commands
  device.queue.submit([encoder.finish()]);
  
  // Cleanup temporary buffers
  vertexBuffer.destroy();
  uniformBuffer.destroy();
}

// 🎨 Render frame (for future batching)
function render() {
  // Future: batch multiple primitives here
  console.log("🎨 WebGPU render frame");
}

// 🎨 Resize canvas to match display dimensions
function resize(width, height) {
  if (!canvas) return;
  
  if (canvas.width !== width || canvas.height !== height) {
    canvas.width = width;
    canvas.height = height;
    console.log(`🎨 WebGPU canvas resized to ${width}x${height}`);
  }
}

// 🎨 Cleanup
function destroy() {
  if (device) {
    device.destroy();
    device = null;
  }
  isInitialized = false;
  console.log("🎨 WebGPU renderer destroyed");
}

export { init, handleCommand, resize, destroy };
