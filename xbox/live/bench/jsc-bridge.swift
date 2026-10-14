// What a JavaScriptCore -> Swift call costs on this Mac, the way main.swift
// binds them: one triangle3d (12 doubles) vs one disc3d (7 doubles).
import Foundation
import JavaScriptCore

let context = JSContext()!
var sink = [Float](); sink.reserveCapacity(1 << 20)
let triangle3d: @convention(block) (Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double) -> Void = {
    a, b, c, d, e, f, g, h, i, r, gg, bb in
    sink.append(Float(a)); sink.append(Float(b)); sink.append(Float(c)); sink.append(Float(r))
    if sink.count > 1 << 19 { sink.removeAll(keepingCapacity: true) }
}
let disc3d: @convention(block) (Double, Double, Double, Double, Double, Double, Double) -> Void = {
    x, y, z, radius, r, g, b in
    sink.append(Float(x)); sink.append(Float(radius))
    if sink.count > 1 << 19 { sink.removeAll(keepingCapacity: true) }
}
context.setObject(triangle3d, forKeyedSubscript: "triangle3d" as NSString)
let triangles3d: @convention(block) (JSValue, Int32) -> Void = { buffer, count in
    guard let ctx = buffer.context?.jsGlobalContextRef,
          let object = JSValueToObject(ctx, buffer.jsValueRef, nil),
          let bytes = JSObjectGetTypedArrayBytesPtr(ctx, object, nil) else { return }
    let values = bytes.assumingMemoryBound(to: Float.self)
    for i in 0..<Int(count) {
        let at = i * 12
        sink.append(values[at]); sink.append(values[at + 1]); sink.append(values[at + 2]); sink.append(values[at + 9])
    }
    if sink.count > 1 << 19 { sink.removeAll(keepingCapacity: true) }
}
context.setObject(triangles3d, forKeyedSubscript: "triangles3d" as NSString)
context.setObject(disc3d, forKeyedSubscript: "disc3d" as NSString)
context.evaluateScript("""
function tris(n) { for (let i = 0; i < n; i++) triangle3d(i, i + 1, -1, i + 2, i, -1, i, i + 3, -1, 200, 100, 50); }
const faces = new Float32Array(2048 * 12);
let count = 0;
function put(x1, y1, z1, x2, y2, z2, x3, y3, z3, r, g, b) {
  if (count === 2048) { triangles3d(faces, count); count = 0; }
  const at = count * 12;
  faces[at] = x1; faces[at + 1] = y1; faces[at + 2] = z1; faces[at + 3] = x2; faces[at + 4] = y2; faces[at + 5] = z2;
  faces[at + 6] = x3; faces[at + 7] = y3; faces[at + 8] = z3; faces[at + 9] = r; faces[at + 10] = g; faces[at + 11] = b;
  count++;
}
function batched(n) { for (let i = 0; i < n; i++) put(i, i + 1, -1, i + 2, i, -1, i, i + 3, -1, 200, 100, 50); if (count) { triangles3d(faces, count); count = 0; } }
function discs(n) { for (let i = 0; i < n; i++) disc3d(i, i + 1, -1, 12, 200, 100, 50); }
""")
func time(_ name: String, _ calls: Int, _ frames: Int) -> Double {
    let fn = context.objectForKeyedSubscript(name)!
    _ = fn.call(withArguments: [calls])
    let start = DispatchTime.now().uptimeNanoseconds
    for _ in 0..<frames { _ = fn.call(withArguments: [calls]) }
    return Double(DispatchTime.now().uptimeNanoseconds - start) / 1e6 / Double(frames)
}
for _ in 0..<3 {
    let t = time("tris", 2700, 60), d = time("discs", 150, 60), b = time("batched", 2700, 60)
    print(String(format: "2700 faces batched through triangles3d: %.3f ms/frame", b))
    print(String(format: "2700 triangle3d calls: %.3f ms/frame (%.3f us each); 150 disc3d calls: %.3f ms/frame (%.3f us each)",
                 t, t * 1000 / 2700, d, d * 1000 / 150))
}
