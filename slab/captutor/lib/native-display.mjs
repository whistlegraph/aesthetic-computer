// Pixel dimensions come from CoreGraphics, never browser zoom or a target preset.
export function nativeDisplayFormat(geometry) {
 for(const key of ['width','height','pixelWidth','pixelHeight'])if(!Number.isInteger(geometry?.[key])||geometry[key]<=0)throw Error('Invalid native display geometry');
 if(geometry.pixelWidth%2||geometry.pixelHeight%2)throw Error('Native H.264 capture requires even dimensions; do not silently resize');
 return {out:{w:geometry.pixelWidth,h:geometry.pixelHeight},nativeResolution:true};
}
export function assertNativePixels(source,target){if(source.w!==target.w||source.h!==target.h)throw Error(`Native capture changed: expected ${target.w}x${target.h}, got ${source.w}x${source.h}`);}
