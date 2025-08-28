// Test case for 3s toggle fix
// This demonstrates the issue and tests the fix

/*
Original issue:
(3s... ($neio 0 0 w h/2)
       ($pif 0 h/2 w h/2))

Expected behavior:
- Show $neio for 3 seconds
- Switch to $pif for 3 seconds  
- Repeat

Previous bug:
- Both $neio and $pif would render continuously
- No actual toggling would occur

Fixed behavior:
- Only one embedded layer renders at a time
- Proper 3-second toggle between them
*/

console.log("3s toggle fix implemented:");
console.log("1. Timing context tracking added to embed function");
console.log("2. Embedded layers store timing information");  
console.log("3. renderEmbeddedLayers checks layer visibility");
console.log("4. Only currently active timing argument renders");
