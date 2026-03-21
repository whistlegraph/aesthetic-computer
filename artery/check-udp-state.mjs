// Check browser UDP/ICE state for debugging
import { withCDP } from './cdp.mjs';

console.log('🔍 Checking browser WebRTC/UDP state...\n');

await withCDP(async (cdp) => {
  // Enable UDP logging
  await cdp.evaluate(`localStorage.setItem('ac-logs-udp', 'true')`);
  console.log('✅ Enabled UDP logging in browser');
  
  // Check current URL
  const url = await cdp.evaluate('location.href');
  console.log('📍 Current URL:', url);
  
  // Check UDP connection state
  const udpState = await cdp.evaluate(`JSON.stringify({
    connected: window.disk?.udp?.connected || false,
    hasChannel: !!window.disk?.udp?.channel,
  })`);
  console.log('🩰 UDP State:', udpState);
  
  // Check if session server sent turnHost
  const devInfo = await cdp.evaluate(`JSON.stringify({
    turnHost: window.disk?.devInfo?.hostIp || 'not set',
    host: window.disk?.devInfo?.host || 'not set',
    mode: window.disk?.devInfo?.mode || 'not set',
  })`);
  console.log('📱 Dev Info:', devInfo);
  
  // Navigate to 1v1 to trigger UDP connection
  console.log('\n🎮 Jumping to 1v1 piece to test UDP...');
  await cdp.jump('1v1');
  
  // Wait for connection attempt
  await new Promise(r => setTimeout(r, 3000));
  
  // Check again
  const udpState2 = await cdp.evaluate(`JSON.stringify({
    connected: window.disk?.udp?.connected || false,
    hasChannel: !!window.disk?.udp?.channel,
    piece: window.disk?.currentPiece || 'unknown',
  })`);
  console.log('🩰 UDP State after jump:', udpState2);
  
  console.log('\n💡 Check browser console (F12) for detailed WebRTC logs');
  console.log('   Look for: ICE connection state, TURN server connection, candidate errors');
});

process.exit(0);
