import { withCDP } from './cdp.mjs';

await withCDP(async (cdp) => {
  const entries = await cdp.eval(`
    (() => {
      const entries = performance.getEntriesByType('resource')
        .filter(e => e.name.includes('.mjs'))
        .map(e => ({
          name: e.name.split('/').pop(),
          duration: Math.round(e.duration),
          wait: Math.round(e.responseStart - e.requestStart),
          download: Math.round(e.responseEnd - e.responseStart),
          parse: Math.round(e.duration - (e.responseStart - e.requestStart) - (e.responseEnd - e.responseStart))
        }))
        .sort((a,b) => b.duration - a.duration)
        .slice(0, 20);
      return entries;
    })()
  `);

  console.log('\n📊 Slowest Module Loads:\n');
  console.log('Module                     Total    Wait  Download    Parse');
  console.log('─'.repeat(70));
  
  entries.forEach(r => {
    console.log(
      `${r.name.padEnd(25)} ` +
      `${String(r.duration).padStart(5)}ms ` +
      `${String(r.wait).padStart(4)}ms ` +
      `${String(r.download).padStart(8)}ms ` +
      `${String(r.parse).padStart(8)}ms`
    );
  });

  const totalParse = entries.reduce((sum, r) => sum + r.parse, 0);
  const totalDownload = entries.reduce((sum, r) => sum + r.download, 0);
  const totalWait = entries.reduce((sum, r) => sum + r.wait, 0);

  console.log('─'.repeat(70));
  console.log(`Total network time: ${totalWait + totalDownload}ms (wait: ${totalWait}ms, download: ${totalDownload}ms)`);
  console.log(`Total parse time: ${totalParse}ms`);
  console.log(`\n💡 Parse time is ${Math.round(totalParse / (totalWait + totalDownload))}x slower than network`);
}, { 
  targetUrl: 'https://localhost:8888/prompt',
  verbose: true 
});
