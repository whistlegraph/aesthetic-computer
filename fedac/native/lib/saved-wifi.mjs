// Connect only to visible saved networks; let the native WPA/DHCP attempt finish.
export function savedWifi() {
  let nextScan=0,scanAt=0,credentials=[],loaded=false;
  const tried=new Map();
  return function step({wifi,system},now=Date.now()) {
    if(!wifi||wifi.connected)return;
    if(!loaded){
      try{const value=JSON.parse(system.readFile('/mnt/wifi_creds.json')||'[]');credentials=Array.isArray(value)?value.filter(c=>typeof c.ssid==='string'&&typeof c.pass==='string'):[];}catch{credentials=[];}
      loaded=true;
    }
    // OFF=0, SCANNING=1, SCAN_DONE=2, CONNECTING=3, CONNECTED=4, FAILED=5.
    if(wifi.state===1||wifi.state===3||wifi.state===4)return;
    if(now>=nextScan){wifi.scan?.();scanAt=now;nextScan=now+15000;return;}
    if(now-scanAt<1500)return;
    const candidates=(wifi.networks||[])
      .filter(n=>credentials.some(c=>c.ssid===n.ssid)&&now-(tried.get(n.ssid)??-Infinity)>=65000)
      .sort((a,b)=>(Number(credentials.find(c=>c.ssid===b.ssid)?.priority)||0)-(Number(credentials.find(c=>c.ssid===a.ssid)?.priority)||0)||b.signal-a.signal);
    if(!candidates.length)return;
    const cred=credentials.find(c=>c.ssid===candidates[0].ssid);
    tried.set(cred.ssid,now);wifi.connect(cred.ssid,cred.pass);nextScan=now+65000;
  };
}
