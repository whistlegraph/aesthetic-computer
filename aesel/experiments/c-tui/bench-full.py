#!/usr/bin/env python3
"""Compare the same complete Aesel engine, with and without the C PTY host."""
import argparse,hashlib,importlib.util,json,platform,random,shutil,sys,tempfile,threading,time
from pathlib import Path
HERE=Path(__file__).resolve().parent;ROOT=HERE.parents[1]
sys.path.insert(0,str(ROOT/'test'))
spec=importlib.util.spec_from_file_location('bench',ROOT/'bin/bench-tui.py')
bench=importlib.util.module_from_spec(spec);spec.loader.exec_module(bench)

def main():
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--runs',type=int,default=10);parser.add_argument('--warmup',type=int,default=2)
    parser.add_argument('--settled',action='store_true',help='wait for provider readiness before timing Enter')
    parser.add_argument('--installed',action='store_true',help='include the full shell launcher and installed symlink')
    parser.add_argument('--out',type=Path,required=True);args=parser.parse_args()
    if args.runs<1 or args.warmup<0:parser.error('invalid sample counts')
    original_setup=bench.setup;OriginalTerminal=bench.Terminal;timing=[]
    def setup(target,root,endpoint,**options):
        command,env=original_setup('aesel-codex-launcher',root,endpoint,runtime='bun',launcher='ac')
        if target=='aesel-native':
            if args.installed:
                executable=root/'bin/aesel-native';executable.symlink_to(HERE/'full')
                command=[str(executable),*command[1:]]
            else:command=[str(HERE/'.build/aesel-native'),'--',*command]
            env['AESEL_NATIVE_TRACE']=str(root/'native-timing.json')
        return command,env
    class Terminal(OriginalTerminal):
        def __init__(self,command,env,*rest):
            self.native_trace=env.get('AESEL_NATIVE_TRACE');self.startup_trace=env.get('AESEL_STARTUP_TRACE');super().__init__(command,env,*rest)
        def send(self,data):
            super().send(data)
            if args.settled and data=='benchmark':
                deadline=time.monotonic()+15
                while time.monotonic()<deadline:
                    self.pump(.01)
                    try:
                        if any(p['phase']=='engine-connected' for p in json.loads(Path(self.startup_trace).read_text())):return
                    except (OSError,ValueError):pass
                raise RuntimeError('Provider did not reach ready state')
        def close(self):
            if self.native_trace and self.child.poll() is None:
                # Exercise the ordinary checkpoint/quit path. A forced two-second
                # process kill can cut off a provider's in-flight final packet.
                self.send('\x15/quit\r');until=time.monotonic()+8
                while self.child.poll() is None and time.monotonic()<until:self.pump(.05)
            super().close()
            if self.native_trace and Path(self.native_trace).exists():timing.append(json.loads(Path(self.native_trace).read_text()))
    bench.setup=setup;bench.Terminal=Terminal
    def sources():
        result=bench.source_hashes()
        for file in [*HERE.glob('*'),HERE/'.build/aesel-native']:
            if file.is_file():result[str(file.relative_to(ROOT))]=hashlib.sha256(file.read_bytes()).hexdigest()
        return result
    before=sources();server=bench.StreamServer();threading.Thread(target=server.serve_forever,daemon=True).start()
    targets=['aesel-native','aesel-bun'];samples=[];warmup=[];rng=random.Random(0)
    try:
        with tempfile.TemporaryDirectory(prefix='aesel-full-cache-') as cache:
            for run in range(-args.warmup,args.runs):
                order=targets.copy();rng.shuffle(order)
                for target in order:
                    sample=bench.measure(target,server,100,True,cache);sample['run']=run+1
                    sample['code_cache']='Bun source/bytecode; same complete Aesel engine'
                    if target=='aesel-native':
                        if timing:sample['native']=timing.pop()
                        else:sample.update(status='failed',error='Missing native ready trace')
                    (warmup if run<0 else samples).append(sample)
                    print(json.dumps(sample),flush=True)
    finally:server.shutdown();server.server_close()
    summary={}
    for target in targets:
        good=[s for s in samples if s['target']==target and s['status']=='ok']
        summary[target]={'passed':len(good),'attempted':args.runs}
        for metric in bench.METRICS:
            values=[v for s in good for v in (s[metric] if isinstance(s[metric],list) else [s[metric]])]
            summary[target][metric]={'p50':bench.percentile(values,.5),'p95':bench.percentile(values,.95)}
        if target=='aesel-native':
            values=[s['native']['core_ready_ms'] for s in good]
            summary[target]['core_ready_ms']={'p50':bench.percentile(values,.5),'p95':bench.percentile(values,.95)}
    after=sources();changed=[k for k in before.keys()|after.keys() if before.get(k)!=after.get(k)]
    failures=[]
    if changed:failures.append('Sources changed during measurement')
    if any(s['status']!='ok' for s in samples+warmup):failures.append('Every sample including warmup must pass')
    native=summary['aesel-native'];bun=summary['aesel-bun']
    if not failures:
        for metric,budget in [('open_ms',16.7),('key_ms',2),('busy_input_ms',4),('stream_paint_ms',16.7)]:
            if native[metric]['p95']>budget:failures.append(f'{metric} p95 exceeds {budget} ms')
        if native['core_ready_ms']['p95']>bun['open_ms']['p95']+15:failures.append('Full core readiness adds more than 15 ms at p95')
        if any(s['native']['pending_input_bytes'] for s in samples if s['target']=='aesel-native'):failures.append('Opening input was not drained')
        if args.settled and native['first_reply_ms']['p95']>bun['first_reply_ms']['p95']+15:failures.append('Ready-provider first reply adds more than 15 ms at p95')
    report={'schema':1,'platform':platform.platform(),'source_changed':changed,'sources':before,'summary':summary,
        'warmup':warmup,'samples':samples,'gate_failures':failures,
        'provider_ready_before_enter':args.settled,
        'installed_launcher':args.installed,
        'measurement':'Same complete Aesel code and Bun bytecode, loopback provider, isolated accounts, randomized real PTYs. Native open_ms means its opening editor accepts a draft; core_ready_ms measures time from C host entry until the complete editor accepts input, not provider readiness. No cold-boot or physical-pixel claim.'}
    args.out.parent.mkdir(parents=True,exist_ok=True);args.out.write_text(json.dumps(report,indent=2)+'\n')
    print(json.dumps({'summary':summary,'gate_failures':failures},indent=2));return int(bool(failures))
if __name__=='__main__':sys.exit(main())
