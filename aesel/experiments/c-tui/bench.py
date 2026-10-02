#!/usr/bin/env python3
"""Native shell vs full Bun TUI and Codex; same loopback stream and real PTYs."""
import argparse, hashlib, importlib.util, json, platform, random, shutil, subprocess, sys, tempfile, threading
from pathlib import Path

HERE=Path(__file__).resolve().parent;ROOT=HERE.parents[1]
sys.path.insert(0,str(ROOT/'test'))
spec=importlib.util.spec_from_file_location('bench_tui',ROOT/'bin/bench-tui.py')
bench=importlib.util.module_from_spec(spec);spec.loader.exec_module(bench)

def main():
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--runs',type=int,default=7);parser.add_argument('--out',type=Path,required=True)
    args=parser.parse_args()
    if args.runs<1:parser.error('--runs must be positive')
    if not (HERE/'.build/aesel-c').exists():parser.error('run make first')
    if not shutil.which('bun'):parser.error('Bun is required for the comparison')
    original_setup=bench.setup
    def setup(target,root,endpoint,**options):
        if target=='codex':return original_setup(target,root,endpoint)
        command,env=original_setup('aesel-codex-launcher',root,endpoint,runtime='bun',launcher='ac')
        if target=='aesel-c':
            env.pop('NODE_OPTIONS',None);env.pop('BUN_OPTIONS',None)
            command=[str(HERE/'.build/aesel-c'),'--runtime',shutil.which('bun'),'--bridge',str(HERE/'bridge.mjs')]
        return command,env
    bench.setup=setup
    def fingerprints():
        result=bench.source_hashes()
        for file in HERE.iterdir():
            if file.is_file():result[str(file.relative_to(ROOT))]=hashlib.sha256(file.read_bytes()).hexdigest()
        result['experiments/c-tui/.build/aesel-c']=hashlib.sha256((HERE/'.build/aesel-c').read_bytes()).hexdigest()
        return result
    before=fingerprints();samples=[];targets=['aesel-c','aesel-bun','codex'];rng=random.Random(0)
    server=bench.StreamServer();threading.Thread(target=server.serve_forever,daemon=True).start()
    try:
        with tempfile.TemporaryDirectory(prefix='aesel-c-cache-') as cache:
            for run in range(args.runs):
                order=targets.copy();rng.shuffle(order)
                for target in order:
                    sample=bench.measure(target,server,100,True,cache)
                    sample['run']=run+1
                    if target=='aesel-c':sample['code_cache']='native C UI + Bun provider sidecar'
                    if target=='aesel-bun':sample['code_cache']='Bun source/bytecode'
                    samples.append(sample);print(json.dumps(sample),flush=True)
    finally:server.shutdown();server.server_close()
    summary={}
    for target in targets:
        good=[sample for sample in samples if sample['target']==target and sample['status']=='ok']
        summary[target]={'passed':len(good),'attempted':args.runs}
        for metric in bench.METRICS:
            values=[value for sample in good for value in (sample[metric] if isinstance(sample[metric],list) else [sample[metric]])]
            summary[target][metric]={'p50':bench.percentile(values,.5),'p95':bench.percentile(values,.95)}
    after=fingerprints();changed=[key for key in before.keys()|after.keys() if before.get(key)!=after.get(key)]
    report={'schema':1,'platform':platform.platform(),'runs':args.runs,'source_changed':changed,'sources':before,
        'versions':{'bun':subprocess.check_output(['bun','--version'],text=True).strip(),
                    'compiler':subprocess.check_output(['clang','--version'],text=True).splitlines()[0]},
        'limits':'C is a reduced native terminal with two pthreads and a Bun provider sidecar. It has no Aesel tools, approvals, markdown, artifacts or durable UI checkpoints. These are not feature-equivalent implementations. Warm process/PTY timings, not physical pixels or real model latency.',
        'samples':samples,'summary':summary}
    args.out.parent.mkdir(parents=True,exist_ok=True);args.out.write_text(json.dumps(report,indent=2)+'\n')
    print(json.dumps(summary,indent=2))
    return int(bool(changed) or any(sample['status']!='ok' for sample in samples))

if __name__=='__main__':sys.exit(main())
