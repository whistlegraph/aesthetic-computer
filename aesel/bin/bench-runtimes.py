#!/usr/bin/env python3
"""Compare Node and Bun using Aesel's real PTY and deterministic local provider."""
import argparse, hashlib, importlib.util, json, os, platform, random, shlex, shutil, subprocess, sys, tempfile, threading
from pathlib import Path

ROOT=Path(__file__).resolve().parents[1]
sys.path.insert(0,str(ROOT/'test'))
spec=importlib.util.spec_from_file_location('bench_tui',ROOT/'bin/bench-tui.py')
bench=importlib.util.module_from_spec(spec);spec.loader.exec_module(bench)


def main():
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--bun',default=shutil.which('bun'))
    parser.add_argument('--node',default=shutil.which('node'))
    parser.add_argument('--backend',choices=['ac','codex'],default='codex')
    parser.add_argument('--runs',type=int,default=5)
    parser.add_argument('--entry',choices=['source','launch'],default='source')
    parser.add_argument('--launcher',action='store_true',help='include the public shell launcher (requires --entry launch)')
    parser.add_argument('--columns',type=int,default=100)
    parser.add_argument('--out',type=Path,required=True)
    args=parser.parse_args()
    if not args.bun or not args.node:parser.error('--bun and --node must name installed runtimes')
    if args.runs<1 or args.columns<32:parser.error('invalid run count or terminal width')
    if args.launcher and args.entry!='launch':parser.error('--launcher requires --entry launch')
    runtimes={name:str(Path(shutil.which(value) or value).resolve()) for name,value in [('node',args.node),('bun',args.bun)]}
    def fingerprints():
        files=[*sorted((ROOT/'src').rglob('*.mjs')),ROOT/'bin/easel',ROOT/'bin/bench-tui.py',Path(__file__),ROOT/'test/latency-fixture.mjs']
        files += list((ROOT/'src').glob('.tui-bun*'))+[ROOT/'src/.tui-built.json']
        return {str(file.relative_to(ROOT)):hashlib.sha256(file.read_bytes()).hexdigest() for file in files if file.is_file()}
    before=fingerprints()
    load_before=os.getloadavg()
    entries={name:subprocess.check_output([exe,'--eval',
        'import {launchTarget} from '+json.dumps((ROOT/'src/launch-target.mjs').as_uri())+'; console.log(launchTarget('+json.dumps(str(ROOT/'src'))+'));'],text=True).strip()
        for name,exe in runtimes.items()} if args.entry=='launch' else {name:'tui.mjs' for name in runtimes}
    original_setup=bench.setup
    entry=ROOT/'src'/('tui.mjs' if args.entry=='source' else 'launch.mjs')
    def setup(target,root,endpoint):
        if target=='codex':return original_setup(target,root,endpoint)
        runtime=target.removeprefix('aesel-')
        base='aesel' if args.backend=='ac' else 'aesel-codex'
        command,env=original_setup(base+('-launcher' if args.launcher else ''),root,endpoint)
        if args.launcher:
            env.pop('NODE_OPTIONS',None)
            env['BUN_OPTIONS' if runtime=='bun' else 'NODE_OPTIONS']=('--preload=' if runtime=='bun' else '--import=')+shlex.quote(str(ROOT/'test/latency-fixture.mjs'))
            env['AESEL_JS_RUNTIME']=runtimes[runtime]
            return command,env
        # Explicit preload under both runtimes; fixture state stays out of providers.
        command=[runtimes[runtime],'--preload' if runtime=='bun' else '--import',
                 str(ROOT/'test/latency-fixture.mjs'),str(entry),*command[4:]]
        return command,env
    bench.setup=setup
    targets=['aesel-node','aesel-bun','codex']
    server=bench.StreamServer();thread=threading.Thread(target=server.serve_forever,daemon=True);thread.start()
    samples=[];rng=random.Random(0)
    try:
        with tempfile.TemporaryDirectory(prefix='aesel-runtime-cache-',dir='/tmp') as cache:
            for run in range(args.runs):
                order=targets.copy();rng.shuffle(order)
                for target in order:
                    sample=bench.measure(target,server,args.columns,True,cache)
                    sample['run']=run+1
                    if target=='aesel-bun':sample['code_cache']='Bun source/bytecode; Node compile cache does not apply'
                    samples.append(sample)
                    print(json.dumps(sample),flush=True)
    finally:server.shutdown();server.server_close()
    summary={}
    for target in targets:
        good=[s for s in samples if s['target']==target and s['status']=='ok']
        summary[target]={'passed':len(good),'attempted':args.runs}
        for metric in bench.METRICS:
            values=[value for sample in good for value in (sample[metric] if isinstance(sample[metric],list) else [sample[metric]])]
            summary[target][metric]={'p50':bench.percentile(values,.5),'p95':bench.percentile(values,.95)}
    after=fingerprints()
    changed=[key for key in before.keys()|after.keys() if before.get(key)!=after.get(key)]
    report={'schema':1,'platform':platform.platform(),'backend':args.backend,'entry':args.entry,'selected_entries':entries,'launcher':args.launcher,'source_changed':changed,
            'environment':bench.launch_environment(),'runtime_executables':runtimes,'load_average_before':load_before,
            'versions':{name:subprocess.check_output([exe,'--version'],text=True).strip() for name,exe in runtimes.items()},
            'measurement':'randomized warm-cache PTY; isolated configuration; same deterministic loopback SSE; no physical pixel timing',
            'sources':before,
            'samples':samples,'summary':summary}
    args.out.parent.mkdir(parents=True,exist_ok=True);args.out.write_text(json.dumps(report,indent=2)+'\n')
    print(json.dumps(summary,indent=2))
    return int(bool(changed) or any(sample['status']!='ok' for sample in samples))

if __name__=='__main__':sys.exit(main())
