#!/usr/bin/env python3
"""Compare real TUIs over PTYs against one deterministic loopback reply stream.

No model calls, credentials, user settings, GUI windows or private prompts.
Process start is warm-cache startup, not a cold boot. Times end at PTY output,
not terminal pixels. Failures are reported, never treated as fast samples.
"""
import argparse
import fcntl
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
import json
import hashlib
import math
import os
from pathlib import Path
import platform
import pty
import random
import re
import select
import shlex
import shutil
import signal
import struct
import subprocess
import sys
import tempfile
import termios
import threading
import time

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / 'test'))
from terminal_probe import Screen

TOKENS = ['SABLE'] + [f' DELTA{i:02}' for i in range(12)]
METRICS = ('open_ms','key_ms','busy_input_ms','submit_request_ms','first_reply_ms','stream_paint_ms')
BUDGETS = {'open_ms':250,'key_ms':16.7,'busy_input_ms':16.7,'submit_request_ms':100,'first_reply_ms':250,'stream_paint_ms':25}
now = time.perf_counter


class StreamServer(ThreadingHTTPServer):
    daemon_threads = True
    def __init__(self):
        super().__init__(('127.0.0.1', 0), Handler)
        self.events = []
        self.sequence = 0


class Handler(BaseHTTPRequestHandler):
    def log_message(self, *args): pass

    def send_json(self, value):
        body = json.dumps(value).encode()
        self.send_response(200); self.send_header('Content-Type','application/json')
        self.send_header('Content-Length',str(len(body))); self.end_headers(); self.wfile.write(body)

    def do_GET(self):
        self.send_json({'data': [], 'models': []})

    def do_POST(self):
        route = self.path.split('?',1)[0]
        body = self.rfile.read(int(self.headers.get('Content-Length', 0)))
        try: request = json.loads(body)
        except ValueError: request = {}
        if route.endswith('/count_tokens'): return self.send_json({'input_tokens':100})
        if not route.endswith(('/messages','/responses')): return self.send_json({})
        # No request bodies or headers are retained in the report.
        self.server.sequence += 1
        number = self.server.sequence
        metadata=json.loads(self.headers.get('x-codex-turn-metadata','{}'))
        auxiliary=metadata.get('thread_source')=='thread_title' or request.get('stream') is False
        self.server.events.append({'event':'request','at':now(),'number':number,'auxiliary':auxiliary})
        response_api = route.endswith('/responses')
        if not response_api and request.get('stream') is False:
            return self.send_json({'id':f'msg_{number}','type':'message','role':'assistant','model':request.get('model'),
                'content':[{'type':'text','text':'Benchmark'}],'stop_reason':'end_turn','stop_sequence':None,
                'usage':{'input_tokens':100,'output_tokens':5}})
        self.send_response(200); self.send_header('Content-Type','text/event-stream')
        self.send_header('Cache-Control','no-cache'); self.send_header('Connection','close'); self.end_headers()
        def event(kind, **fields):
            value = {'type':kind, **fields}
            self.wfile.write(f'event: {kind}\ndata: {json.dumps(value)}\n\n'.encode()); self.wfile.flush()
        try:
            model = request.get('model','benchmark')
            message = {'id':f'msg_{number}','type':'message','role':'assistant','content':[]}
            response = {'id':f'resp_{number}','object':'response','created_at':int(time.time()),'status':'in_progress','model':model,'output':[]}
            if response_api:
                event('response.created', response=response)
                event('response.output_item.added',output_index=0,item={**message,'status':'in_progress'})
                event('response.content_part.added',item_id=message['id'],output_index=0,content_index=0,part={'type':'output_text','text':'','annotations':[]})
            else:
                event('message_start',message={**message,'model':model,'stop_reason':None,'stop_sequence':None,'usage':{'input_tokens':100,'output_tokens':0}})
                event('content_block_start',index=0,content_block={'type':'text','text':''})
            time.sleep(.1)  # Identical simulated time to first token.
            tokens=['{"title":"Benchmark"}'] if auxiliary else TOKENS
            for token in tokens:
                if not auxiliary: self.server.events.append({'event':'token','token':token.strip(),'at':now(),'number':number})
                if response_api: event('response.output_text.delta',item_id=message['id'],output_index=0,content_index=0,delta=token)
                else: event('content_block_delta',index=0,delta={'type':'text_delta','text':token})
                time.sleep(.04)
            text = ''.join(tokens)
            if response_api:
                part = {'type':'output_text','text':text,'annotations':[]}
                message.update(status='completed',content=[part])
                event('response.output_text.done',item_id=message['id'],output_index=0,content_index=0,text=text)
                event('response.content_part.done',item_id=message['id'],output_index=0,content_index=0,part=part)
                event('response.output_item.done',output_index=0,item=message)
                event('response.completed',response={**response,'status':'completed','output':[message],'usage':{'input_tokens':100,'output_tokens':30,'total_tokens':130}})
            else:
                event('content_block_stop',index=0)
                event('message_delta',delta={'stop_reason':'end_turn','stop_sequence':None},usage={'output_tokens':30})
                event('message_stop')
        except (BrokenPipeError,ConnectionResetError): pass
        self.close_connection = True


class Terminal:
    def __init__(self, command, env, cwd, columns):
        self.master, slave = pty.openpty()
        fcntl.ioctl(slave,termios.TIOCSWINSZ,struct.pack('HHHH',30,columns,0,0))
        # Never count the PTY's line-discipline echo as a TUI paint.
        attrs=termios.tcgetattr(slave); attrs[3] &= ~(termios.ECHO|termios.ICANON); termios.tcsetattr(slave,termios.TCSANOW,attrs)
        self.screen=Screen(columns,30); self.raw=bytearray(); self.first_output=None; self.observation_ms=0
        self.started=now()
        self.child=subprocess.Popen(command,cwd=cwd,env=env,stdin=slave,stdout=slave,stderr=slave,start_new_session=True)
        os.close(slave)

    def pump(self, timeout=.01):
        if select.select([self.master],[],[],timeout)[0]:
            try: data=os.read(self.master,262144)
            except OSError: data=b''
            if data:
                at=now(); self.first_output=self.first_output or at; self.raw.extend(data)
                observed=now(); self.screen.feed(data); self.observation_ms+=(now()-observed)*1000
                for reply in self.screen.replies: self.send(reply)
                self.screen.replies.clear()
        return self.screen.text

    def until(self, predicate, timeout=20):
        deadline=now()+timeout
        while now()<deadline:
            text=self.pump()
            if predicate(text): return now()
            if self.child.poll() is not None: break
        raise RuntimeError('TUI did not reach the measured state: '+self.screen.text.strip()[-1800:])

    def send(self,data): os.write(self.master,data if isinstance(data,bytes) else data.encode())

    def close(self):
        if self.child.poll() is None:
            os.killpg(self.child.pid,signal.SIGTERM)
            try: self.child.wait(timeout=2)
            except subprocess.TimeoutExpired: os.killpg(self.child.pid,signal.SIGKILL); self.child.wait()
        os.close(self.master)


def setup(target, root, endpoint, runtime='node', launcher='direct'):
    # Allowlist the process environment. Vendor configuration and auth are empty.
    env={key:os.environ[key] for key in ('PATH','TMPDIR','LANG') if key in os.environ}
    env.update(HOME=str(root),TERM='xterm-256color',COLORTERM='truecolor',TERM_PROGRAM='benchmark',
        CODEX_HOME=str(root/'codex'),CLAUDE_CONFIG_DIR=str(root/'claude'),
        ANTHROPIC_API_KEY='benchmark-local-only',ANTHROPIC_BASE_URL=endpoint,
        CLAUDE_CODE_DISABLE_NONESSENTIAL_TRAFFIC='1',DISABLE_AUTOUPDATER='1',
        AESEL_BENCH_ROOT=str(root),AESEL_BENCH_ENDPOINT=endpoint,AESEL_THEME='own',AESEL_GROUND='paint',
        SLAB_HOME=str(root/'slab'),SLAB_TERMINAL_TTY='ttys999',AESEL_CONFIG_DIR=str(root/'.config/aesel'),
        AESEL_HISTORY_DIR=str(root/'history'),AESEL_TRANSCRIPTS=str(root/'transcripts'))
    (root/'codex').mkdir(); (root/'claude').mkdir()
    (root/'codex/config.toml').write_text(f'''model = "gpt-5.4"
model_provider = "benchmark"
check_for_update_on_startup = false
[model_providers.benchmark]
name = "Benchmark"
base_url = "{endpoint}/v1"
wire_api = "responses"
requires_openai_auth = false
[projects.{json.dumps(str(root))}]
trust_level = "trusted"
''')
    (root/'claude/.claude.json').write_text(json.dumps({'hasCompletedOnboarding':True,'theme':'dark',
        'customApiKeyResponses':{'approved':['benchmark-local-only'],'rejected':[]},
        'projects':{str(root):{'hasTrustDialogAccepted':True}}}))
    if target.startswith('aesel'):
        base=target.removesuffix('-launcher')
        backend=base.split('-',1)[1] if '-' in base else 'ac'
        env['AESEL_BENCH_BACKEND']=backend
        (root/'.ac-token').write_text(json.dumps({'access_token':'fixture',
            'expires_at':int(time.time()*1000)+3600000,'user':{'sub':'latency-fixture','handle':'bench'}}))
        disclosure=int(re.search(r'DISCLOSURE_VERSION\s*=\s*(\d+)',(ROOT/'src/transcript-format.mjs').read_text()).group(1))
        acknowledgments=root/'.config/aesel/disclosures';acknowledgments.mkdir(parents=True)
        (acknowledgments/(hashlib.sha256(b'latency-fixture').hexdigest()+'.json')).write_text(json.dumps(
            {'owner':'latency-fixture','version':disclosure,'acceptedAt':'2026-01-01T00:00:00.000Z'}))
        if target.endswith('-launcher'):
            env['BUN_OPTIONS' if runtime=='bun' else 'NODE_OPTIONS']=('--preload=' if runtime=='bun' else '--import=')+shlex.quote(str(ROOT/'test/latency-fixture.mjs'))
            env['AESEL_JS_RUNTIME']=runtime
            executable=ROOT/'bin/easel'
            if launcher!='direct':
                installed=root/'bin';installed.mkdir()
                executable=installed/launcher
                executable.symlink_to(ROOT/'bin'/('easel' if launcher=='ac' else launcher))
            command=[str(executable),str(root),'--pro','--backend',backend,'--no-autopublish']
        else:
            command=[shutil.which(runtime),'--preload' if runtime=='bun' else '--import',str(ROOT/'test/latency-fixture.mjs'),str(ROOT/'src/launch.mjs'),
                '--cwd',str(root),'--pro','--backend',backend,'--no-autopublish']
    elif target=='claude':
        command=[shutil.which('claude'),'--bare','--strict-mcp-config','--mcp-config','{"mcpServers":{}}','--model','claude-sonnet-4-6']
    else:
        command=[shutil.which('codex'),'--no-daemon','--no-alt-screen','--sandbox','read-only','--ask-for-approval','never','-C',str(root)]
    if not command[0]: raise RuntimeError(f'{target} is not installed')
    return command,env


def measure(target, server, columns, trace_startup=False, code_cache=None, runtime=None, launcher=None):
    with tempfile.TemporaryDirectory(prefix='aesel-latency-',dir='/tmp') as tmp:
        root=Path(tmp).resolve(); endpoint=f'http://127.0.0.1:{server.server_port}'
        options={**({'runtime':runtime} if runtime else {}),**({'launcher':launcher} if launcher else {})}
        command,env=setup(target,root,endpoint,**options)
        cache=Path(code_cache) if code_cache else root/'node-compile'
        if target.startswith('aesel'): env['NODE_COMPILE_CACHE']=str(cache)
        if trace_startup and target.startswith('aesel'): env['AESEL_STARTUP_TRACE']=str(root/'startup.json')
        result={'target':target,'columns':columns}
        if target.startswith('aesel'): result['code_cache']='reused' if any(cache.rglob('*')) else 'empty'
        if runtime and target.startswith('aesel'):
            result['runtime']=runtime
            if runtime=='bun':result['code_cache']='Bun source/bytecode; Node compile cache does not apply'
        term=Terminal(command,env,root,columns)
        try:
            def ready(text):
                if target.startswith('aesel'): return '@bench' in text
                if target=='claude': return '❯' in text and ('shortcuts' in text or 'Claude Code' in text)
                return 'for shortcuts' in text or 'Ask Codex to do anything' in text
            result['prompt_ms']=(term.until(ready)-term.started)*1000
            result['prompt_bytes']=len(term.raw)
            initial_key=now()
            term.send('zqv')
            editable=term.until(lambda s:'zqv' in s)
            result['open_ms']=(editable-term.started)*1000
            result['initial_key_ms']=(editable-initial_key)*1000
            result['startup_observer_ms']=term.observation_ms
            result['first_output_ms']=(term.first_output-term.started)*1000
            typed='zqv'; key_times=[]
            for char in 'xjkrb':
                typed+=char; sent=now(); term.send(char)
                key_times.append((term.until(lambda s:typed in s,3)-sent)*1000)
            result['key_ms']=key_times
            term.send('\x15'); term.until(lambda s:typed not in s,3)
            term.send('benchmark'); term.until(lambda s:'benchmark' in s,3)
            start_event=len(server.events); submitted=now(); term.send('\r')
            seen={}; busy_sent=None; deadline=now()+25
            while now()<deadline:
                screen=term.pump()
                if busy_sent is None and any(e['event']=='token' for e in server.events[start_event:]):
                    busy_sent=now(); term.send('vzx')
                if busy_sent is not None and 'busy_input_ms' not in result and 'vzx' in screen:
                    result['busy_input_ms']=(now()-busy_sent)*1000
                for token in TOKENS:
                    word=token.strip()
                    if word not in seen and word in screen: seen[word]=now()
                if len(seen)==len(TOKENS) and 'busy_input_ms' in result: break
                if term.child.poll() is not None: break
            events=server.events[start_event:]
            requests=[e for e in events if e['event']=='request' and not e['auxiliary']]
            if len(requests)!=1 or len(seen)!=len(TOKENS) or 'busy_input_ms' not in result:
                raise RuntimeError(f'Expected one request and {len(TOKENS)} painted markers; got {requests}, {len(seen)}. '+screen.strip()[-1800:])
            result['submit_request_ms']=(requests[0]['at']-submitted)*1000
            result['first_reply_ms']=(seen['SABLE']-submitted)*1000
            result['stream_paint_ms']=[(seen[e['token']]-e['at'])*1000 for e in events if e['event']=='token']
            result['output_bytes']=len(term.raw)
            result['status']='ok'
        except Exception as error:
            result.update(status='failed',error=str(error))
        finally:
            term.close()
            if trace_startup and (root/'startup.json').exists(): result['startup']=json.loads((root/'startup.json').read_text())
        return result


def percentile(values, p):
    return sorted(values)[max(0,math.ceil(len(values)*p)-1)] if values else None


def gates(summary, compare=False):
    failures=[]
    aesel=summary.get('aesel')
    if not aesel or aesel['passed']!=aesel['attempted']:
        return ['Aesel must have a complete set of successful samples']
    for metric,budget in BUDGETS.items():
        if aesel[metric]['p95']>budget:
            failures.append(f"aesel {metric} p95 {aesel[metric]['p95']:.2f} > budget {budget:.2f}")
    if compare:
        for own_target,target in [('aesel','claude'),('aesel','codex'),('aesel-claude','claude'),('aesel-codex','codex')]:
            if own_target not in summary: continue
            own_summary=summary[own_target]
            if own_summary['passed']!=own_summary['attempted']:
                failures.append(f'{own_target} must have a complete set of successful samples'); continue
            peer=summary.get(target)
            if not peer or peer['passed']!=peer['attempted']:
                failures.append(f'{target} must have a complete set of successful samples'); continue
            # Equality means <=, without a hidden tolerance. Keep both typical
            # latency and the tail so a slow peer outlier cannot hide a gap.
            for metric in METRICS:
                for quantile in ('p50','p95'):
                    own,other=own_summary[metric][quantile],peer[metric][quantile]
                    if own>other: failures.append(f'{own_target} {metric} {quantile} {own:.2f} > {target} {other:.2f}')
    return failures


def startup_gates(summary):
    peer=summary.get('codex')
    if not peer or peer['passed']!=peer['attempted']: return ['Codex must have a complete set of successful samples']
    own=[(target,value) for target,value in summary.items() if target.startswith('aesel')]
    if not own: return ['At least one Aesel target is required']
    failures=[]
    for target,value in own:
        if value['passed']!=value['attempted']:
            failures.append(f'{target} must have a complete set of successful samples'); continue
        for quantile in ('p50','p95'):
            ours,theirs=value['open_ms'][quantile],peer['open_ms'][quantile]
            if ours>=theirs: failures.append(f'{target} open_ms {quantile} {ours:.2f} >= codex {theirs:.2f}')
    return failures


def source_hashes():
    files=[*sorted((ROOT/'src').rglob('*.mjs')),*sorted((ROOT/'src').glob('.tui-*.json')),
        ROOT/'bin/easel',ROOT/'bin/a',ROOT/'bin/aes',ROOT/'bin/bench-tui.py',ROOT/'bin/build-tui.mjs',ROOT/'bin/build-bun.mjs',ROOT/'test/latency-fixture.mjs',
        ROOT/'test/terminal_probe.py',ROOT/'package.json']
    files+=list((ROOT/'src').glob('.tui-bun.cjs*'))
    return {str(file.relative_to(ROOT)):hashlib.sha256(file.read_bytes()).hexdigest() for file in files}


def launch_environment():
    # Local report only. A nonexistent automount in PATH can add tens of
    # milliseconds to every shell lookup, before the runtime trace begins.
    directories=[]
    for directory in os.get_exec_path():
        started=now(); exists=os.path.isdir(directory)
        directories.append({'path':directory,'is_directory':exists,'probe_ms':(now()-started)*1000})
    executables={tool:str(Path(executable).resolve()) for tool in ('node','bun','codex','claude')
        if (executable:=shutil.which(tool))}
    return {'path':directories,'executables':executables,'load_average':os.getloadavg(),'logical_cpus':os.cpu_count()}


def main():
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--targets',default='aesel,claude,codex')
    parser.add_argument('--runtime',choices=['node','bun'],default='node',help='JavaScript host used for Aesel targets')
    parser.add_argument('--launcher',choices=['direct','ac','a','aes'],default='direct',help='direct launcher file, or an installed symlink for launcher targets')
    parser.add_argument('--runs',type=int,default=5)
    parser.add_argument('--columns',type=int,default=100)
    parser.add_argument('--out',type=Path)
    parser.add_argument('--trace-startup',action='store_true',help='include local Aesel startup phase timings')
    parser.add_argument('--cold-cache',action='store_true',help='start every Aesel trial with an empty Node code cache')
    parser.add_argument('--compare-startup',action='store_true',help='fail unless every Aesel target starts faster than Codex at p50 and p95')
    parser.add_argument('--gate',action='store_true',help='fail if Aesel exceeds fixed p95 latency budgets')
    parser.add_argument('--compare',action='store_true',help='also fail unless Aesel matches or beats both peers at p50 and p95')
    args=parser.parse_args()
    targets=args.targets.split(',')
    allowed=('aesel','aesel-claude','aesel-codex','aesel-launcher','aesel-codex-launcher','claude','codex')
    if args.runs<1 or args.columns<32 or any(t not in allowed for t in targets): parser.error('invalid targets, runs or columns')
    if args.launcher!='direct' and not any(t.endswith('-launcher') for t in targets): parser.error('--launcher requires a launcher target')
    sources_before=source_hashes()
    load_before=os.getloadavg()
    server=StreamServer(); thread=threading.Thread(target=server.serve_forever,daemon=True); thread.start()
    samples=[]; rng=random.Random(0)
    try:
        with tempfile.TemporaryDirectory(prefix='aesel-code-cache-',dir='/tmp') as cache:
            for run in range(args.runs):
                order=targets.copy(); rng.shuffle(order)
                for target in order:
                    sample=measure(target,server,args.columns,args.trace_startup,None if args.cold_cache else cache,args.runtime,args.launcher)
                    sample['run']=run+1; samples.append(sample)
                    print(json.dumps(sample),flush=True)
    finally: server.shutdown(); server.server_close()
    summary={}
    for target in targets:
        good=[s for s in samples if s['target']==target and s['status']=='ok']
        summary[target]={'passed':len(good),'attempted':args.runs}
        for metric in METRICS:
            values=[v for s in good for v in (s[metric] if isinstance(s[metric],list) else [s[metric]])]
            summary[target][metric]={'p50':percentile(values,.5),'p95':percentile(values,.95)}
    versions={'aesel':json.loads((ROOT/'package.json').read_text())['version']}
    for tool in ('claude','codex','node','bun'):
        if shutil.which(tool): versions[tool]=subprocess.check_output([shutil.which(tool),'--version'],text=True,stderr=subprocess.DEVNULL).strip()
    failures=gates(summary,args.compare) if args.gate or args.compare else []
    if args.compare_startup: failures+=startup_gates(summary)
    sources_after=source_hashes()
    if sources_before!=sources_after: failures.append('Source changed during the benchmark; rerun against one revision')
    report={'schema':1,'clock':'monotonic','platform':platform.platform(),'python':platform.python_version(),'versions':versions,'aesel_runtime':args.runtime,'launcher':args.launcher,
        'environment':launch_environment(),'load_average_before':load_before,
        'source_sha256':sources_before,'source_changed':sources_before!=sources_after,
        'code_cache':('Bun uses prebuilt bytecode when verified; Node compile cache does not apply' if args.runtime=='bun' else
            'empty for every trial' if args.cold_cache else 'starts empty, shared across Aesel trials; first cold sample retained'),
        'measurement':'fresh processes, warm filesystem caches; PTY output; isolated configuration; deterministic loopback SSE; no physical pixel timing',
        'stream':{'first_token_delay_ms':100,'interval_ms':40,'tokens':len(TOKENS)},'samples':samples,'summary':summary,
        'budgets_ms':BUDGETS,'gate_failures':failures}
    if args.out: args.out.parent.mkdir(parents=True,exist_ok=True); args.out.write_text(json.dumps(report,indent=2)+'\n')
    print(json.dumps({'summary':summary,'gate_failures':failures},indent=2))
    return int(bool(failures) or any(s['status']!='ok' for s in samples))


if __name__=='__main__': sys.exit(main())
