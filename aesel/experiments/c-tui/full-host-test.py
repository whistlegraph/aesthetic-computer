import importlib.util,json,os,shutil,signal,sys,tempfile,time,fcntl,termios,struct
from pathlib import Path
HERE=Path(__file__).resolve().parent;ROOT=HERE.parents[1]
sys.path.insert(0,str(ROOT/'test'))
spec=importlib.util.spec_from_file_location('bench',ROOT/'bin/bench-tui.py')
bench=importlib.util.module_from_spec(spec);spec.loader.exec_module(bench)
with tempfile.TemporaryDirectory(prefix='native-full-test-') as tmp:
    root=Path(tmp);log=root/'events.jsonl';trace=root/'trace.json'
    env=dict(os.environ,NATIVE_TEST_LOG=str(log),AESEL_NATIVE_TRACE=str(trace))
    terminal=bench.Terminal([os.environ.get('AESEL_TEST_NATIVE_HOST',str(HERE/'.build/aesel-native')),'--',shutil.which('python3'),str(HERE/'full-fixture.py')],env,tmp,100)
    try:
        terminal.until(lambda s:'Aesel starting' in s)
        terminal.send('early draft');terminal.until(lambda s:'early draft' in s)
        terminal.until(lambda s:'Gate: press y' in s)
        assert not log.exists(),'an opening draft must not answer consent'
        terminal.send('y');terminal.until(lambda s:'draft:early draft' in s)
        events=[json.loads(l) for l in log.read_text().splitlines()]
        assert events==[{'gate':'y'},{'draft':'early draft'}],events
        terminal.send('\x1b[<0;22;7M');terminal.until(lambda s:len(log.read_text().splitlines())>=3)
        assert json.loads(log.read_text().splitlines()[2])['input']==list(b'\x1b[<0;22;7M')
        terminal.send('\x04');terminal.until(lambda s:terminal.child.poll() is not None)
        assert terminal.child.returncode==0
        timing=json.loads(trace.read_text());assert timing['core_ready_ms']>200 and timing['pending_input_bytes']==0
        assert b'\x1b[?1049l' in terminal.raw
        print('PASS: immediate editing, consent isolation, exact-once draft delivery, mouse bytes, clean exit, ready timing')
    finally:terminal.close()
