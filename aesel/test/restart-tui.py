"""Restart the real TUI in a PTY with isolated stores and an offline engine."""
import os, pty, subprocess, tempfile, json, time, select, shutil, shlex, fcntl, termios, struct
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
with tempfile.TemporaryDirectory(prefix='aesel-restart-', dir='/tmp') as tmp:
    root = Path(tmp)
    runtime = root / 'runtime'
    launcher = os.environ.get('AESEL_TEST_LAUNCHER')
    js_runtime = os.environ.get('AESEL_TEST_RUNTIME','node')
    (runtime / 'src').mkdir(parents=True)
    for source in (ROOT / 'src').iterdir():
        target = runtime / 'src' / source.name
        if source.name in ['tui.mjs', 'launch.mjs', 'terminal-cli.mjs', '.tui-built.mjs', '.tui-bun.cjs', '.tui-bun.cjs.jsc']:
            shutil.copyfile(source, target)
        else:
            target.symlink_to(source)
    if launcher:
        (runtime/'bin').mkdir()
        for source in (ROOT/'bin').iterdir():
            target=runtime/'bin'/source.name
            if source.name in ['easel','a','aes']:shutil.copy2(source,target)
            else:target.symlink_to(source)
    else:
        (runtime/'bin').symlink_to(ROOT/'bin')
    for name in ['context', 'media', 'package.json']:
        (runtime / name).symlink_to(ROOT / name)
    master, slave = pty.openpty()
    fcntl.ioctl(slave, termios.TIOCSWINSZ, struct.pack('HHHH', 28, 100, 0, 0))
    log = root / 'events.jsonl'
    env = dict(os.environ, TERM='xterm-256color', NO_COLOR='1',
               SLAB_TERMINAL_TTY='ttys999', SLAB_HOME=str(root / 'slab'),
               AESEL_TEST_LOG=str(log), AESEL_HISTORY_DIR=str(root / 'history'),
               AESEL_TRANSCRIPTS=str(root / 'transcripts'), AESEL_CONFIG_DIR=str(root / '.config/aesel'),
               NODE_OPTIONS='--import=' + str(ROOT / 'test/restart-tui-fixture.mjs'))
    for key in ['AESEL_DESKTOP', 'AESEL_DESKTOP_SESSION', 'AESEL_SESSION_ID']:
        env.pop(key, None)
    # Let the fixture's homedir override isolate any remaining default stores.
    env.pop('HOME', None)
    if launcher:
        installed=root/'bin';installed.mkdir()
        executable=installed/launcher;executable.symlink_to(runtime/'bin'/('easel' if launcher=='ac' else launcher))
        env['AESEL_JS_RUNTIME']=shutil.which(js_runtime)
        env.pop('NODE_OPTIONS',None)
        env['BUN_OPTIONS' if js_runtime=='bun' else 'NODE_OPTIONS']=('--preload=' if js_runtime=='bun' else '--import=')+shlex.quote(str(ROOT/'test/restart-tui-fixture.mjs'))
        preferences=root/'.config/aesel/provider.json';preferences.parent.mkdir(parents=True,exist_ok=True)
        preferences.write_text(json.dumps({'backend':'codex','model':'restart-model','effort':'ultra'}))
        command=[str(executable),tmp,'--pro','--backend','codex','--model','restart-model',
                 '--prompt','startup-once','--no-autopublish']
    else:
        command=[shutil.which(js_runtime),'--import',str(ROOT/'test/restart-tui-fixture.mjs'),
                 str(runtime/'src'/os.environ.get('AESEL_TEST_ENTRY','tui.mjs')),
                 '--cwd',tmp,'--pro','--backend','codex','--model','restart-model','--effort','ultra',
                 '--prompt','startup-once','--no-autopublish']
    child = subprocess.Popen(command,
                             stdin=slave, stdout=slave, stderr=slave, env=env)
    os.close(slave)
    output = bytearray()

    def rows():
        return [json.loads(line) for line in log.read_text().splitlines()] if log.exists() else []

    def connections():
        return [row for row in rows() if row['event'] == 'restart-connection']

    def wait_for(predicate, timeout=15):
        until = time.monotonic() + timeout
        while time.monotonic() < until:
            if select.select([master], [], [], .05)[0]:
                try:
                    output.extend(os.read(master, 65536))
                except OSError:
                    if predicate():
                        return
                    break
            if predicate():
                return
        raise AssertionError(output.decode(errors='replace')[-7000:])

    def send(text):
        os.write(master, (text + '\r').encode())

    try:
        wait_for(lambda: any(row['event'] == 'read' for row in rows()))
        # Change code on disk after boot: only the new process can render this.
        with (runtime / 'src/tui.mjs').open('a') as source:
            source.write("\nprocess.stdout.write('REIFIED_FIXTURE_VERSION\\n');\n")
        send('/mouse off')
        send('/preview notepat')
        send('hold turn')
        wait_for(lambda: any(row.get('text') == 'hold turn' for row in rows()))
        send('/reify')
        wait_for(lambda: b'reify queued' in output)
        assert len(connections()) == 1
        send('queued-once')
        os.write(master, 'unfinished 🟣 draft'.encode() + b'\x1b[D\x1b[D')
        wait_for(lambda: len(connections()) == 2)
        first, second = connections()
        if os.environ.get('AESEL_TEST_EXPECT_ENTRY'):
            assert first['entry'] == os.environ['AESEL_TEST_EXPECT_ENTRY'], first.get('entry')
        if launcher:
            assert '--eval' not in first['execArgv'], 'launcher bootstrap must not replay during reify'
        if launcher or os.environ.get('AESEL_TEST_ENTRY') == 'launch.mjs':
            assert second['entry'] == 'tui.mjs', 'edited source must replace the stale build'
        assert first['pid'] == second['pid']
        if not os.environ.get('AESEL_TEST_NATIVE_HOST'):
            assert first['pid'] == child.pid
        else:
            assert first['pid'] != child.pid and child.poll() is None
        assert first['sessionId'] == second['sessionId']
        assert second['resume'] == 'fixture'
        assert second['model'] == 'restart-model' and second['effort'] == 'ultra'
        assert '--continue-session' in second['args'] and '--prompt' not in second['args']
        assert len([row for row in rows() if row.get('text') == 'startup-once']) == 1
        saved = second['checkpoint']
        assert saved['ui']['input'] == 'unfinished 🟣 draft'
        assert saved['ui']['cursor'] == len('unfinished 🟣 draft') - 2
        assert saved['ui']['queued'] == ['queued-once']
        assert saved['options']['mouseEnabled'] is False
        assert saved['slab']['pinned'] == 'prompt.ac/notepat'
        checkpoint_path = Path(second['args'][second['args'].index('--checkpoint') + 1])
        assert checkpoint_path.name == first['sessionId'] + '.json'
        wait_for(lambda: b'REIFIED_FIXTURE_VERSION' in output)
        wait_for(lambda: any(row.get('text') == 'queued-once' for row in rows()))
        assert len([row for row in rows() if row.get('text') == 'queued-once']) == 1
        # Ctrl-U clears the restored editor draft before typing the command.
        os.write(master, b'\x15')
        broken = runtime / 'src' / 'broken-reify-test.mjs'
        broken.write_text('export const broken = ;')
        send('/restart')
        wait_for(lambda: b'Reify stopped:' in output)
        assert len(connections()) == 2 and child.poll() is None
        broken.unlink()
        send('reify through control')
        wait_for(lambda: len(connections()) == 3)
        assert next(row['result'] for row in rows() if row['event'] == 'reify-response')['status'] == 'queued'
        third = connections()[2]
        assert third['sessionId'] == first['sessionId']
        assert any(entry['text'] == 'Reify queued after this reply.' for entry in third['checkpoint']['ui']['entries'])
        send('/quit')
        wait_for(lambda: child.poll() is not None)
        assert child.returncode == 0
        snapshot = json.loads(checkpoint_path.read_text())
        assert len([entry for entry in snapshot['ui']['entries'] if entry['text'] == 'startup-once']) == 1
        print('PASS: reify loads edited code; preserves PID, Slab identity, thread, settings, draft, queue and preview; waits for final reply; syntax failure leaves session running')
    finally:
        if child.poll() is None:
            child.kill()
            child.wait(timeout=5)
        os.close(master)
