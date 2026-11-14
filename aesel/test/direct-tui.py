"""Real TUI: a typed command line runs in the workspace without a model turn."""
import os, pty, subprocess, tempfile, json, time, select, shutil, fcntl, termios, struct, re
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
ESC = re.compile(r'\x1b(?:\[[0-?]*[ -/]*[@-~]|\][^\x07]*\x07)')
with tempfile.TemporaryDirectory(prefix='aesel-direct-', dir='/tmp') as tmp:
    root = Path(tmp)
    (root / 'package.json').write_text(json.dumps({'scripts': {
        'start': "node -e \"console.log('SERVER_UP on 3000'); setInterval(()=>{}, 1000)\""}}))
    master, slave = pty.openpty()
    fcntl.ioctl(slave, termios.TIOCSWINSZ, struct.pack('HHHH', 30, 100, 0, 0))
    log = root / 'events.jsonl'
    env = dict(os.environ, TERM='xterm-256color', COLORTERM='truecolor', AESEL_THEME='own', AESEL_GROUND='paint',
               SLAB_TERMINAL_TTY='ttys999', SLAB_HOME=str(root / 'slab'), AESEL_TEST_LOG=str(log),
               AESEL_HISTORY_DIR=str(root / 'history'), AESEL_TRANSCRIPTS=str(root / 'transcripts'),
               AESEL_CONFIG_DIR=str(root / '.config/aesel'),
               NODE_OPTIONS='--import=' + str(ROOT / 'test/activity-tui-fixture.mjs'))
    for key in ['AESEL_DESKTOP', 'AESEL_DESKTOP_SESSION', 'AESEL_SESSION_ID', 'NO_COLOR', 'HOME']:
        env.pop(key, None)
    child = subprocess.Popen([shutil.which('node'), str(ROOT / 'src/tui.mjs'), '--cwd', tmp, '--pro', '--backend', 'codex',
                              '--no-autopublish'], stdin=slave, stdout=slave, stderr=slave, env=env)
    os.close(slave)
    output = bytearray()

    def plain():
        try:
            return ESC.sub('', json.loads((root / 'frame.json').read_text())['frame'])
        except (OSError, ValueError):
            return ''

    def wait_for(predicate, timeout=15):
        until = time.monotonic() + timeout
        while time.monotonic() < until:
            if select.select([master], [], [], .05)[0]:
                try:
                    output.extend(os.read(master, 65536))
                except OSError:
                    break
            if predicate(): return
        raise AssertionError(plain()[-3000:] or output.decode(errors='replace')[-3000:])

    def model_called():
        return log.exists() and any(json.loads(line).get('event') == 'activity-ready' for line in log.read_text().splitlines())

    try:
        wait_for(lambda: len(plain()) > 0)
        time.sleep(.5)
        os.write(master, b'rpm start\r')
        wait_for(lambda: 'SERVER_UP on 3000' in plain())
        assert 'no braincells' in plain()
        assert 'rpm → npm' in plain()
        assert not model_called(), 'a command line must not start a model turn'
        os.write(master, b'/kill\r')
        wait_for(lambda: 'stopped' in plain())
        os.write(master, b'!echo DIRECT_OK; exit 4\r')
        wait_for(lambda: 'DIRECT_OK' in plain() and 'exit 4' in plain())
        assert not model_called()
        print('direct TUI passed')
    finally:
        child.kill()
        child.wait()
