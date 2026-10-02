"""Real TUI: transient tool feed, reported usage, resize, and saved reify state."""
import os, pty, subprocess, tempfile, json, time, select, shutil, fcntl, termios, struct, signal, re
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
ESC = re.compile(r'\x1b(?:\[[0-?]*[ -/]*[@-~]|\][^\x07]*\x07)')
with tempfile.TemporaryDirectory(prefix='aesel-activity-', dir='/tmp') as tmp:
    root = Path(tmp)
    master, slave = pty.openpty()
    fcntl.ioctl(slave, termios.TIOCSWINSZ, struct.pack('HHHH', 24, 100, 0, 0))
    log = root / 'events.jsonl'
    env = dict(os.environ, TERM='xterm-256color', COLORTERM='truecolor', AESEL_THEME='own', AESEL_GROUND='paint',
               SLAB_TERMINAL_TTY='ttys999', SLAB_HOME=str(root / 'slab'), AESEL_TEST_LOG=str(log),
               AESEL_HISTORY_DIR=str(root / 'history'), AESEL_TRANSCRIPTS=str(root / 'transcripts'),
               AESEL_CONFIG_DIR=str(root / '.config/aesel'),
               NODE_OPTIONS='--import=' + str(ROOT / 'test/activity-tui-fixture.mjs'))
    for key in ['AESEL_DESKTOP', 'AESEL_DESKTOP_SESSION', 'AESEL_SESSION_ID', 'NO_COLOR', 'HOME']:
        env.pop(key, None)
    child = subprocess.Popen([shutil.which('node'), str(ROOT / 'src/tui.mjs'), '--cwd', tmp, '--pro', '--backend', 'codex',
                              '--prompt', 'Make the interface quieter.', '--no-autopublish'],
                             stdin=slave, stdout=slave, stderr=slave, env=env)
    os.close(slave)
    output = bytearray()

    def frame():
        try:
            return json.loads((root / 'frame.json').read_text())
        except (OSError, ValueError):
            return {'frame': '', 'columns': 0}

    def plain():
        return ESC.sub('', frame()['frame'])

    def wait_for(predicate, timeout=15):
        until = time.monotonic() + timeout
        while time.monotonic() < until:
            if select.select([master], [], [], .05)[0]:
                try:
                    output.extend(os.read(master, 65536))
                except OSError:
                    if predicate(): return
                    break
            if predicate(): return
        raise AssertionError(output.decode(errors='replace')[-5000:])

    def save(name):
        evidence = os.environ.get('AESEL_TEST_EVIDENCE')
        if evidence:
            folder = Path(evidence); folder.mkdir(parents=True, exist_ok=True)
            (folder / (name + '.json')).write_text(json.dumps(frame()))

    def resize(width, height):
        fcntl.ioctl(master, termios.TIOCSWINSZ, struct.pack('HHHH', height, width, 0, 0))
        child.send_signal(signal.SIGWINCH)
        wait_for(lambda: frame()['columns'] == width)
        assert len(plain().splitlines()) == height

    try:
        wait_for(lambda: '› checking the preview' in plain())
        assert 'RAW_' not in output.decode(errors='replace')
        assert len([line for line in plain().splitlines() if 'files' in line or 'checking the preview' in line]) <= 3
        assert len(set(re.findall(r'\x1b\[38;2;[\d;]+m', frame()['frame']))) >= 3
        save('working-wide')
        os.write(master, b'draft while working')
        wait_for(lambda: 'draft while working' in plain())
        def pose():
            match = re.search(r'[/\\]{2}\([o.^-]+\)>', next(reversed(plain().splitlines()), ''))
            return match.group() if match else ''
        donkey = pose()
        assert donkey
        wait_for(lambda: bool(pose()) and pose() != donkey)
        assert 'draft while working' in plain(), 'animation must preserve active typing'
        resize(32, 16)
        assert '› checking the preview' in plain()
        assert 'draft while working' in plain()
        assert all(len(line) == 32 for line in plain().splitlines())
        save('working-narrow')
        (root / 'finish').touch()
        wait_for(lambda: '13 tools' in plain())
        assert 'checking the preview' not in plain()
        assert 'The interface is ready.' in plain()
        save('done-narrow')
        resize(100, 24)
        assert '6.4k tokens · 800 reasoning tokens' in plain()
        save('done-wide')
        os.write(master, b'\x15/reify\r')
        wait_for(lambda: len([line for line in log.read_text().splitlines() if json.loads(line)['event'] == 'connected']) == 2)
        wait_for(lambda: '13 tools' in plain())
        assert 'checking the preview' not in plain()
        assert 'RAW_' not in plain()
        os.write(master, b'/quit\r')
        wait_for(lambda: child.poll() is not None)
        assert child.returncode == 0
        print('PASS: real TUI feed is bounded and transient; raw tools stay off the page; Codex tokens and reasoning appear beneath the reply; resize and reify preserve the receipt')
    finally:
        if child.poll() is None:
            child.kill(); child.wait(timeout=5)
        os.close(master)
