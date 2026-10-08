"""Resume saved piece versions through the real TUI with isolated, offline stores."""
import fcntl, hashlib, json, os, pty, select, shutil, struct, subprocess, tempfile, termios, time
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
FIRST = 'export function paint({wipe}) { wipe(0); }\n'
SECOND = 'export function paint({wipe}) { wipe(255); }\n'


def check_resume(name, *, pro=False, missing_file=False, saved_versions=()):
    with tempfile.TemporaryDirectory(prefix='aesel-resume-') as tmp:
        root = Path(tmp).resolve()
        piece = root / 'piece.mjs'
        if not missing_file:
            piece.write_text(SECOND)
        history = root / 'history' / hashlib.sha256(str(piece).encode()).hexdigest()
        for version, source in enumerate(saved_versions):
            history.mkdir(parents=True, exist_ok=True)
            (history / f'v{version}.json').write_text(json.dumps({
                'version': version, 'source': source,
                'revision': hashlib.sha256(source.encode()).hexdigest(),
                'updatedAt': '2026-10-08T00:00:00.000Z',
            }))
        checkpoint = root / 'session.json'
        snapshot = {
            'schema': 1, 'cwd': str(root), 'backend': 'codex', 'model': 'fixture-model',
            'live': {'file': str(piece), 'runtime': 'mjs', 'channel': 'fixture-channel'},
            'pieceVersion': 0,
            'ui': {'entries': [{'id': 'old', 'kind': 'user', 'text': 'saved conversation'}],
                   'input': 'saved draft', 'cursor': 11, 'history': [], 'queued': [], 'medium': 'piece'},
            'options': {'autopublish': False, 'mouseEnabled': False},
            'engine': {'threadId': 'fixture'}, 'handoff': '', 'archivedConversation': [],
        }
        checkpoint.write_text(json.dumps(snapshot))
        log = root / 'events.jsonl'
        env = {key: value for key, value in os.environ.items()
               if not key.startswith(('AESEL_', 'EASEL_', 'SLAB_')) and key not in ('HOME', 'NODE_OPTIONS')}
        env.update(TERM='xterm-256color', NO_COLOR='1', SLAB_HOME=str(root / 'slab'),
                   AESEL_TEST_LOG=str(log), AESEL_HISTORY_DIR=str(root / 'history'),
                   AESEL_TRANSCRIPTS=str(root / 'transcripts'),
                   AESEL_CONFIG_DIR=str(root / '.config/aesel'))
        master, slave = pty.openpty()
        fcntl.ioctl(slave, termios.TIOCSWINSZ, struct.pack('HHHH', 28, 100, 0, 0))
        command = [shutil.which('node'), '--import', str(ROOT / 'test/harness-tui-fixture.mjs'),
                   str(ROOT / 'src/tui.mjs'), '--cwd', str(root), '--continue-session',
                   '--checkpoint', str(checkpoint), '--private', '--no-autopublish']
        if pro:
            command.append('--pro')
        child = subprocess.Popen(command, stdin=slave, stdout=slave, stderr=slave, env=env)
        os.close(slave)
        output = bytearray()

        def wait_for(predicate):
            until = time.monotonic() + 12
            while time.monotonic() < until:
                if select.select([master], [], [], .05)[0]:
                    try:
                        output.extend(os.read(master, 65536))
                    except OSError:
                        break
                if predicate():
                    return
            assert predicate(), f'{name}: {output.decode(errors="replace")[-7000:]}'

        def connected():
            return log.exists() and any(json.loads(line)['event'] == 'connected'
                                        for line in log.read_text().splitlines())

        try:
            wait_for(connected)
            assert child.poll() is None
            os.write(master, b'\x15/quit\r')
            wait_for(lambda: child.poll() is not None)
            assert child.returncode == 0, output.decode(errors='replace')[-7000:]
            restored = json.loads(checkpoint.read_text())
            assert any(entry['text'] == 'saved conversation' for entry in restored['ui']['entries'])
            assert restored['engine']['threadId'] == 'fixture'
            errors = [entry['text'] for entry in restored['ui']['entries'] if entry['kind'] == 'error']
            versions = sorted(history.glob('v*.json')) if history.exists() else []
            if pro:
                assert not errors, errors
                assert len(versions) == len(saved_versions)
                assert sorted(p.name for p in root.glob('*.mjs')) == ([] if missing_file else ['piece.mjs'])
                if not missing_file:
                    assert piece.read_text() == SECOND
            elif not saved_versions:
                assert any('No saved v0' in error for error in errors), errors
                assert piece.read_text() == SECOND
                assert len(versions) == 1  # Startup checkpoints the current source afterwards.
            else:
                assert not errors, errors
                assert piece.read_text() == saved_versions[0]
                assert len(versions) == len(saved_versions) + (len(saved_versions) > 1)
                if len(saved_versions) > 1:
                    assert json.loads(versions[-1].read_text())['restoredFrom'] == 0
            print(f'PASS: {name}')
        finally:
            if child.poll() is None:
                child.kill()
                child.wait(timeout=5)
            os.close(master)


check_resume('pro resume ignores saved piece version', pro=True, saved_versions=(FIRST, SECOND))
check_resume('pro resume ignores a missing piece', pro=True, missing_file=True)
check_resume('missing v0 keeps the current file and reports the recovery error')
check_resume('piece resume restores v0 and appends history', saved_versions=(FIRST, SECOND))
check_resume('piece resume at the latest version leaves history unchanged', saved_versions=(SECOND,))
