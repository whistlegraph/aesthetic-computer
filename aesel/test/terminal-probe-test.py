import unittest
import importlib.util
import tempfile
from pathlib import Path
from terminal_probe import Screen
spec=importlib.util.spec_from_file_location('bench_tui',Path(__file__).resolve().parents[1]/'bin/bench-tui.py')
bench=importlib.util.module_from_spec(spec);spec.loader.exec_module(bench)


class TerminalProbeTest(unittest.TestCase):
    def test_split_escapes_utf8_and_cursor_edits(self):
        screen=Screen(20,3)
        for byte in 'old\x1b[1;1Hnew\x1b[K\r\n❯ zqv'.encode(): screen.feed(bytes([byte]))
        self.assertEqual(screen.text.splitlines()[0].strip(),'new')
        self.assertIn('❯ zqv',screen.text)
        screen.feed(b'\x1b[2;3H\x1b[K')
        self.assertNotIn('zqv',screen.text)

    def test_title_is_not_visible_text_and_queries_get_replies(self):
        screen=Screen(20,3)
        screen.feed(b'\x1b]0;SABLE\x07\x1b[6n')
        self.assertNotIn('SABLE',screen.text)
        self.assertEqual(screen.replies,[b'\x1b[1;1R'])

    def test_clear_and_scroll(self):
        screen=Screen(10,2)
        screen.feed(b'one\r\ntwo\r\nthree')
        self.assertIn('two',screen.text)
        self.assertNotIn('one',screen.text)
        screen.feed(b'\x1b[2J')
        self.assertFalse(screen.text.strip())


class GateTest(unittest.TestCase):
    def summary(self):
        return {target:{'passed':5,'attempted':5,**{metric:{'p50':1,'p95':2} for metric in bench.METRICS}}
                for target in ('aesel','claude','codex')}

    def test_equal_passes_but_slow_tail_fails(self):
        summary=self.summary()
        self.assertEqual(bench.gates(summary,True),[])
        summary['aesel']['open_ms']['p95']=3
        self.assertTrue(any('open_ms p95' in problem for problem in bench.gates(summary,True)))

    def test_missing_or_failed_peer_is_not_a_pass(self):
        summary=self.summary();summary['codex']['passed']=4
        self.assertTrue(bench.gates(summary,True))
        del summary['codex']
        self.assertTrue(bench.gates(summary,True))

    def test_fixed_budget_fails_even_when_peers_are_slow(self):
        summary=self.summary();summary['aesel']['key_ms']['p95']=20
        self.assertTrue(any('budget' in problem for problem in bench.gates(summary)))

    def test_startup_requires_a_strict_win_at_both_percentiles(self):
        summary=self.summary()
        self.assertTrue(bench.startup_gates(summary), 'a tie is not a win')
        summary['aesel']['open_ms']={'p50':.5,'p95':1.5}
        self.assertEqual(bench.startup_gates(summary),[])
        summary['aesel']['open_ms']['p95']=2.1
        self.assertTrue(bench.startup_gates(summary))

    def test_startup_checks_every_launcher_and_rejects_missing_peers(self):
        summary=self.summary();summary['aesel']['open_ms']={'p50':.5,'p95':1.5}
        summary['aesel-codex-launcher']=dict(summary['claude'])
        self.assertTrue(bench.startup_gates(summary))
        del summary['aesel-codex-launcher'];summary['codex']['passed']=4
        self.assertTrue(bench.startup_gates(summary))
        del summary['codex']
        self.assertTrue(bench.startup_gates(summary))


class LauncherTest(unittest.TestCase):
    def test_installed_commands_keep_the_symlink_in_the_measured_command(self):
        for name in ('ac','a','aes'):
            with self.subTest(name=name), tempfile.TemporaryDirectory() as folder:
                root=Path(folder)
                command,env=bench.setup('aesel-codex-launcher',root,'http://127.0.0.1:1234',runtime='bun',launcher=name)
                executable=Path(command[0])
                self.assertTrue(executable.is_symlink())
                self.assertEqual(executable,root/'bin'/name)
                self.assertEqual(executable.resolve(),bench.ROOT/'bin'/('easel' if name=='ac' else name))
                self.assertEqual(env['AESEL_JS_RUNTIME'],'bun')
                self.assertEqual(command[command.index('--backend')+1],'codex')


if __name__=='__main__': unittest.main()
