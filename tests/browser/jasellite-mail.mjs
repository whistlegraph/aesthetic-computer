// jasellite-mail, 2026.10.08
// Read a letter sent to a mail+tag@aesthetic.computer address, for the signup
// journeys. mail@aesthetic.computer's maildir and mu index live on jasellite
// (toolchain/macos/SCORE.md, "Mail lives on jasellite"), so each lap is one ssh
// hop: mbsync ac-mail, mu index, then the newest matching letter's text.

import { execFile } from "node:child_process";
import { promisify } from "node:util";

const pexec = promisify(execFile);
const MAIL_HOST = process.env.AC_MAIL_HOST ?? "jas@24.144.92.66";
// jasellite's login shell is fish; single quotes are literal in fish and sh.
const quote = (s) => `'${String(s).replaceAll("'", `'\\''`)}'`;

const onJasellite = async (cmd) =>
  (await pexec("ssh", ["-o", "BatchMode=yes", "-o", "ConnectTimeout=10", MAIL_HOST, cmd],
    { timeout: 120_000, maxBuffer: 8 * 1024 * 1024 })).stdout;

// Waits for a letter to `to` that arrived after `since` (ms epoch) and returns
// the first match of `pattern` in it (capture group 1 when there is one). The
// index is shared with jasellite's mail-sync timer; a busy lock just means try
// again on the next lap.
export async function waitForLetter(to, pattern, { since = 0, timeout = 150_000 } = {}) {
  const deadline = Date.now() + timeout;
  const after = Math.floor(since / 1000) - 5;
  while (Date.now() < deadline) {
    const text = await onJasellite(
      `mbsync ac-mail >/dev/null 2>&1; mu index --quiet >/dev/null 2>&1; ` +
      `for f in (mu find ${quote(`to:${to} maildir:/ac-mail/INBOX`)} --fields=l --sortfield=date --reverse 2>/dev/null); ` +
      `if test (stat -c %Y "$f") -ge ${after}; mu view "$f"; break; end; end`,
    ).catch(() => "");
    const found = text.match(pattern);
    if (found) return found[1] ?? found[0];
    await new Promise((r) => setTimeout(r, 6000));
  }
  throw new Error(`no letter to ${to} matching ${pattern} within ${timeout / 1000}s`);
}
