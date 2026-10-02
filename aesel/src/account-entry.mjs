import {createInterface} from 'node:readline/promises';
import {isOffline} from './account-access.mjs';
import {nativeTerminalPhase} from './native-terminal.mjs';

// Finish account setup before constructing a workspace or starting any engine.
// Offline is not signed out: an account that already holds a handle boots on
// the cached one and is checked again when the network answers.
export async function requireAccountEntry(session, {input=process.stdin, output=process.stdout, question}={}) {
  let terminal;
  const ask = question || (async prompt => {
    terminal ||= createInterface({input,output});
    nativeTerminalPhase('gate');
    return terminal.question(prompt);
  });
  try {
    for (;;) {
      let offline = false;
      try { await session.requireAccount(); return true; }
      catch (error) {
        offline = isOffline(error);
        if (offline && session.handle) {
          output.write(`offline · continuing as @${session.handle}\n`);
          return true;
        }
        output.write(offline
          ? `\n${error.message}\nSigning in needs the network. Connect, then /retry.\n`
          : `\n${error.message}\n`);
      }
      if (!question && (!input.isTTY || !output.isTTY)) throw new Error('Open Aesel in a terminal to sign in and claim an AC handle.');
      let answer;
      try { answer = (await ask(offline ? '/retry · /quit\n> ' : '/login · /handle NAME · /retry · /quit\n> ')).trim(); }
      catch (error) { if (error?.name === 'AbortError') { output.write('\n'); return false; } throw error; }
      if (['/quit','/exit','q'].includes(answer)) return false;
      try {
        if (answer === '/login') await session.login({onUrl:url=>output.write(`Sign in: ${url}\n`)});
        else if (answer.startsWith('/handle ')) await session.claimHandle(answer.slice(8));
        else if (answer !== '/retry' && answer !== '') output.write('Complete AC sign-in and handle setup to continue.\n');
      } catch (error) { output.write(`${error.message}\n`); }
    }
  } finally { terminal?.close(); nativeTerminalPhase('boot'); }
}
