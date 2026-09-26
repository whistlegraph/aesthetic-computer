import {createInterface} from 'node:readline/promises';

// Finish account setup before constructing a workspace or starting any engine.
export async function requireAccountEntry(session, {input=process.stdin, output=process.stdout, question}={}) {
  let terminal;
  const ask = question || (async prompt => {
    terminal ||= createInterface({input,output});
    return terminal.question(prompt);
  });
  try {
    for (;;) {
      try { await session.requireAccount(); return true; }
      catch (error) { output.write(`\n${error.message}\n`); }
      if (!question && (!input.isTTY || !output.isTTY)) throw new Error('Open Aesel in a terminal to sign in and claim an AC handle.');
      const answer = (await ask('/login · /handle NAME · /retry · /quit\n> ')).trim();
      if (['/quit','/exit','q'].includes(answer)) return false;
      try {
        if (answer === '/login') await session.login({onUrl:url=>output.write(`Sign in: ${url}\n`)});
        else if (answer.startsWith('/handle ')) await session.claimHandle(answer.slice(8));
        else if (answer !== '/retry') output.write('Complete AC sign-in and handle setup to continue.\n');
      } catch (error) { output.write(`${error.message}\n`); }
    }
  } finally { terminal?.close(); }
}
