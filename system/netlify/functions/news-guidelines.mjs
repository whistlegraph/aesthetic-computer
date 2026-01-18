// News Guidelines page, 2026.01.16
// Static guidelines page for news.aesthetic.computer
// Adapted from Hacker News guidelines in honor of Y Combinator

import { respond } from "../../backend/http.mjs";

function layout(body) {
  return `<!doctype html>
<html lang="en">
  <head>
    <meta charset="utf-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1" />
    <title>Guidelines — Aesthetic News</title>
    <link rel="icon" href="/news.aesthetic.computer/favicon.svg" type="image/svg+xml" />
    <link rel="stylesheet" href="https://aesthetic.computer/type/webfonts/berkeley-mono-variable.css">
    <link rel="stylesheet" href="/news.aesthetic.computer/main.css" />
  </head>
  <body>
    <div class="news-wrapper">
      ${body}
    </div>
  </body>
</html>`;
}

function header() {
  return `
  <header class="news-header">
    <div class="news-logo">
      <a href="/" class="news-logo-icon">A</a>
      <a href="/"><b>Aesthetic News</b></a>
    </div>
    <nav class="news-nav">
      <a href="/new">new</a>
      <span>|</span>
      <a href="/comments">comments</a>
      <span>|</span>
      <a href="/submit">submit</a>
    </nav>
  </header>`;
}

function footer() {
  return `
  <footer class="news-footer">
    <div class="news-footer-links">
      <a href="/guidelines">Guidelines</a>
      <span>|</span>
      <a href="https://aesthetic.computer/list">List</a>
      <span>|</span>
      <a href="https://aesthetic.computer/about">About</a>
      <span>|</span>
      <a href="https://give.aesthetic.computer">Gift</a>
      <span>|</span>
      <a href="https://aesthetic.computer/desktop">Desktop</a>
      <span>|</span>
      <a href="https://aesthetic.computer">Aesthetic Computer</a>
    </div>
  </footer>`;
}

export async function handler(event) {
  if (event.httpMethod !== "GET") {
    return respond(405, { error: "Method not allowed" });
  }

  const body = `
  ${header()}
  <main class="news-main news-guidelines">
    <h1>Aesthetic News Guidelines</h1>
    
    <p class="news-attribution">
      <em>These guidelines are adapted from <a href="https://news.ycombinator.com/newsguidelines.html" target="_blank" rel="noreferrer">Hacker News</a> 
      in honor of Y Combinator's excellent community standards. Thank you, HN!</em>
    </p>

    <h2>What to Submit</h2>
    
    <p><strong>On-Topic:</strong> Anything that creative coders, digital artists, or curious minds would find interesting. 
    That includes more than code and art. If you had to reduce it to a sentence: anything that gratifies one's creative and intellectual curiosity.</p>
    
    <p><strong>Off-Topic:</strong> Most stories about politics, or crime, or sports, or celebrities, unless they're evidence of 
    some interesting new phenomenon. Videos of pratfalls or disasters, or cute animal pictures. If they'd cover it on TV news, 
    it's probably off-topic.</p>

    <h2>In Submissions</h2>
    
    <ul>
      <li>Please don't do things to make titles stand out, like using uppercase or exclamation points, or saying how great an article is. 
      It's implicit in submitting something that you think it's important.</li>
      
      <li>Please submit the original source. If a post reports on something found on another site, submit the latter.</li>
      
      <li>Please don't use Aesthetic News primarily for promotion. It's ok to post your own stuff part of the time, 
      but the primary use of the site should be for curiosity.</li>
      
      <li>If the title includes the name of the site, please take it out, because the site name will be displayed after the link.</li>
      
      <li>If the title contains a gratuitous number or number + adjective, we'd appreciate it if you'd crop it. 
      E.g. translate "10 Ways To Do X" to "How To Do X," and "14 Amazing Ys" to "Ys." Exception: when the number is meaningful.</li>
      
      <li>Otherwise please use the original title, unless it is misleading or linkbait; don't editorialize.</li>
      
      <li>If you submit a video or pdf, please warn us by appending [video] or [pdf] to the title.</li>
      
      <li>Please don't delete and repost. Deletion is for things that shouldn't have been submitted in the first place.</li>
      
      <li>Don't solicit upvotes, comments, or submissions. Users should vote and comment when they run across something 
      they personally find interesting—not for promotion.</li>
    </ul>

    <h2>In Comments</h2>
    
    <ul>
      <li>Be kind. Don't be snarky. Converse curiously; don't cross-examine. Edit out swipes.</li>
      
      <li>Comments should get more thoughtful and substantive, not less, as a topic gets more divisive.</li>
      
      <li>When disagreeing, please reply to the argument instead of calling names. 
      "That is idiotic; 1 + 1 is 2, not 3" can be shortened to "1 + 1 is 2, not 3."</li>
      
      <li>Don't be curmudgeonly. Thoughtful criticism is fine, but please don't be rigidly or generically negative.</li>
      
      <li>Please don't fulminate. Please don't sneer, including at the rest of the community.</li>
      
      <li>Please respond to the strongest plausible interpretation of what someone says, not a weaker one that's easier to criticize. 
      Assume good faith.</li>
      
      <li>Eschew flamebait. Avoid generic tangents. Omit internet tropes.</li>
      
      <li>Please don't post shallow dismissals, especially of other people's work. A good critical comment teaches us something.</li>
      
      <li>Please don't use Aesthetic News for political or ideological battle. It tramples curiosity.</li>
      
      <li>Please don't comment on whether someone read an article. "Did you even read the article? It mentions that" 
      can be shortened to "The article mentions that".</li>
      
      <li>Please don't pick the most provocative thing in an article or post to complain about in the thread. 
      Find something interesting to respond to instead.</li>
      
      <li>Throwaway accounts are ok for sensitive information, but please don't create accounts routinely. 
      Aesthetic News is a community—users should have an identity that others can relate to.</li>
      
      <li>Please don't use uppercase for emphasis. If you want to emphasize a word or phrase, 
      put *asterisks* around it and it will get italicized.</li>
      
      <li>Please don't complain that a submission is inappropriate. If a story is spam or off-topic, flag it. 
      Don't feed egregious comments by replying; flag them instead.</li>
      
      <li>Please don't complain about tangential annoyances—e.g. article or website formats, name collisions, 
      or back-button breakage. They're too common to be interesting.</li>
      
      <li>Please don't comment about the voting on comments. It never does any good, and it makes boring reading.</li>
    </ul>
  </main>
  ${footer()}`;

  const html = layout(body);
  return respond(200, html, { "Content-Type": "text/html" });
}
