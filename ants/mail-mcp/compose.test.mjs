// node --test ants/mail-mcp/compose.test.mjs
import { test } from "node:test";
import assert from "node:assert/strict";
import { markupToText, textToHtml } from "./compose.mjs";

test("a labelled link reads as its label in HTML and keeps its address in text", () => {
  const body = "my handle is [@whistlegraph](https://instagram.com/Whistlegraph).";
  assert.equal(
    textToHtml(body),
    '<p style="margin:0 0 1em 0;">my handle is <a href="https://instagram.com/Whistlegraph">@whistlegraph</a>.</p>',
  );
  assert.equal(markupToText(body), "my handle is @whistlegraph (https://instagram.com/Whistlegraph).");
});

test("bare URLs still link themselves, full stop outside", () => {
  assert.match(textToHtml("see https://x.y/z."), /<a href="https:\/\/x\.y\/z">https:\/\/x\.y\/z<\/a>\./);
  assert.equal(markupToText("see https://x.y/z."), "see https://x.y/z.");
});

test("bold, italic and code", () => {
  const html = textToHtml("**bold move**, *really*, `npm run ac`");
  assert.match(html, /<strong>bold move<\/strong>, <em>really<\/em>, <code [^>]*>npm run ac<\/code>/);
  assert.equal(markupToText("**bold move**, *really*, `npm run ac`"), "bold move, really, `npm run ac`");
});

test("asterisks that are not emphasis stay as typed", () => {
  const body = "2 * 3 * 4 and a*b*c and **not closed";
  assert.equal(textToHtml(body), `<p style="margin:0 0 1em 0;">${body}</p>`);
  assert.equal(markupToText(body), body);
});

test("emphasis inside a label, links inside lists", () => {
  assert.match(
    textToHtml("[**loud** label](https://a.b/C?d=1&e=2)"),
    /<a href="https:\/\/a\.b\/C\?d=1&amp;e=2"><strong>loud<\/strong> label<\/a>/,
  );
  assert.match(textToHtml("- one [a](https://a.b)\n- two"), /<ul[^>]*><li>one <a href="https:\/\/a\.b">a<\/a><\/li><li>two<\/li><\/ul>/);
});

test("a label that is its own address is not doubled in text", () => {
  assert.equal(markupToText("[https://a.b/c](https://a.b/c)"), "https://a.b/c");
});

test("nothing but http, https and mailto becomes a link, and markup cannot inject", () => {
  const html = textToHtml('<script>x</script> [x](javascript:alert(1)) [q](https://a.b/"onmouseover=1)');
  assert.ok(!html.includes("<script>"));
  assert.ok(!html.includes('href="javascript:'));
  assert.ok(!/href="[^"]*"onmouseover/.test(html));
  assert.ok(html.includes('href="https://a.b/&quot;onmouseover=1"'));
});
