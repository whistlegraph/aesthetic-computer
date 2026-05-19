// prompt-search.test, 2026.05.18
// End-to-end browser validation of the universal AC search box, sigil
// autocomplete, Enter-discipline, and the rolodex history scrub — driven
// through a real local Chrome against a live URL.
//
//   npm run test:browser                 # headless, production
//   AC_TEST_URL=https://localhost:8888 npm run test:browser   # local branch
//   AC_HEADED=1 AC_SLOWMO=60 npm run test:browser             # watch it
//
// Assertions that need internals use window.__acPromptTest (present only
// when the target was built from the universal-search branch). Against a
// target without it, those checks soft-skip and screenshots still capture.

import { ACSession, scenario, report } from "./ac-harness.mjs";

const ac = await ACSession.open();
let hookSeen = false;

try {
  await scenario("boot prompt", async (expect) => {
    await ac.boot("prompt");
    await ac.shot("01-boot");
    const st = await ac.state();
    hookSeen = st !== null;
    if (!hookSeen) {
      console.log(
        "  ℹ️  no __acPromptTest on this target — screenshot-only mode " +
          "(deploy the universal-search branch or point AC_TEST_URL at a " +
          "local `npm run site` to get state assertions).",
      );
    }
    expect(true, "page booted without throwing");
  });

  await scenario("sigil autocomplete: $", async (expect) => {
    await ac.focusPrompt();
    await ac.clearInput();
    await ac.type("$");
    await ac.settleSearch();
    await ac.shot("02-sigil-dollar");
    const st = await ac.state();
    if (st?.ac) {
      expect(st.ac.activeTrigger === "$", `activeTrigger is "$" (got ${JSON.stringify(st.ac.activeTrigger)})`);
      expect(st.ac.visible === true, "dropdown visible");
    } else if (hookSeen) {
      expect(false, "state hook returned no ac");
    }
  });

  await scenario("universal search: bare word", async (expect) => {
    await ac.focusPrompt();
    await ac.clearInput();
    await ac.type("line");
    await ac.settleSearch(1800);
    await ac.shot("03-universal-line");
    const st = await ac.state();
    if (st?.ac) {
      expect(st.ac.activeTrigger === "", `universal mode (activeTrigger === "", got ${JSON.stringify(st.ac.activeTrigger)})`);
      expect(st.ac.items.length > 0, `has results (${st.ac.items.length})`);
      const colored = st.ac.items.some((i) => Array.isArray(i.color));
      expect(colored, "results are color-typed");
      expect(st.ac.navigated === false, "not navigated yet (Enter would still RUN the command)");
    }
  });

  await scenario("Enter discipline + arrow-to-complete", async (expect) => {
    // From the universal results, arrowing in must flip `navigated`.
    await ac.press("ArrowDown");
    const st = await ac.state();
    if (st?.ac) {
      expect(st.ac.navigated === true, "ArrowDown set navigated=true");
    }
    await ac.shot("04-universal-navigated");
    // Tab completes the selected result wholesale (universal getCompletedText).
    await ac.press("Tab");
    await ac.settleSearch(600);
    const st2 = await ac.state();
    if (st2) {
      expect(
        typeof st2.input === "string" && st2.input.length > 0,
        `Tab completed the input (got ${JSON.stringify(st2.input)})`,
      );
    }
    await ac.shot("05-after-tab-complete");
  });

  await scenario("rolodex: vertical drag scrubs history", async (expect) => {
    // Seed one history entry for the prompt slug, then return to prompt.
    await ac.focusPrompt();
    await ac.clearInput();
    await ac.type("notepat");
    await ac.press("Enter");
    await ac.wait(2500);
    await ac.boot("prompt"); // back to a fresh prompt
    await ac.focusPrompt();
    await ac.clearInput();
    await ac.shot("06-rolodex-before");
    const vp = { h: 1200 };
    // Swipe up from low on the screen toward the middle.
    await ac.dragVertical({ fromY: Math.round(vp.h * 0.8), toY: Math.round(vp.h * 0.45) });
    await ac.shot("07-rolodex-after");
    const st = await ac.state();
    if (st) {
      expect(
        typeof st.input === "string" && st.input.length > 0,
        `drag pulled a prior command into the input (got ${JSON.stringify(st.input)})`,
      );
    }
  });

  await scenario("UNITICKER deprecated", async (expect) => {
    const st = await ac.state();
    if (st) {
      expect(st.deprecateUniticker === true, "DEPRECATE_UNITICKER is on");
    }
  });
} finally {
  await ac.close();
}

process.exit(report());
