import { ACSession, scenario, report } from "./ac-harness.mjs";

const ac = await ACSession.open();
try {
  await scenario("resize continuity preserves framebuffer aspect", async (expect) => {
    await ac.boot("@sol");
    const sizes = [{ width: 420, height: 900 }, { width: 1000, height: 520 }, { width: 720, height: 720 }];
    const samples = [];
    for (const size of sizes) {
      await ac.page.setViewport(size);
      await ac.wait(40);
      samples.push(await ac.page.evaluate(() => {
        const freeze = document.querySelector('canvas[data-type="freeze"]');
        return { present: Boolean(freeze), objectFit: freeze ? getComputedStyle(freeze).objectFit : null };
      }));
    }
    const frozen = samples.filter((sample) => sample.present);
    expect(frozen.length > 0, "observed a continuity frame during resize");
    expect(frozen.every((sample) => sample.objectFit === "contain"), "continuity frames preserve aspect");
  });
} finally { await ac.close(); }
process.exit(report());
