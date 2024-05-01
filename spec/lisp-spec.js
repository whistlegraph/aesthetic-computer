import fs from "fs/promises";
import {
  parse,
  evaluate,
} from "../system/public/aesthetic.computer/lib/lisp.mjs";

const tests = ["addition", "subtraction"];

describe("🤖 Kid Lisp", () => {
  let pieces = {};

  beforeAll(async () => {
    // List of all pieces you want to preload
    const loadPromises = tests.map((name) =>
      load(name).then((data) => {
        if (data) pieces[name] = data;
        else throw new Error(`Failed to load file: ${name}`);
      }),
    );

    try {
      console.log("Loading tests...");
      await Promise.all(loadPromises);
    } catch (error) {
      console.error("🔴 Error during test setup:", error);
      throw error; // This will fail the test suite
    }
  });

  it("Add numbers", () => {
    // console.log(`🧪\n${pieces.addition.src}`);
    expect(evaluate(parse(pieces.addition.src))).toEqual(6);
  });

  it("Subtract numbers", () => {
    // console.log(`🧪\n${pieces.subtraction.src}`);
    expect(evaluate(parse(pieces.subtraction.src))).toEqual(3);
  });
});

async function load(name) {
  const filePath = `./system/public/aesthetic.computer/disks/${name}.lisp`;
  try {
    const src = await fs.readFile(filePath, "utf8");
    const desc = src.split("\n")[0].replace(/^;\s*/, "");
    return { desc, src };
  } catch (error) {
    console.error(`🔴 Error setting up tests for ${name}:`, error);
    return null;
  }
}
