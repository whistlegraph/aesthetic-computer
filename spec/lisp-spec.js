// KidLisp: Spec 24.05.03.22.45
// A test runner for `kidlisp`. 

const tests = ["addition", "subtraction"];

import fs from "fs/promises";
import {
  parse,
  evaluate,
} from "../system/public/aesthetic.computer/lib/kidlisp.mjs";

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
      console.log("🧒 Loading kidlisp tests...");
      await Promise.all(loadPromises);
    } catch (error) {
      console.error("🔴 Error during test setup:", error);
      throw error; // This will fail the test suite
    }
  });

  it("Add numbers", () => {
    expect(evaluate(parse(pieces.addition.src))).toEqual(6);
  });

  it("Subtract numbers", () => {
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
    console.error(`🔴 Error setting up \`kidlisp\` tests for ${name}:`, error);
    return null;
  }
}
