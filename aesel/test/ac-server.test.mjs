import assert from "node:assert/strict";
import { mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import test from "node:test";
import { AcServer } from "../src/ac-server.mjs";
import { PIECE_REPLY } from "../src/piece-prompt.mjs";
