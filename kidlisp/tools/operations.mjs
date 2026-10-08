#!/usr/bin/env node
// Machine-readable audited instruction contracts, generated from the registry.
import { operationManifest } from "../../system/public/aesthetic.computer/lib/kidlisp-ops.mjs";
process.stdout.write(JSON.stringify(operationManifest(), null, 2) + "\n");
