#!/usr/bin/env node
// Build script that reads the shared process-tree.js and ast-tree.js and generates the webview code
// This ensures the extension always uses the same code as dev testing

import { readFileSync, writeFileSync } from 'fs';
import { dirname, join } from 'path';
import { fileURLToPath } from 'url';

const __dirname = dirname(fileURLToPath(import.meta.url));

const processTreeJS = readFileSync(join(__dirname, 'views', 'process-tree-2d.js'), 'utf-8');
const astTreeJS = readFileSync(join(__dirname, 'views', 'ast-tree.js'), 'utf-8');

// Export for use in extension.ts at build time
const output = `// Auto-generated - DO NOT EDIT DIRECTLY
// Edit views/process-tree-2d.js and views/ast-tree.js instead and run: node build-views.mjs

export const PROCESS_TREE_JS = ${JSON.stringify(processTreeJS)};

export const AST_TREE_JS = ${JSON.stringify(astTreeJS)};
`;

writeFileSync(join(__dirname, 'generated-views.ts'), output);
console.log('✅ Generated views/process-tree-2d.js → generated-views.ts');
console.log('✅ Generated views/ast-tree.js → generated-views.ts');
