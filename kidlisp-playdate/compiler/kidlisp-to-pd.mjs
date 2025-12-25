#!/usr/bin/env node

// kidlisp-to-pd.mjs - Compile KidLisp to Playdate C code
// Transpiles KidLisp source to C that uses the kidlisp.h runtime

import { readFileSync, writeFileSync } from 'fs';
import { basename } from 'path';

// Tokenizer
function tokenize(source) {
  const tokens = [];
  let i = 0;
  
  while (i < source.length) {
    const char = source[i];
    
    // Skip whitespace
    if (/\s/.test(char)) {
      i++;
      continue;
    }
    
    // Skip comments
    if (char === ';') {
      while (i < source.length && source[i] !== '\n') i++;
      continue;
    }
    
    // Parentheses
    if (char === '(' || char === ')') {
      tokens.push({ type: 'paren', value: char });
      i++;
      continue;
    }
    
    // Strings
    if (char === '"' || char === "'") {
      const quote = char;
      i++;
      let str = '';
      while (i < source.length && source[i] !== quote) {
        str += source[i];
        i++;
      }
      i++; // skip closing quote
      tokens.push({ type: 'string', value: str });
      continue;
    }
    
    // Numbers
    if (/[\d\-]/.test(char) && (char !== '-' || /\d/.test(source[i+1] || ''))) {
      let num = '';
      if (char === '-') {
        num = '-';
        i++;
      }
      while (i < source.length && /[\d\.]/.test(source[i])) {
        num += source[i];
        i++;
      }
      tokens.push({ type: 'number', value: parseFloat(num) });
      continue;
    }
    
    // Symbols/identifiers
    if (/[a-zA-Z_\+\-\*\/\%\=\<\>\!]/.test(char)) {
      let sym = '';
      while (i < source.length && /[a-zA-Z0-9_\-\+\*\/\%\=\<\>\!]/.test(source[i])) {
        sym += source[i];
        i++;
      }
      tokens.push({ type: 'symbol', value: sym });
      continue;
    }
    
    i++;
  }
  
  return tokens;
}

// Parser
function parse(tokens) {
  let pos = 0;
  
  function parseExpr() {
    const token = tokens[pos];
    
    if (!token) return null;
    
    if (token.type === 'paren' && token.value === '(') {
      pos++; // skip (
      const list = [];
      while (tokens[pos] && !(tokens[pos].type === 'paren' && tokens[pos].value === ')')) {
        list.push(parseExpr());
      }
      pos++; // skip )
      return { type: 'list', value: list };
    }
    
    pos++;
    return token;
  }
  
  const ast = [];
  while (pos < tokens.length) {
    const expr = parseExpr();
    if (expr) ast.push(expr);
  }
  
  return ast;
}

// Code Generator
class CodeGenerator {
  constructor() {
    this.variables = new Map();
    this.functions = [];
    this.mainCode = [];
    this.indent = '    ';
  }
  
  generate(ast) {
    for (const expr of ast) {
      this.generateExpr(expr);
    }
    
    return this.buildOutput();
  }
  
  generateExpr(expr, context = 'statement') {
    if (!expr) return '';
    
    if (expr.type === 'number') {
      return expr.value.toString();
    }
    
    if (expr.type === 'string') {
      return `"${expr.value}"`;
    }
    
    if (expr.type === 'symbol') {
      return this.resolveSymbol(expr.value);
    }
    
    if (expr.type === 'list') {
      return this.generateCall(expr.value, context);
    }
    
    return '';
  }
  
  resolveSymbol(name) {
    // Map KidLisp symbols to C
    const mapping = {
      'white': 'KL_WHITE',
      'black': 'KL_BLACK',
      'width': 'kl_width()',
      'height': 'kl_height()',
      'frame': 'kl_frame()'
    };
    
    if (mapping[name]) return mapping[name];
    // Always prefix user variables with var_
    const safeName = name.replace(/-/g, '_');
    if (this.variables.has(safeName) || this.variables.has(name)) {
      return `var_${safeName}`;
    }
    return safeName;
  }
  
  generateCall(list, context) {
    if (list.length === 0) return '';
    
    const [head, ...args] = list;
    const fn = head.type === 'symbol' ? head.value : null;
    
    if (!fn) return '';
    
    // Variable definition
    if (fn === 'def') {
      const varName = args[0].value.replace(/-/g, '_');
      const value = this.generateExpr(args[1], 'expression');
      
      if (!this.variables.has(varName)) {
        this.variables.set(varName, 'float'); // Default to float
      }
      
      if (context === 'statement') {
        this.mainCode.push(`${this.indent}var_${varName} = ${value};`);
        return '';
      } else {
        return `(var_${varName} = ${value})`;
      }
    }
    
    // Graphics
    if (fn === 'wipe') {
      const color = args.length > 0 ? this.generateExpr(args[0], 'expression') : 'KL_WHITE';
      if (context === 'statement') {
        this.mainCode.push(`${this.indent}kl_wipe(${color});`);
        return '';
      }
      return `kl_wipe(${color})`;
    }
    
    if (fn === 'ink') {
      const color = this.generateExpr(args[0], 'expression');
      if (context === 'statement') {
        this.mainCode.push(`${this.indent}kl_ink(${color});`);
        return '';
      }
      return `kl_ink(${color})`;
    }
    
    if (fn === 'line') {
      // Support (line) with no args for random line
      if (args.length === 0) {
        if (context === 'statement') {
          this.mainCode.push(`${this.indent}kl_line_random();`);
          return '';
        }
        return `kl_line_random()`;
      }
      const [x1, y1, x2, y2] = args.map(a => this.generateExpr(a, 'expression'));
      if (context === 'statement') {
        this.mainCode.push(`${this.indent}kl_line(${x1}, ${y1}, ${x2}, ${y2});`);
        return '';
      }
      return `kl_line(${x1}, ${y1}, ${x2}, ${y2})`;
    }
    
    if (fn === 'box') {
      const [x, y, w, h] = args.slice(0, 4).map(a => this.generateExpr(a, 'expression'));
      const filled = args.length > 4 && args[4].value === 'fill' ? 1 : 0;
      if (context === 'statement') {
        this.mainCode.push(`${this.indent}kl_box(${x}, ${y}, ${w}, ${h}, ${filled});`);
        return '';
      }
      return `kl_box(${x}, ${y}, ${w}, ${h}, ${filled})`;
    }
    
    if (fn === 'circle') {
      const [x, y, r] = args.slice(0, 3).map(a => this.generateExpr(a, 'expression'));
      const filled = args.length > 3 && args[3].value === 'fill' ? 1 : 0;
      if (context === 'statement') {
        this.mainCode.push(`${this.indent}kl_circle(${x}, ${y}, ${r}, ${filled});`);
        return '';
      }
      return `kl_circle(${x}, ${y}, ${r}, ${filled})`;
    }
    
    if (fn === 'plot') {
      const [x, y] = args.map(a => this.generateExpr(a, 'expression'));
      if (context === 'statement') {
        this.mainCode.push(`${this.indent}kl_plot(${x}, ${y});`);
        return '';
      }
      return `kl_plot(${x}, ${y})`;
    }
    
    if (fn === 'scroll') {
      const dx = args.length > 0 ? this.generateExpr(args[0], 'expression') : '0';
      const dy = args.length > 1 ? this.generateExpr(args[1], 'expression') : '0';
      if (context === 'statement') {
        this.mainCode.push(`${this.indent}kl_scroll(${dx}, ${dy});`);
        return '';
      }
      return `kl_scroll(${dx}, ${dy})`;
    }
    
    if (fn === 'blur') {
      const amount = args.length > 0 ? this.generateExpr(args[0], 'expression') : '1';
      if (context === 'statement') {
        this.mainCode.push(`${this.indent}kl_blur(${amount});`);
        return '';
      }
      return `kl_blur(${amount})`;
    }
    
    if (fn === 'write') {
      const text = this.generateExpr(args[0], 'expression');
      const x = this.generateExpr(args[1], 'expression');
      const y = this.generateExpr(args[2], 'expression');
      
      // Handle number to string conversion
      const textExpr = args[0].type === 'string' ? text : `snprintf(temp_str, 32, "%g", (float)${text}), temp_str`;
      
      if (context === 'statement') {
        if (args[0].type !== 'string') {
          this.mainCode.push(`${this.indent}snprintf(temp_str, 32, "%g", (float)${text});`);
          this.mainCode.push(`${this.indent}kl_write(temp_str, ${x}, ${y});`);
        } else {
          this.mainCode.push(`${this.indent}kl_write(${text}, ${x}, ${y});`);
        }
        return '';
      }
      return `kl_write(${text}, ${x}, ${y})`;
    }
    
    // Math
    if (fn === '+') {
      return `(${args.map(a => this.generateExpr(a, 'expression')).join(' + ')})`;
    }
    
    if (fn === '-') {
      return `(${args.map(a => this.generateExpr(a, 'expression')).join(' - ')})`;
    }
    
    if (fn === '*') {
      return `(${args.map(a => this.generateExpr(a, 'expression')).join(' * ')})`;
    }
    
    if (fn === '/') {
      return `(${args.map(a => this.generateExpr(a, 'expression')).join(' / ')})`;
    }
    
    if (fn === '%') {
      return `((int)(${this.generateExpr(args[0], 'expression')}) % (int)(${this.generateExpr(args[1], 'expression')}))`;
    }
    
    if (fn === 'sin') {
      return `kl_sin(${this.generateExpr(args[0], 'expression')})`;
    }
    
    if (fn === 'cos') {
      return `kl_cos(${this.generateExpr(args[0], 'expression')})`;
    }
    
    if (fn === 'random') {
      return `kl_random(${this.generateExpr(args[0], 'expression')})`;
    }
    
    if (fn === 'wiggle') {
      return `kl_wiggle(${this.generateExpr(args[0], 'expression')})`;
    }
    
    // Comparisons
    if (fn === '>' || fn === '<' || fn === '=' || fn === '>=' || fn === '<=') {
      const op = fn === '=' ? '==' : fn;
      return `(${this.generateExpr(args[0], 'expression')} ${op} ${this.generateExpr(args[1], 'expression')})`;
    }
    
    // Control flow
    if (fn === 'if') {
      const cond = this.generateExpr(args[0], 'expression');
      const then = args[1];
      const els = args[2];
      
      if (context === 'statement') {
        this.mainCode.push(`${this.indent}if (${cond}) {`);
        const oldIndent = this.indent;
        this.indent = this.indent + '    ';
        this.generateExpr(then, 'statement');
        this.indent = oldIndent;
        if (els) {
          this.mainCode.push(`${this.indent}} else {`);
          this.indent = this.indent + '    ';
          this.generateExpr(els, 'statement');
          this.indent = oldIndent;
        }
        this.mainCode.push(`${this.indent}}`);
        return '';
      }
      
      return `(${cond} ? ${this.generateExpr(then, 'expression')} : ${els ? this.generateExpr(els, 'expression') : '0'})`;
    }
    
    if (fn === 'repeat') {
      const count = this.generateExpr(args[0], 'expression');
      const iterVar = args.length > 2 ? args[1].value.replace(/-/g, '_') : 'i';
      const body = args.length > 2 ? args.slice(2) : [args[1]];
      
      this.mainCode.push(`${this.indent}for (int ${iterVar} = 0; ${iterVar} < ${count}; ${iterVar}++) {`);
      const oldIndent = this.indent;
      this.indent = this.indent + '    ';
      for (const expr of body) {
        this.generateExpr(expr, 'statement');
      }
      this.indent = oldIndent;
      this.mainCode.push(`${this.indent}}`);
      return '';
    }
    
    // Input
    if (fn === 'crank') return 'kl_crank()';
    if (fn === 'crank-delta') return 'kl_crank_delta()';
    if (fn === 'button-a') return 'kl_button_a()';
    if (fn === 'button-b') return 'kl_button_b()';
    if (fn === 'button-up') return 'kl_button_up()';
    if (fn === 'button-down') return 'kl_button_down()';
    if (fn === 'button-left') return 'kl_button_left()';
    if (fn === 'button-right') return 'kl_button_right()';
    if (fn === 'button-pressed-a') return 'kl_button_pressed_a()';
    if (fn === 'button-pressed-b') return 'kl_button_pressed_b()';
    
    // System
    if (fn === 'width') return 'kl_width()';
    if (fn === 'height') return 'kl_height()';
    if (fn === 'frame') return 'kl_frame()';
    
    // Unknown function - treat as call
    console.warn(`Unknown function: ${fn}`);
    return `${fn}(${args.map(a => this.generateExpr(a, 'expression')).join(', ')})`;
  }
  
  buildOutput() {
    // Convert source code to comma syntax for display
    // (ink black) (line) (scroll 1) -> ink black, line, scroll 1
    const sourceLines = (this.sourceCode || '')
      .split('\n')
      .filter(line => !line.trim().startsWith(';'))  // Skip comment lines
      .map(line => line.trim())
      .filter(line => line.length > 0);
    
    // Convert each line from paren syntax to comma syntax
    const commaSyntax = sourceLines
      .map(line => {
        // Remove outer parens and convert to comma format
        return line
          .replace(/^\(/, '')      // Remove leading (
          .replace(/\)$/, '')      // Remove trailing )
          .replace(/\)\s*\(/g, ', '); // Replace )( with ,
      })
      .join(', ');
    
    // Split into display lines (max ~40 chars each for readability)
    const displayLines = [];
    let current = '';
    for (const part of commaSyntax.split(', ')) {
      if (current.length + part.length + 2 > 45 && current.length > 0) {
        displayLines.push(current);
        current = part;
      } else {
        current = current ? current + ', ' + part : part;
      }
    }
    if (current) displayLines.push(current);
    
    // Escape for C string
    const escapedLines = displayLines.slice(0, 5).map(line => 
      line.replace(/\\/g, '\\\\').replace(/"/g, '\\"')
    );
    
    const sourceName = this.sourceName || 'kidlisp';
    
    let output = `// Generated by kidlisp-to-pd.mjs
// Do not edit - regenerate from source

#include <stdio.h>
#include <stdlib.h>
#include "pd_api.h"
#include "kidlisp.h"

// Temp string buffer for number display
static char temp_str[32];

// Source code label
static const char* source_name = "${sourceName}";
static const char* source_lines[] = {
${escapedLines.map(line => `    "${line}"`).join(',\n')}
};
static const int source_line_count = ${escapedLines.length};

// Forward declarations
static int update(void* userdata);

// === Variables ===
`;
    
    // Generate variable declarations
    for (const [name, type] of this.variables) {
      output += `static float var_${name} = 0;\n`;
    }
    
    output += `
// === Main Loop ===
static void kidlisp_main(void) {
`;
    
    // Generate main code
    output += this.mainCode.join('\n') + '\n';
    
    output += `}

// Draw source code HUD directly with outlined text (after effects)
static void draw_source_hud(void) {
    // Draw source lines (compact, 8px spacing)
    for (int i = 0; i < source_line_count && i < 5; i++) {
        kl_write_outlined(source_lines[i], 5, 4 + i * 8);
    }
    
    // Draw QR code in bottom right
    kl_draw_qr();
}

// Playdate event handler
#ifdef _WINDLL
__declspec(dllexport)
#endif
int eventHandler(PlaydateAPI* pd, PDSystemEvent event, uint32_t arg) {
    (void)arg;
    
    if (event == kEventInit) {
        kl_init(pd);
        pd->system->setUpdateCallback(update, pd);
    }
    
    return 0;
}

// Frame update
static int update(void* userdata) {
    (void)userdata;
    kl_update();
    kidlisp_main();
    draw_source_hud();
    return 1;
}
`;
    
    return output;
  }
}

// Main
function main() {
  const args = process.argv.slice(2);
  
  if (args.length < 2) {
    console.log('Usage: kidlisp-to-pd.mjs <input.lisp> <output.c>');
    console.log('');
    console.log('Compiles KidLisp source to Playdate C code.');
    process.exit(1);
  }
  
  const [inputPath, outputPath] = args;
  
  console.log(`📖 Reading ${inputPath}...`);
  const source = readFileSync(inputPath, 'utf-8');
  
  // Extract just the filename without extension for the label
  const sourceName = basename(inputPath, '.lisp');
  
  console.log('🔤 Tokenizing...');
  const tokens = tokenize(source);
  
  console.log('🌳 Parsing...');
  const ast = parse(tokens);
  
  console.log('⚙️  Generating C code...');
  const generator = new CodeGenerator();
  generator.sourceName = sourceName;
  generator.sourceCode = source;
  const cCode = generator.generate(ast);
  
  console.log(`💾 Writing ${outputPath}...`);
  writeFileSync(outputPath, cCode);
  
  console.log('✅ Done!');
}

main();
