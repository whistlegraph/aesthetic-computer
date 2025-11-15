# KidLisp Malformed Code Protection Plan

## Problem Statement

When KidLisp code has syntax errors (especially missing closing parentheses), the system crashes with errors like:
```
❌ Error updating nested embedded layer: TypeError: api.page is not a function
```

This happens repeatedly and floods the console, making the system unusable.

## Root Causes

1. **Missing Pre-Validation**: Code is parsed and executed without checking for syntax validity first
2. **Auto-Closing Limitations**: While `tokenize()` attempts to auto-close parentheses, malformed code can still slip through
3. **No Error Recovery in Embedded Layers**: When an embedded layer fails, it continues to retry every frame
4. **Missing API Safety Checks**: The code assumes `api.page` exists without verifying

## Solution Architecture

### Phase 1: Pre-Execution Validation (Linting Layer)

Add a validation step before code execution that checks for:
- Balanced parentheses
- Valid syntax structure
- Safe AST structure
- Recognized function names (optional warning)

```javascript
class KidLispValidator {
  validate(input) {
    const errors = [];
    const warnings = [];
    
    // 1. Check parenthesis balance
    const parenBalance = this.checkParenthesisBalance(input);
    if (parenBalance !== 0) {
      errors.push({
        type: 'UNBALANCED_PARENS',
        message: `Missing ${Math.abs(parenBalance)} closing ${parenBalance > 0 ? ')' : '('}`,
        severity: 'error'
      });
    }
    
    // 2. Check for common syntax errors
    this.checkSyntaxErrors(input, errors);
    
    // 3. Try to parse and catch tokenization errors
    try {
      const tokens = tokenize(input);
      const ast = readFromTokens([...tokens]);
      
      // 4. Validate AST structure
      this.validateAST(ast, errors, warnings);
    } catch (e) {
      errors.push({
        type: 'PARSE_ERROR',
        message: e.message,
        severity: 'error'
      });
    }
    
    return {
      valid: errors.length === 0,
      errors,
      warnings
    };
  }
  
  checkParenthesisBalance(input) {
    let balance = 0;
    for (const char of input) {
      if (char === '(') balance++;
      if (char === ')') balance--;
    }
    return balance;
  }
  
  checkSyntaxErrors(input, errors) {
    // Check for mismatched quotes
    const doubleQuotes = (input.match(/"/g) || []).length;
    const singleQuotes = (input.match(/'/g) || []).length;
    
    if (doubleQuotes % 2 !== 0) {
      errors.push({
        type: 'UNMATCHED_QUOTE',
        message: 'Unmatched double quote',
        severity: 'error'
      });
    }
    
    if (singleQuotes % 2 !== 0) {
      errors.push({
        type: 'UNMATCHED_QUOTE',
        message: 'Unmatched single quote',
        severity: 'error'
      });
    }
  }
  
  validateAST(ast, errors, warnings) {
    // Walk the AST and check for structural issues
    // This can be expanded based on common error patterns
  }
}
```

### Phase 2: Safe Error Recovery

Modify the `parse()` method to catch and handle validation errors:

```javascript
parse(input) {
  const parseStart = this.startTiming('parse');
  
  // Pre-validation step
  if (!this.validator) {
    this.validator = new KidLispValidator();
  }
  
  const validation = this.validator.validate(input);
  
  if (!validation.valid) {
    // Store validation errors for debugging
    this.lastValidationErrors = validation.errors;
    
    // Log errors to console
    console.error('❌ KidLisp Validation Failed:', validation.errors);
    
    // Return a safe empty AST instead of crashing
    this.endTiming('parse', parseStart);
    return [];
  }
  
  // Continue with existing parsing logic...
  try {
    // ... existing code ...
  } catch (error) {
    console.error('❌ KidLisp Parse Error:', error);
    this.lastParseError = error;
    this.endTiming('parse', parseStart);
    return [];
  }
}
```

### Phase 3: Embedded Layer Safety

Add safety checks in `updateEmbeddedLayer()`:

```javascript
async updateEmbeddedLayer(embeddedLayer, api, env) {
  let didRender = false;

  try {
    // Validate that api has required methods
    if (!api || typeof api.page !== 'function') {
      console.error(`❌ Invalid API object for embedded layer ${embeddedLayer.cacheId}`);
      return didRender;
    }
    
    // Check if layer has valid parsed code
    if (!embeddedLayer.parsedCode || embeddedLayer.parsedCode.length === 0) {
      console.warn(`⚠️ Empty parsed code for embedded layer ${embeddedLayer.cacheId}`);
      return didRender;
    }
    
    // Continue with existing logic...
    const originalPage = api.screen;
    api.page(embeddedLayer.buffer);
    
    // ... rest of existing code ...
    
  } catch (error) {
    console.error(`❌ Error in embedded layer ${embeddedLayer.cacheId}:`, error);
    
    // Mark layer as failed to prevent infinite retry loops
    embeddedLayer.failed = true;
    embeddedLayer.failureCount = (embeddedLayer.failureCount || 0) + 1;
    
    // Stop retrying after 3 failures
    if (embeddedLayer.failureCount >= 3) {
      console.warn(`⚠️ Disabling failed embedded layer ${embeddedLayer.cacheId}`);
      embeddedLayer.disabled = true;
    }
    
    // Always try to restore original page
    try {
      if (api && typeof api.page === 'function' && api.screen) {
        api.page(api.screen);
      }
    } catch (restoreError) {
      console.error("❌ Failed to restore page:", restoreError);
    }
  }
  
  return didRender;
}
```

### Phase 4: User Feedback

Add visual feedback for validation errors:

```javascript
// In the appropriate rendering location
renderValidationErrors(api) {
  if (this.lastValidationErrors && this.lastValidationErrors.length > 0) {
    const errors = this.lastValidationErrors;
    let y = 10;
    
    api.ink('red');
    api.write(`Syntax Errors (${errors.length}):`, 10, y);
    y += 15;
    
    errors.forEach((error, i) => {
      api.ink('yellow');
      api.write(`${i + 1}. ${error.message}`, 10, y);
      y += 12;
    });
  }
}
```

## Implementation Steps

1. **Create KidLispValidator class** (new file: `kidlisp-validator.mjs`)
2. **Integrate validation into parse()** method
3. **Add safety checks to updateEmbeddedLayer()**
4. **Add failure tracking to prevent infinite retry**
5. **Add visual error feedback**
6. **Test with known malformed code**

## Testing Strategy

Test cases to verify:
1. Missing closing parenthesis: `(blue (ink rainbow`
2. Missing opening parenthesis: `blue) ink rainbow)`
3. Unmatched quotes: `(write "hello)`
4. Empty parentheses: `()()()`
5. Deeply nested malformed: `(repeat 10 (repeat 10 (line`
6. Comma-separated with error: `blue, ink rainbow, (line`

## Expected Outcomes

- ✅ No more system crashes from malformed code
- ✅ Clear error messages for users
- ✅ Graceful degradation (show previous valid frame)
- ✅ Performance maintained (validation is fast)
- ✅ Better developer experience

## Future Enhancements

- Auto-fix common errors
- Suggest corrections
- Real-time validation in editor
- Syntax highlighting based on validation
