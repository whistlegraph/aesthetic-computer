# Documentation Testing & Validation

*Testing framework for ensuring documentation accuracy and completeness*

## Overview

A comprehensive testing system to validate that documentation matches implementation, examples work correctly, and coverage is maintained across the Aesthetic Computer codebase.

## Test Categories

### 1. API Documentation Tests

#### Function Signature Validation
- **Purpose**: Ensure documented functions exist and have correct signatures
- **Method**: Compare `docs.js` entries with actual `disk.mjs` exports
- **Tests**:
  ```javascript
  describe('API Function Signatures', () => {
    it('should have all documented functions exported from disk.mjs', () => {
      const documented = extractDocumentedFunctions('docs.js');
      const exported = extractExportedFunctions('disk.mjs');
      
      documented.forEach(func => {
        expect(exported).toContain(func.name);
      });
    });
  });
  ```

#### Parameter Type Validation
- **Purpose**: Verify parameter types match documentation
- **Method**: Runtime analysis with mock inputs
- **Implementation**: Mock BIOS system for testing API functions

#### Example Code Validation
- **Purpose**: Ensure all code examples in documentation work
- **Method**: Execute examples in controlled environment
- **Tests**:
  ```javascript
  describe('Documentation Examples', () => {
    it('should execute graphics examples without errors', async () => {
      const examples = extractExamplesFromDocs('graphics');
      
      for (const example of examples) {
        const mockAPI = createMockAPI();
        await expect(executeExample(example, mockAPI)).resolves.not.toThrow();
      }
    });
  });
  ```

### 2. KidLisp Documentation Tests

#### Syntax Validation
- **Purpose**: Verify KidLisp syntax examples are valid
- **Method**: Parse examples through KidLisp interpreter
- **Tests**:
  ```javascript
  describe('KidLisp Syntax Examples', () => {
    it('should parse all syntax examples successfully', () => {
      const syntaxExamples = extractKidlispExamples('syntax');
      
      syntaxExamples.forEach(example => {
        expect(() => parseKidlisp(example.code)).not.toThrow();
      });
    });
  });
  ```

#### Function Documentation Coverage
- **Purpose**: Ensure all KidLisp functions are documented
- **Method**: Compare implemented functions with documented functions
- **Implementation**: Extract from `kidlisp.mjs` function registry

#### Example Program Execution
- **Purpose**: Verify KidLisp example programs run correctly
- **Method**: Execute in sandboxed KidLisp environment
- **Tests**: Validate visual output matches expected results

### 3. Piece Documentation Tests

#### Lifecycle Function Coverage
- **Purpose**: Ensure all piece lifecycle functions are documented
- **Method**: Scan existing pieces for function usage patterns
- **Tests**:
  ```javascript
  describe('Piece Lifecycle Documentation', () => {
    it('should document all lifecycle functions used in pieces', () => {
      const usedFunctions = scanPiecesForLifecycleFunctions();
      const documentedFunctions = extractDocumentedLifecycleFunctions();
      
      usedFunctions.forEach(func => {
        expect(documentedFunctions).toContain(func);
      });
    });
  });
  ```

#### Template Validation
- **Purpose**: Verify piece templates are valid and functional
- **Method**: Test templates as actual pieces
- **Implementation**: Load templates in mock AC environment

#### Cross-Reference Accuracy
- **Purpose**: Ensure piece documentation references are accurate
- **Method**: Validate all links and function references
- **Tests**: Check that referenced API functions exist

### 4. Link and Reference Tests

#### Internal Link Validation
- **Purpose**: Ensure all internal documentation links work
- **Method**: Parse markdown files and validate references
- **Tests**:
  ```javascript
  describe('Documentation Links', () => {
    it('should have valid internal references', () => {
      const links = extractInternalLinks(documentationFiles);
      
      links.forEach(link => {
        expect(fileExists(link.target)).toBe(true);
      });
    });
  });
  ```

#### External Link Validation
- **Purpose**: Check external links are accessible
- **Method**: HTTP requests to validate URLs
- **Implementation**: Periodic link checking with retry logic

#### Cross-Reference Consistency
- **Purpose**: Ensure references between docs are bidirectional
- **Method**: Build reference graph and check completeness

## Testing Framework

### Mock API System

#### Mock BIOS Implementation
```javascript
class MockBIOS {
  constructor() {
    this.screen = { width: 800, height: 600 };
    this.canvas = createMockCanvas();
    this.state = {};
  }
  
  // Implement minimal API for testing
  wipe(color) { /* mock implementation */ }
  ink(color) { /* mock implementation */ }
  box(x, y, w, h) { /* mock implementation */ }
  // ... other API functions
}
```

#### Sandboxed Execution Environment
- **Isolated context** for running examples
- **Resource limits** to prevent infinite loops
- **State capture** for validating results
- **Error handling** with detailed reporting

### Test Data Generation

#### API Function Discovery
```javascript
function extractAPIFunctions(diskPath) {
  const source = readFileSync(diskPath, 'utf8');
  
  // Parse AST to find exported functions
  const ast = parse(source, { sourceType: 'module' });
  
  return ast.body
    .filter(node => node.type === 'ExportNamedDeclaration')
    .map(node => extractFunctionInfo(node));
}
```

#### Documentation Parsing
```javascript
function extractDocumentationExamples(docsPath) {
  const content = readFileSync(docsPath, 'utf8');
  const tokens = marked.lexer(content);
  
  return tokens
    .filter(token => token.type === 'code')
    .filter(token => token.lang === 'javascript' || token.lang === 'lisp')
    .map(token => ({ code: token.text, lang: token.lang }));
}
```

### Continuous Integration

#### GitHub Actions Workflow
```yaml
name: Documentation Tests

on: [push, pull_request]

jobs:
  docs-tests:
    runs-on: ubuntu-latest
    
    steps:
    - uses: actions/checkout@v2
    
    - name: Setup Node.js
      uses: actions/setup-node@v2
      with:
        node-version: '18'
    
    - name: Install dependencies
      run: npm ci
    
    - name: Run API documentation tests
      run: npm run test:docs:api
    
    - name: Run KidLisp documentation tests  
      run: npm run test:docs:kidlisp
    
    - name: Run piece documentation tests
      run: npm run test:docs:pieces
    
    - name: Generate coverage report
      run: npm run docs:coverage
    
    - name: Upload coverage
      uses: codecov/codecov-action@v1
```

#### Local Development Workflow
```bash
# Run all documentation tests
npm run test:docs

# Run specific test suites
npm run test:docs:api
npm run test:docs:kidlisp
npm run test:docs:pieces

# Generate coverage report
npm run docs:coverage

# Update documentation cache
npm run docs:update

# Validate examples only
npm run test:examples
```

## Coverage Metrics

### API Coverage
- **Function Coverage**: % of API functions documented
- **Parameter Coverage**: % of parameters with type documentation
- **Example Coverage**: % of functions with working examples
- **Usage Coverage**: % of functions with real-world usage examples

### KidLisp Coverage
- **Syntax Coverage**: % of language features documented
- **Function Coverage**: % of built-in functions documented
- **Example Coverage**: % of concepts with examples
- **Tutorial Coverage**: % of language progression covered

### Piece Coverage
- **Lifecycle Coverage**: % of lifecycle functions documented
- **Pattern Coverage**: % of common patterns documented
- **Template Coverage**: % of use cases with templates
- **Integration Coverage**: % of API integration documented

## Reporting

### Test Results Dashboard
```
📊 Documentation Test Results

API Documentation:
  ✅ Function Signatures: 95% (142/150 functions)
  ✅ Examples: 78% (117/150 functions)
  ❌ Parameter Types: 65% (98/150 functions)

KidLisp Documentation:
  ✅ Syntax Examples: 100% (25/25 examples)
  ✅ Function Coverage: 88% (44/50 functions)
  ✅ Tutorial Progression: 92% (23/25 topics)

Piece Documentation:
  ✅ Lifecycle Functions: 100% (7/7 functions)
  ⚠️  Templates: 60% (6/10 use cases)
  ❌ Integration Examples: 45% (18/40 patterns)

Overall Score: 82% (456/556 total items)
```

### Actionable Reports
- **Missing Documentation**: List of undocumented functions
- **Broken Examples**: Examples that fail to execute
- **Outdated References**: Links and references that need updates
- **Coverage Gaps**: Areas needing attention

## Tools & Scripts

### Test Runner
```bash
#!/bin/bash
# test-documentation.sh

echo "🧪 Running Documentation Tests..."

# API Tests
echo "📚 Testing API Documentation..."
node tools/test-api-docs.mjs

# KidLisp Tests  
echo "🔤 Testing KidLisp Documentation..."
node tools/test-kidlisp-docs.mjs

# Piece Tests
echo "🧩 Testing Piece Documentation..."
node tools/test-piece-docs.mjs

# Link Tests
echo "🔗 Testing Documentation Links..."
node tools/test-links.mjs

echo "✅ Documentation tests complete!"
```

### Example Validator
```javascript
// validate-examples.mjs
import { executeMockPiece } from './mock-bios.mjs';
import { extractExamples } from './docs-parser.mjs';

async function validateExamples() {
  const examples = await extractExamples('/reference');
  const results = [];
  
  for (const example of examples) {
    try {
      await executeMockPiece(example.code);
      results.push({ ...example, status: 'pass' });
    } catch (error) {
      results.push({ ...example, status: 'fail', error: error.message });
    }
  }
  
  return results;
}
```

## Integration Points

- **CI/CD Pipeline**: Automated testing on every commit
- **Pre-commit Hooks**: Quick validation before commits
- **Documentation Updates**: Automatic test updates when docs change
- **Development Workflow**: Tests as part of development cycle
- **Release Process**: Documentation validation before releases

---

*This testing framework ensures documentation stays accurate, complete, and useful as the Aesthetic Computer system evolves.*