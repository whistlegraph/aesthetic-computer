# Aesthetic Computer Documentation TODO

*Last updated: September 16, 2025*

## 🎯 Current Focus: Documentation System Setup

### Phase 1: Infrastructure Setup
- [ ] **Reorganize reference folder structure**
  - [x] Create main README.md with system overview
  - [x] Create TODO.md for tracking work
  - [ ] Create organized folder structure (api/, kidlisp/, pieces/, system/, tools/)
  - [ ] Move existing Ableton docs to ableton/ subfolder
  - [ ] Set up templates for consistent documentation

### Phase 2: API Documentation Analysis
- [ ] **Dynamic API Surface Analysis**
  - [ ] Create mock BIOS system for testing disk.mjs API
  - [ ] Build tool to automatically discover all exported functions from disk.mjs
  - [ ] Map function signatures, parameters, and return types
  - [ ] Cross-reference with docs.js manual documentation
  - [ ] Generate API coverage report showing documented vs undocumented functions

- [ ] **JavaScript API Reference**
  - [ ] Extract and document all graphics functions (wipe, ink, box, circle, etc.)
  - [ ] Document input handling functions (pointer, keyboard, touch)
  - [ ] Document audio/sound system API
  - [ ] Document networking/socket functions
  - [ ] Document storage/session functions
  - [ ] Document math/utility functions
  - [ ] Create examples for each API category

### Phase 3: KidLisp Documentation
- [ ] **Language Reference**
  - [ ] Extract LLM API spec from kidlisp.mjs into readable format
  - [ ] Create comprehensive syntax guide
  - [ ] Document all built-in functions with examples
  - [ ] Create tutorial progression from basic to advanced
  - [ ] Document integration with JavaScript API

- [ ] **KidLisp Function Catalog**
  - [ ] Graphics functions (wipe, ink, line, box, circle, tri, etc.)
  - [ ] Animation functions (wiggle, timing expressions)
  - [ ] Math operations (+, -, *, /, comparisons)
  - [ ] Variable and function definition (def, later)
  - [ ] Control flow and logic
  - [ ] Image/media functions (paste, stamp)

### Phase 4: Piece Documentation System
- [ ] **Piece Structure Documentation**
  - [ ] Document piece lifecycle (boot, paint, act, sim, beat, leave)
  - [ ] Explain piece metadata and configuration
  - [ ] Document export patterns and conventions
  - [ ] Create piece template examples

- [ ] **Piece Catalog System**
  - [ ] Tool to analyze existing pieces for documentation
  - [ ] Generate piece index with descriptions and features
  - [ ] Cross-reference pieces with API usage
  - [ ] Create searchable piece database

### Phase 5: Development Tools Integration
- [ ] **CLI Documentation Explorer**
  - [ ] Design command-line interface for browsing docs
  - [ ] Integration with existing kidlisp-tools
  - [ ] Search functionality across all documentation
  - [ ] Examples and code snippet browsing

- [ ] **Emacs Integration**
  - [ ] Create 'docs' tab configuration for Emacs
  - [ ] Key bindings for quick documentation lookup
  - [ ] Integration with AC development workflow
  - [ ] Live documentation updates

### Phase 6: Testing and Validation
- [ ] **Documentation Test Suite**
  - [ ] Automated testing of code examples in documentation
  - [ ] Validation that API documentation matches implementation
  - [ ] Link checking for all documentation references
  - [ ] Style and consistency checking

- [ ] **Coverage Analysis**
  - [ ] Generate reports on documentation coverage
  - [ ] Identify undocumented API functions
  - [ ] Track documentation debt and technical writing needs
  - [ ] Performance metrics for documentation updates

## 🔧 Technical Implementation Notes

### API Analysis Tools Needed
1. **Mock BIOS Creator**: Simulate AC environment for testing disk.mjs
2. **Function Extractor**: Parse disk.mjs to find all exported functions
3. **Signature Analyzer**: Determine function parameters and types
4. **Coverage Mapper**: Compare actual API vs documented API
5. **Example Validator**: Test all code examples in documentation

### Documentation Architecture
- **Markdown-based**: Easy to edit, version control, and integrate
- **Modular structure**: Separate concerns (API, language, pieces, system)
- **Cross-references**: Links between different documentation sections
- **Searchable**: CLI and web-based search capabilities
- **Executable examples**: Code snippets that can be tested automatically

### Integration Points
- **docs.js**: Web endpoint for API documentation
- **prompt.mjs**: Command discovery and piece launching
- **kidlisp-tools**: CLI development tools
- **Emacs config**: Editor integration for documentation browsing
- **VS Code extension**: IDE integration for development

## 📈 Success Metrics

- **API Coverage**: % of disk.mjs functions documented
- **Example Coverage**: % of documented functions with working examples  
- **KidLisp Coverage**: % of language features documented with examples
- **Piece Coverage**: % of existing pieces with documentation
- **Tool Integration**: Functional CLI explorer and Emacs integration
- **Developer Productivity**: Reduced time to find API information

## 🎨 Future Enhancements

- **Interactive Documentation**: Live examples that run in browser
- **Video Tutorials**: Recorded demonstrations of concepts
- **Community Contributions**: System for user-contributed documentation
- **Auto-generation**: Tools that update docs from code comments
- **Multilingual Support**: Documentation in multiple languages