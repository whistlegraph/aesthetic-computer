# Aesthetic Computer Documentation System

## Overview
This folder serves as the central documentation hub for Aesthetic Computer, covering:

1. **JavaScript API Reference** - The interface provided by `disk.mjs` to AC pieces
2. **KidLisp Language Reference** - Syntax and functions for the KidLisp programming language
3. **Piece Documentation** - How to create and structure AC pieces
4. **System Architecture** - Understanding the AC ecosystem
5. **Development Tools** - CLI tools and development workflows

## Documentation Status & Coverage

### Current State
- **docs.js**: Comprehensive but manually maintained API docs for web interface
- **disk.mjs**: 9940 lines of API implementation, needs dynamic analysis
- **kidlisp.mjs**: 10085 lines with embedded LLM API spec, well documented
- **prompt.mjs**: Command list for piece discovery and LLM integration

### Goals
- [ ] Dynamic API surface analysis system
- [ ] Automated documentation coverage reporting  
- [ ] Test suite for validating API documentation
- [ ] Integrated CLI documentation explorer for Emacs
- [ ] Centralized piece documentation system

## Folder Structure

```
reference/
├── README.md (this file)
├── TODO.md (tracking documentation work)
├── api/
│   ├── javascript-api.md (disk.mjs API reference)
│   ├── analysis/ (dynamic API analysis tools)
│   └── coverage/ (documentation coverage reports)
├── kidlisp/
│   ├── language-reference.md (KidLisp syntax guide)
│   ├── functions.md (built-in functions)
│   └── examples/ (example KidLisp programs)
├── pieces/
│   ├── structure.md (how pieces work)
│   ├── templates/ (piece templates)
│   └── catalog/ (documented pieces)
├── system/
│   ├── architecture.md (AC system overview)
│   ├── boot-sequence.md (initialization process)
│   └── networking.md (socket/session systems)
├── tools/
│   ├── cli-explorer.md (emacs docs integration)
│   ├── testing.md (documentation testing)
│   └── scripts/ (analysis and validation tools)
└── ableton/ (existing Ableton Live integration docs)
    └── ... (current ableton files moved here)
```

## Next Steps

1. **Create folder structure** with proper organization
2. **Move existing Ableton docs** to dedicated subfolder
3. **Set up analysis tools** for dynamic API discovery
4. **Create documentation templates** for consistent formatting
5. **Integrate with development workflow** via Emacs and CLI tools

## Integration Points

- **docs.js**: Web-based API documentation endpoint
- **prompt.mjs**: Command discovery and piece launching  
- **kidlisp-tools**: CLI tools for KidLisp development
- **Emacs configuration**: 'docs' tab for reference browsing
- **VS Code extension**: Documentation integration for development

This system will provide comprehensive, up-to-date documentation that grows with the codebase and supports both interactive exploration and development workflows.