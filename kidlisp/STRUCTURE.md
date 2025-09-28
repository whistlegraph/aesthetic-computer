# KidLisp Directory Structure

This directory contains all KidLisp-related documentation, tools, examples, and reports.

## 📁 Directory Organization

```
kidlisp/
├── README.md                            # 📖 Main KidLisp documentation & overview
├── STRUCTURE.md                         # 🗺️ This navigation guide
├── COMPLETE_API_MAP.md                  # 🎯 Complete function reference (118 functions)
├── docs/                                # 📚 Detailed documentation
│   ├── README.md                        # Documentation directory overview
│   ├── suck-complete-technical-guide.md # 🌪️ Complete suck function documentation
│   ├── suck-function-fixed.md           # Historical suck implementation notes
│   ├── suck-redesign-notes.md           # Design evolution notes
│   ├── core/                            # Core language features
│   │   └── language-reference.md         # Language syntax & constructs
│   ├── features/                        # Feature-specific docs
│   │   ├── embedding-system.md           # Code embedding system
│   │   ├── feed-system.md               # Feed integration
│   │   ├── suck-lossless-implementation.md # Suck algorithm details
│   │   └── transformation-functions.md   # All transformation docs
│   ├── implementation/                  # Implementation guides
│   │   ├── blur-buffer-issues.md         # Blur system implementation
│   │   ├── embedding-fixes.md           # Embedding system fixes
│   │   ├── make-integration.md          # Build system integration
│   │   ├── singleton-refactor.md        # Singleton pattern refactor
│   │   ├── todo-fixes.md                # TODO list fixes
│   │   └── unified-kidlisp-execution.md # Execution system
│   ├── integration/                     # External integrations
│   │   └── x-ff1-art-computer.md        # Feral File integration
│   └── reports/                         # Technical analysis
│       ├── fa2-contract-spec.md          # FA2 contract specification
│       └── feral-file-integration.md     # Feral File technical details
├── examples/                            # 📋 Code examples & tutorials
│   └── README.md                        # Examples directory overview
├── reports/                             # 📊 Technical reports & analysis
│   ├── README.md                        # Reports directory overview
│   ├── analysis-report.md               # Architecture analysis
│   ├── comparison-metrics.md            # Performance comparisons
│   ├── implementation-guide.md          # Implementation guidelines
│   ├── zoom-vs-suck-implementation-report.md  # Transformation comparison
│   └── tinylisp/                        # TinyLisp comparison research
│       ├── README.md                    # TinyLisp research overview
│       ├── [... full tinylisp research archive ...]
└── tools/                               # 🔧 Development utilities
    ├── README.md                        # Tools directory overview
    ├── api-summary.mjs                  # 🎯 API analysis tool (118 functions)
    ├── get-source.mjs                   # Source extraction utility
    └── source-tree.mjs                  # Codebase analysis tool
```

## 📚 What's Where

- **README.md**: Start here! Main language documentation and API reference
- **docs/**: Function-specific documentation, design notes, and knowledge base
- **reports/**: Technical analysis, architecture decisions, implementation comparisons
- **examples/**: Working KidLisp code examples and demos
- **tools/**: Development utilities and helper scripts

## 🎯 Quick Navigation

- **Learning KidLisp**: Start with [README.md](README.md)
- **Function Documentation**: Check [docs/](docs/) for specific functions
- **Technical Details**: See [reports/](reports/) for implementation analysis
- **Code Examples**: Browse [examples/](examples/) for working programs
- **Development**: Use [tools/](tools/) for development utilities

This organization keeps all KidLisp-related materials together while maintaining clear separation between user documentation, technical reports, and development resources.
