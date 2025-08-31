# KidLisp Tools

Tools for analyzing and debugging KidLisp pieces.

## source-tree.fish

Analyzes KidLisp pieces and displays their embedded layer tree structure along with performance characteristics.

### Usage

```fish
./source-tree.fish <piece-name>
```

### Examples

```fish
# Analyze the $cow piece and its embedded layers
./source-tree.fish cow

# Analyze a specific embedded layer
./source-tree.fish 39i

# Works with or without $ prefix
./source-tree.fish $r2f
```

### Features

- **📁 Tree Structure**: Shows hierarchical structure of embedded layers
- **📄 Source Preview**: Displays first few lines of each piece's source code
- **🔍 Performance Analysis**: Identifies expensive operations (blur, zoom, contrast, etc.)
- **⏱️ Animation Detection**: Detects timing expressions and animations
- **🎲 Randomness Detection**: Identifies pieces using randomness (?)
- **📊 Layer Count**: Shows how many layers a piece embeds

### Output Explanation

- `📁` - Piece with embedded layers
- `📄` - Simple piece (no embedded layers)
- `⚠️` - Contains expensive operations that may impact performance
- `✅` - Clean piece with no expensive operations
- `⏱️` - Contains animations (timing expressions)
- `🎲` - Uses randomness
- `❌` - Piece not found

### Performance Analysis

The tool automatically detects these expensive operations:
- **blur** - Gaussian blur effects
- **zoom** - Scaling transformations
- **contrast** - Contrast adjustments
- **spin** - Rotation animations
- **flood** - Flood fill operations

Use this information to understand why certain pieces may have slower initial frames.

### Requirements

- fish shell
- curl
- Local dev server running (`npm run dev`)
- Access to `https://localhost:8888/.netlify/functions/store-kidlisp`

### Development

To add new analysis features, modify the `analyze_performance_features` function.
To improve parsing, update the `extract_embedded_pieces` function.
