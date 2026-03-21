# GIF Encoding Options

This system now supports two GIF encoding libraries:

## Libraries

### 1. gif.js (default)
- **Location**: `/aesthetic.computer/dep/gif/`
- **Type**: Web Worker based
- **Features**: Multi-threaded encoding, dithering support
- **Best for**: General purpose, complex images with gradients

### 2. gifenc
- **Location**: `/aesthetic.computer/dep/gifenc/`
- **Type**: Pure JavaScript, single-threaded
- **Features**: Faster encoding, smaller file size, optimized for V8
- **Best for**: Simple graphics, vector-style images, performance-critical applications

## Usage

To use gifenc instead of gif.js, add `useGifenc: true` to your gif creation request:

```javascript
send({
  type: "create-animated-gif",
  content: {
    frames: frameData,
    useGifenc: true  // Set to false or omit for gif.js
  }
});
```

## Integration Notes

1. **Package.json**: gifenc is added as a dependency
2. **Distribution**: Files are copied from `node_modules/gifenc/dist/` to `/dep/gifenc/`
3. **Loading**: gifenc uses ES modules (dynamic import), gif.js uses script tags
4. **Compatibility**: Both maintain the same API interface for the rest of the system

## Performance Comparison

Based on the gifenc documentation:
- **gifenc**: ~2.1 seconds for 150 1024x1024px frames with workers
- **gif.js**: Typically 2x slower than gifenc for similar content

## File Structure

```
/dep/
├── gif/
│   ├── gif.js
│   ├── gif.worker.js
│   └── *.map files
└── gifenc/
    ├── gifenc.esm.js
    ├── gifenc.js
    └── *.map files
```

## Testing

Use the `gif-test` piece to compare both encoders:
- Auto-demo runs at startup
- Manual controls: 'r' (record), 'g' (gif.js), 'e' (gifenc)
