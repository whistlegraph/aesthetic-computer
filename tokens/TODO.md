# 🎯 Teia Packaging TODOs

## Next Priority: KidLisp Cache Packaging

### Goal
Package KidLisp pieces from MongoDB cache as static, runnable OBJKT packages using the existing pipeline but optimized for minimal size.

### Requirements
- [ ] **Source from MongoDB**: Load KidLisp code from the kidlisp cache in the database
- [ ] **Minimal Dependencies**: Only bundle essential libraries needed for KidLisp execution
- [ ] **No AC Disks**: Exclude unnecessary aesthetic.computer disks to keep packages thin
- [ ] **No User Auth**: Strip out all authentication and user-dependent features
- [ ] **Standalone Execution**: Ensure KidLisp pieces can run independently in Teia's sandbox

### Technical Considerations
- Identify minimal lib dependencies for KidLisp runtime
- Create a separate packing mode for database-sourced pieces
- Implement selective bundling (only kidlisp.mjs and core dependencies)
- Add database connection for fetching cached KidLisp code
- Ensure proper Teia compatibility for interpreted KidLisp pieces

### Implementation Notes
- Extend `ac-pack.mjs` with `--kidlisp-cache` or `--minimal` flags
- Query MongoDB for specific KidLisp piece by ID/name
- Bundle only: kidlisp.mjs, core helpers, minimal runtime
- Skip heavy dependencies like 3D, audio, networking modules
- Test with existing KidLisp pieces in cache

---

## Completed ✅
- [x] Basic Teia packaging pipeline
- [x] File organization and gitignore setup  
- [x] Documentation consolidation
- [x] Static HTML generation with Teia integration
- [x] Auth0 stripping (with known syntax issues to fix)
- [x] Comprehensive dependency bundling (43+ libs + 3 systems)

## Future Enhancements
- [ ] Fix remaining auth0 syntax errors in boot.mjs modification
- [ ] Optimize bundle sizes for different piece types
- [ ] Add piece-specific dependency analysis
- [ ] Create automated testing for OBJKT packages
- [ ] Support for other piece formats beyond .mjs/.lisp
