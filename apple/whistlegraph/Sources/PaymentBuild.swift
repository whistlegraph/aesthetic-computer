// Wallet checkout and NFT minting are experiments installed directly with Xcode.
// App Store and TestFlight builds use Release. Do not enable these with an
// account check, remote flag, receipt environment, or App Review detection.
#if WHISTLEGRAPH_INTERNAL_PAYMENTS && !DEBUG
#error("Internal wallet testing requires a Debug build and cannot ship in Release.")
#endif
