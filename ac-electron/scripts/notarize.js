// @ts-check
const { notarize } = require("@electron/notarize");
const path = require("path");

/**
 * Notarize the macOS app after signing
 * Called by electron-builder via afterSign hook
 */
exports.default = async function notarizing(context) {
  const { electronPlatformName, appOutDir } = context;

  // Only notarize on macOS
  if (electronPlatformName !== "darwin") {
    console.log("Skipping notarization - not macOS");
    return;
  }

  // Skip if not in CI or explicitly disabled
  if (!process.env.CI && !process.env.FORCE_NOTARIZE) {
    console.log("Skipping notarization - not in CI (set FORCE_NOTARIZE=1 to override)");
    return;
  }

  // Check for required environment variables
  const appleId = process.env.APPLE_ID;
  const appleIdPassword = process.env.APPLE_NOTARIZE_PWD;
  const teamId = process.env.APPLE_TEAM_ID;

  if (!appleId || !appleIdPassword || !teamId) {
    console.log("Skipping notarization - missing credentials");
    console.log("  APPLE_ID:", appleId ? "✓" : "✗");
    console.log("  APPLE_NOTARIZE_PWD:", appleIdPassword ? "✓" : "✗");
    console.log("  APPLE_TEAM_ID:", teamId ? "✓" : "✗");
    return;
  }

  const appName = context.packager.appInfo.productFilename;
  const appPath = path.join(appOutDir, `${appName}.app`);

  console.log(`Notarizing ${appPath}...`);

  try {
    await notarize({
      tool: "notarytool",
      appPath,
      appleId,
      appleIdPassword,
      teamId,
    });
    console.log("Notarization complete!");
  } catch (error) {
    console.error("Notarization failed:", error);
    throw error;
  }
};
