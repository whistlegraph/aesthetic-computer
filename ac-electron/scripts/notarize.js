// @ts-check
const { notarize } = require("@electron/notarize");
const path = require("path");

/**
 * Notarize the macOS app after signing
 * Called by electron-builder via afterSign hook
 */
exports.default = async function notarizing(context) {
  const { electronPlatformName, appOutDir } = context;

  if (electronPlatformName !== "darwin") {
    console.log("Skipping notarization - not macOS");
    return;
  }

  if (process.env.SKIP_NOTARIZE) {
    console.log("Skipping notarization - SKIP_NOTARIZE set");
    return;
  }

  const appleId = process.env.APPLE_ID;
  const appleIdPassword =
    process.env.APPLE_APP_SPECIFIC_PASSWORD || process.env.APPLE_NOTARIZE_PWD;
  const teamId = process.env.APPLE_TEAM_ID;

  if (!appleId || !appleIdPassword || !teamId) {
    console.log("Skipping notarization - missing credentials");
    console.log("  APPLE_ID:", appleId ? "✓" : "✗");
    console.log("  APPLE_APP_SPECIFIC_PASSWORD:", appleIdPassword ? "✓" : "✗");
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
