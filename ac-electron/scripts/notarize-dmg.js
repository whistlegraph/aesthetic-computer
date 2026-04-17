// @ts-check
const { execFileSync } = require("child_process");
const path = require("path");

// electron-builder's built-in notarize handles the .app bundle but not the
// DMG wrapper: the DMG ships unsigned, so `spctl -t install` reports
// "no usable signature" even after stapling a notarization ticket. This
// afterAllArtifactBuild hook signs each DMG with the same Developer ID,
// submits it to notarytool, and staples the ticket so fresh downloads pass
// Gatekeeper without a warning.
exports.default = async function notarizeDmg(context) {
  if (process.platform !== "darwin") return;
  if (process.env.SKIP_NOTARIZE) {
    console.log("Skipping DMG notarization - SKIP_NOTARIZE set");
    return;
  }

  const appleId = process.env.APPLE_ID;
  const password =
    process.env.APPLE_APP_SPECIFIC_PASSWORD || process.env.APPLE_NOTARIZE_PWD;
  const teamId = process.env.APPLE_TEAM_ID;
  const signIdentity =
    process.env.CSC_NAME ||
    `Developer ID Application: Jeffrey Scudder (${teamId || ""})`;

  if (!appleId || !password || !teamId) {
    console.log("Skipping DMG notarization - missing credentials");
    return;
  }

  const dmgs = (context.artifactPaths || []).filter((p) => p.endsWith(".dmg"));
  if (dmgs.length === 0) return;

  for (const dmg of dmgs) {
    const name = path.basename(dmg);
    console.log(`  • signing DMG ${name}`);
    execFileSync(
      "codesign",
      ["--sign", signIdentity, "--timestamp", "--force", dmg],
      { stdio: "inherit" }
    );
    console.log(`  • notarizing DMG ${name}`);
    execFileSync(
      "xcrun",
      [
        "notarytool",
        "submit",
        dmg,
        "--apple-id",
        appleId,
        "--password",
        password,
        "--team-id",
        teamId,
        "--wait",
      ],
      { stdio: "inherit" }
    );
    console.log(`  • stapling DMG ${name}`);
    execFileSync("xcrun", ["stapler", "staple", dmg], { stdio: "inherit" });
  }
};
