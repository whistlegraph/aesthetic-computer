import { execFileSync } from "node:child_process";
import { existsSync, mkdirSync, readdirSync, renameSync, rmSync, statSync } from "node:fs";
import { join } from "node:path";

// Keep full first pages separate from the square illustration tiles in thumbs/.
export function generatePagePreviews(siteDir) {
  const directory = join(siteDir, "previews");
  mkdirSync(directory, { recursive: true });
  let count = 0;
  for (const file of readdirSync(siteDir).filter(name => name.endsWith(".pdf"))) {
    const pdf = join(siteDir, file);
    const output = join(directory, file.replace(/\.pdf$/, ".jpg"));
    if (!existsSync(output) || statSync(output).mtimeMs < statSync(pdf).mtimeMs) {
      const temporary = output + ".tmp";
      try {
        execFileSync("pdftoppm", ["-f", "1", "-singlefile", "-scale-to", "1100",
          "-jpeg", "-jpegopt", "quality=85", pdf, temporary], { timeout: 30000, stdio: "pipe" });
        renameSync(temporary + ".jpg", output);
      } catch (error) {
        console.warn(`Preview unavailable: ${file}: ${error.message.split("\n")[0]}`);
      } finally {
        rmSync(temporary + ".jpg", { force: true });
      }
    }
    if (existsSync(output)) count++;
  }
  return count;
}
