// Generate New Piece, 23.05.22.21.57
// Produce source for a new piece from the `blank.mjs` template on the
// command line.

/* #region 🏁 TODO 
  - [] Make it so `this-piece` -> titleizes to: `This Piece`.
  - [] Abstract the replacement logic in order to produce AlI generated pieces.
    - [] And also share the same abstracted logic in `prompt''s `code` command.
#endregion */

import fs from "fs";
import path from "path";
import { fileURLToPath } from "url";
import { timestamp } from "./system/public/aesthetic.computer/lib/num.mjs";

async function generateNewPiece(name) {
  const capitalizedName = name.charAt(0).toUpperCase() + name.slice(1);

  const __filename = fileURLToPath(import.meta.url);
  const __dirname = path.dirname(__filename);

  const source = path.join(
    __dirname,
    "system",
    "public",
    "aesthetic.computer",
    "disks",
    "blank.mjs"
  );
  const destination = path.join(
    __dirname,
    "system",
    "public",
    "aesthetic.computer",
    "disks",
    `${name}.mjs`
  );

  await fs.promises.copyFile(source, destination);

  let fileContents = await fs.promises.readFile(destination, "utf-8");

  // Perform replacements from the `blank.mjs` template.
  fileContents = fileContents.replace(/\$NAME/g, capitalizedName);
  fileContents = fileContents.replace(/\$TIMESTAMP/g, timestamp());
  fileContents = fileContents.replace(
    /\$THIS_IS_A_TEMPLATE_FOR_MAKING_NEW_PIECES/g,
    process.argv.slice(3).join(" ")
  );

  await fs.promises.writeFile(destination, fileContents);
}

await generateNewPiece(process.argv[2]);
