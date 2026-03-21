// Apple Developer Merchant ID Domain Association, 23.11.20.14.49
// This fulfills the requirements here: https://dashboard.stripe.com/login?redirect=%2Fsettings%2Fpayment_method_domains

import fs from "fs";
import path from "path";

export async function handler(event) {
  const host = event.headers.host;

  let filename;
  if (host.includes("botce.ac")) {
    filename = "botce.ac-apple-developer-merchantid-domain-association";
    //} else if (host.includes("aesthetic.computer")) {
  } else {
    filename =
      "aesthetic.computer-apple-developer-merchantid-domain-association";
    //} else {
    //  return { statusCode: 404, body: "Not Found" };
  }

  // Construct the path to the file
  const filePath = path.join(
    __dirname,
    "..",
    "..",
    "public",
    ".well-known",
    filename,
  );

  // Check if the file exists and read it
  try {
    if (!fs.existsSync(filePath)) {
      return { statusCode: 404, body: "File not found" };
    }
    const fileContents = fs.readFileSync(filePath, "utf8");

    return {
      statusCode: 200,
      body: fileContents,
      headers: { "Content-Type": "text/plain" },
    };
  } catch (error) {
    return { statusCode: 500, body: `Error reading file: ${error.message}` };
  }
}
