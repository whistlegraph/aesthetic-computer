import { Configuration, OpenAIApi } from "openai";
import Busboy from "busboy";
import { respond } from "../../backend/http.mjs";
import { PassThrough } from "stream";

export async function handler(event, context) {
  if (event.httpMethod !== "POST")
    return respond(405, { error: "Wrong request type." });

  const form = await parseFormData(event);
  form.image.data.name = "painting.png";

  try {
    const configuration = new Configuration({
      apiKey: process.env.OPENAI_API_KEY,
    });
    const openai = new OpenAIApi(configuration);

    const response = await openai.createImageVariation(
      form.image.data,
      1,
      "256x256"
    );

    // Fetch the returned image URL and respond with the file.
    const { got } = await import("got"); // Import "got"
    const imageResponse = await got.get(response.data.data[0].url, {
      responseType: "buffer",
    });

    const image = imageResponse.body; // Buffer

    return {
      statusCode: 200,
      headers: { "Content-Type": imageResponse.headers["content-type"] },
      body: image.toString("base64"), // Convert buffer to base64 string.
      isBase64Encoded: true, // Set isBase64Encoded to true
    };
  } catch (error) {
    console.log(error);
    return respond(500, { error: `Error varying painting: ${error}` });
  }
}

// Use `busboy` to parse form data.
function parseFormData(event) {
  return new Promise((resolve, reject) => {
    const busboy = Busboy({
      headers: { "content-type": event.headers["content-type"] },
    });
    let formData = {};

    busboy.on("field", (fieldname, val) => (formData[fieldname] = val));

    busboy.on("file", (fieldname, file, filename, encoding, mimetype) => {
      let buffers = [];
      file.on("data", (data) => buffers.push(data));
      file.on("end", () => {
        formData[fieldname] = {
          data: Buffer.concat(buffers),
          filename,
          encoding,
          mimetype,
        };
      });
    });

    busboy.on("finish", () => resolve(formData));
    busboy.on("error", (error) => reject(error));

    busboy.write(
      event.body,
      event.isBase64Encoded ? "base64" : "binary",
      (err) => {
        if (err) reject(err);
        else busboy.end();
      }
    );
  });
}
