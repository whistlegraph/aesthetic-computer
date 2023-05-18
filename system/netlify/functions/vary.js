import { Configuration, OpenAIApi } from "openai";
import Busboy from "busboy";
import { respond } from "../../backend/http.mjs";

export async function handler(event, context) {
  if (event.httpMethod !== "POST") {
    return respond(405, { error: "Wrong request type." });
  }

  const data = await parseFormData(event);

  data.image.data.name = "painting.png";
  console.log(data.image.data);

  try {
    const configuration = new Configuration({
      apiKey: process.env.OPENAI_API_KEY,
    });
    const openai = new OpenAIApi(configuration);

    const response = await openai.createImageVariation(
      data.image.data,
      1,
      "256x256"
    );
    console.log(response);
    return respond(200, { url: response.data.data[0].url });
  } catch (error) {
    console.log(error);
    return respond(500, { error: `Error varying painting: ${error}` });
  }
}

function parseFormData(event) {
  return new Promise((resolve, reject) => {
    const busboy = Busboy({
      headers: { "content-type": event.headers["content-type"] },
    });
    let formData = {};

    busboy.on("field", (fieldname, val) => {
      formData[fieldname] = val;
    });

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

    busboy.on("finish", () => {
      resolve(formData);
    });

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
