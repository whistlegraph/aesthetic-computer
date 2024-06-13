export const handler = async (event, context) => {
  return {
    body: "<html><body><h1>sotce.net</h1></body></html>",
    statusCode: 200,
    headers: {
      "Content-Type": "text/html"
    }
  };
};