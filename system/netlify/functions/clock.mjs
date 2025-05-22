export async function handler(event, context) {
  return {
    statusCode: 200,
    headers: { 'Content-Type': 'text/plain' },
    body: new Date().toISOString(),
  };
}