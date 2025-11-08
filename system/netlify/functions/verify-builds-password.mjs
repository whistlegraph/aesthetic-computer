export default async (req, context) => {
  if (req.method !== 'POST') {
    return new Response(JSON.stringify({ success: false }), {
      status: 405,
      headers: { 'Content-Type': 'application/json' }
    });
  }

  try {
    const { password } = await req.json();
    const correctPassword = process.env.BUILDS_PASSWORD || 'BuildM@chine1@';

    return new Response(
      JSON.stringify({ success: password === correctPassword }),
      {
        status: 200,
        headers: { 'Content-Type': 'application/json' }
      }
    );
  } catch (error) {
    return new Response(JSON.stringify({ success: false }), {
      status: 400,
      headers: { 'Content-Type': 'application/json' }
    });
  }
};
