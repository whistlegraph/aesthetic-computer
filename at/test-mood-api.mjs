// Test script to check the mood API endpoint
// Run with: node test-mood-api.mjs

async function testMoodAPI() {
  const apiUrl = 'https://aesthetic.computer';
  
  console.log('🔍 Testing mood API endpoint...\n');
  console.log(`Fetching: ${apiUrl}/api/mood/all\n`);
  
  try {
    const response = await fetch(`${apiUrl}/api/mood/all`);
    
    console.log('Status:', response.status);
    console.log('Status Text:', response.statusText);
    console.log('Headers:', Object.fromEntries(response.headers.entries()));
    console.log('\n');
    
    if (!response.ok) {
      console.error('❌ Response not OK');
      const text = await response.text();
      console.error('Response body:', text);
      return;
    }
    
    const data = await response.json();
    console.log('✅ Successfully fetched data\n');
    console.log('Response type:', Array.isArray(data) ? 'Array' : typeof data);
    
    if (Array.isArray(data)) {
      console.log(`📊 Direct array with ${data.length} moods`);
      console.log('\nFirst 3 moods:');
      data.slice(0, 3).forEach((mood, i) => {
        console.log(`\n${i + 1}.`, JSON.stringify(mood, null, 2));
      });
    } else if (data && data.moods && Array.isArray(data.moods)) {
      console.log(`📊 Object with moods property containing ${data.moods.length} moods`);
      console.log('\nFirst 3 moods:');
      data.moods.slice(0, 3).forEach((mood, i) => {
        console.log(`\n${i + 1}.`, JSON.stringify(mood, null, 2));
      });
    } else {
      console.log('⚠️  Unexpected data structure:');
      console.log(JSON.stringify(data, null, 2));
    }
    
    // Test with a specific handle
    console.log('\n\n🔍 Testing with specific handle (@digitpain)...\n');
    const handleResponse = await fetch(`${apiUrl}/api/mood/all?for=@digitpain`);
    const handleData = await handleResponse.json();
    
    if (handleData && handleData.moods) {
      console.log(`📊 Found ${handleData.moods.length} moods for @digitpain`);
      if (handleData.moods.length > 0) {
        console.log('\nLatest mood:');
        console.log(JSON.stringify(handleData.moods[0], null, 2));
      }
    } else {
      console.log('Response:', JSON.stringify(handleData, null, 2));
    }
    
  } catch (error) {
    console.error('❌ Error:', error.message);
    console.error(error);
  }
}

testMoodAPI();
