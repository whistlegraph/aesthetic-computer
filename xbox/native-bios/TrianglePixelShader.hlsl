struct PixelInput {
  float4 position : SV_POSITION;
  float4 color : COLOR0;
};

float4 main(PixelInput input) : SV_TARGET {
  // Screen-door transparency keeps depth correct without sorting or an
  // additional full-screen pass. FXAA softens the fine coverage pattern.
  if(input.color.a<0.999){
    const uint2 pixel=uint2(input.position.xy)&3;
    const uint pattern[16]={0,8,2,10,12,4,14,6,3,11,1,9,15,7,13,5};
    clip(input.color.a-(pattern[pixel.y*4+pixel.x]+0.5)/16.0);
  }
  return float4(input.color.rgb,1.0);
}
