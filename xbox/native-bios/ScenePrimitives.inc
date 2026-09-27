// Included inside QuickJsEngine's anonymous namespace, after CallScope.
// Input ownership stays with QuickJS for the full native call.
struct SceneFloats {
  JSContext* ctx; JSValue buffer=JS_UNDEFINED; const float* data=nullptr; size_t count=0;
  explicit SceneFloats(JSContext* c):ctx(c){}
  ~SceneFloats(){JS_FreeValue(ctx,buffer);}
  bool read(JSValueConst value){
    if(JS_GetTypedArrayType(value)!=JS_TYPED_ARRAY_FLOAT32)return false;
    size_t off=0,len=0,stride=0,total=0;
    buffer=JS_GetTypedArrayBuffer(ctx,value,&off,&len,&stride);
    if(JS_IsException(buffer))return false;
    auto* bytes=JS_GetArrayBuffer(ctx,&total,buffer);
    if(!bytes||stride!=4||off>total||len>total-off)return false;
    data=reinterpret_cast<const float*>(bytes+off);count=len/4;return true;
  }
};
struct ScenePoint{double x,y,z;};
static ScenePoint SceneMix(ScenePoint a,ScenePoint b,double t){return {a.x+(b.x-a.x)*t,a.y+(b.y-a.y)*t,a.z+(b.z-a.z)*t};}
template<class Distance> static std::vector<ScenePoint> SceneClip(const std::vector<ScenePoint>& in,Distance dist){
  std::vector<ScenePoint> out;out.reserve(in.size()+2);
  for(size_t i=0;i<in.size();i++){auto a=in[i],b=in[(i+1)%in.size()];double da=dist(a),db=dist(b);if(da>=0)out.push_back(a);if((da>=0)!=(db>=0))out.push_back(SceneMix(a,b,da/(da-db)));}
  return out;
}
static void SceneTriangle(Graphics& g,ScenePoint a,ScenePoint b,ScenePoint c,Color ink){
  g.triangle({float(a.x),float(a.y),float(a.z),float(b.x),float(b.y),float(b.z),float(c.x),float(c.y),float(c.z),ink});
}
static Color SceneColor(double r,double g,double b){return {uint8_t(std::clamp(r,0.,255.)),uint8_t(std::clamp(g,0.,255.)),uint8_t(std::clamp(b,0.,255.)),255};}
JSValue SceneDisc(JSContext* ctx,JSValueConst,int argc,JSValueConst* args){
 auto* scope=static_cast<CallScope*>(JS_GetContextOpaque(ctx));double v[7]{};
 if(!scope||!scope->api||argc<7)return JS_ThrowTypeError(ctx,"disc3d requires x,y,z,radius,r,g,b");
 for(int i=0;i<7;i++)if(JS_ToFloat64(ctx,&v[i],args[i])||!std::isfinite(v[i]))return JS_ThrowTypeError(ctx,"non-finite disc");
 if(v[3]<0||v[3]>8192||std::abs(v[0])+v[3]>32200||std::abs(v[1])+v[3]>32200)return JS_UNDEFINED;
 int n=v[3]<6?8:v[3]<13?12:v[3]<26?16:v[3]<52?20:28;auto color=SceneColor(v[4],v[5],v[6]);
 ScenePoint center{v[0],v[1],v[2]},last{v[0]+v[3],v[1],v[2]};
 for(int i=1;i<=n;i++){double a=i*6.283185307179586/n;ScenePoint next{v[0]+std::cos(a)*v[3],v[1]+std::sin(a)*v[3],v[2]};SceneTriangle(scope->api->graphics,center,last,next,color);last=next;}
 return JS_UNDEFINED;
}
JSValue SceneCapsule(JSContext* ctx,JSValueConst,int argc,JSValueConst* args){
 auto* scope=static_cast<CallScope*>(JS_GetContextOpaque(ctx));double v[9]{};
 if(!scope||!scope->api||argc<9)return JS_ThrowTypeError(ctx,"capsule3d requires endpoints,z,width,r,g,b");
 for(int i=0;i<9;i++)if(JS_ToFloat64(ctx,&v[i],args[i])||!std::isfinite(v[i]))return JS_ThrowTypeError(ctx,"non-finite capsule");
 const double radius=v[5]*.5,dx=v[2]-v[0],dy=v[3]-v[1],length=std::hypot(dx,dy);
 if(radius<0||radius>8192)return JS_UNDEFINED;
 for(int i=0;i<4;i++)if(std::abs(v[i])+radius>32200)return JS_UNDEFINED;
 const double ux=length>1e-6?dx/length:1,uy=length>1e-6?dy/length:0,nx=-uy*radius,ny=ux*radius;
 const auto color=SceneColor(v[6],v[7],v[8]);auto& g=scope->api->graphics;
 ScenePoint a{v[0]+nx,v[1]+ny,v[4]},b{v[0]-nx,v[1]-ny,v[4]},c{v[2]+nx,v[3]+ny,v[4]},d{v[2]-nx,v[3]-ny,v[4]};SceneTriangle(g,a,b,c,color);SceneTriangle(g,b,d,c,color);
 const int n=radius<6?3:radius<13?4:radius<26?6:radius<52?8:12;
 for(int end:{-1,1}){const double x=end<0?v[0]:v[2],y=end<0?v[1]:v[3];ScenePoint center{x,y,v[4]},last{x+nx,y+ny,v[4]};
  for(int i=1;i<=n;i++){const double angle=i*3.141592653589793/n,side=std::cos(angle),along=std::sin(angle)*end*radius;ScenePoint next{x+nx*side+ux*along,y+ny*side+uy*along,v[4]};SceneTriangle(g,center,last,next,color);last=next;}}
 return JS_UNDEFINED;
}
JSValue SceneMesh(JSContext* ctx,JSValueConst,int argc,JSValueConst* args){
 auto* scope=static_cast<CallScope*>(JS_GetContextOpaque(ctx));SceneFloats verts(ctx),faces(ctx),camera(ctx);
 if(!scope||!scope->api||argc<3||!verts.read(args[0])||!faces.read(args[1])||!camera.read(args[2]))return JS_ThrowTypeError(ctx,"sceneMesh requires three Float32Arrays");
 if(verts.count%3||faces.count%10||camera.count!=27||verts.count>300000||faces.count>100000)return JS_ThrowRangeError(ctx,"sceneMesh layout or budget");
 const auto* m=camera.data;
 for(size_t i=0;i<27;i++)if(!std::isfinite(m[i]))return JS_ThrowTypeError(ctx,"non-finite camera");
 if(m[17]<=0||m[18]<-32200||m[19]<-32200||m[20]>32200||m[21]>32200||m[18]>=m[20]||m[19]>=m[21])return JS_ThrowRangeError(ctx,"invalid viewport");
 std::vector<ScenePoint> points;points.reserve(verts.count/3);
 for(size_t i=0;i<verts.count;i+=3){double x=verts.data[i]-m[0],y=verts.data[i+1]-m[1],z=verts.data[i+2]-m[2];if(!std::isfinite(x)||!std::isfinite(y)||!std::isfinite(z))return JS_ThrowTypeError(ctx,"non-finite vertex");points.push_back({x*m[3]+y*m[4]+z*m[5],x*m[6]+y*m[7]+z*m[8],x*m[9]+y*m[10]+z*m[11]});}
 // Validate the complete index stream before submitting any geometry.
 for(size_t i=0;i<faces.count;i+=10){for(int j=0;j<10;j++)if(!std::isfinite(faces.data[i+j]))return JS_ThrowTypeError(ctx,"non-finite face");for(int j=0;j<4;j++){double id=faces.data[i+j];if(id<0||id>=points.size()||std::floor(id)!=id)return JS_ThrowRangeError(ctx,"invalid mesh index");}}
 int emitted=0;
 for(size_t i=0;i<faces.count;i+=10){const auto* f=faces.data+i;std::vector<ScenePoint> poly{points[size_t(f[0])],points[size_t(f[1])],points[size_t(f[2])],points[size_t(f[3])]};
  poly=SceneClip(poly,[&](ScenePoint p){return p.z-m[17];});if(poly.size()<3)continue;
  for(auto& p:poly){double k=m[14]+(m[15]/p.z-m[14])*m[16];p={m[12]+p.x*k,m[13]-p.y*k,std::clamp(double(m[22])+p.z*m[23],-1.499,1.4)};}
  poly=SceneClip(poly,[&](ScenePoint p){return p.x-m[18];});poly=SceneClip(poly,[&](ScenePoint p){return m[20]-p.x;});poly=SceneClip(poly,[&](ScenePoint p){return p.y-m[19];});poly=SceneClip(poly,[&](ScenePoint p){return m[21]-p.y;});
  const double light=.72+std::max(0.,double(-f[7]*m[24]-f[8]*m[25]-f[9]*m[26]))*.28;const auto color=SceneColor(std::round(f[4]*light),std::round(f[5]*light),std::round(f[6]*light));
  for(size_t j=1;j+1<poly.size();j++){SceneTriangle(scope->api->graphics,poly[0],poly[j],poly[j+1],color);++emitted;}
 }
 return JS_NewInt32(ctx,emitted);
}
