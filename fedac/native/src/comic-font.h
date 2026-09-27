#ifndef AC_COMIC_FONT_H
#define AC_COMIC_FONT_H
#ifdef HAVE_COMIC_FONT
#include <ft2build.h>
#include FT_FREETYPE_H
static FT_Library ac_comic_library;
static FT_Face ac_comic_face;
static int ac_comic_failed, ac_comic_px;
static struct ComicGlyph {
  uint32_t code; int px, width, height, left, top, baseline;
  double advance; unsigned char *coverage;
} ac_comic_cache[2048];
static int ac_comic_draw(ACFramebuffer *fb,const char *text,double x,double y,double size,
    uint8_t r,uint8_t g,uint8_t b) {
  if (!ac_comic_face) {
    if (ac_comic_failed) return 0;
    if (FT_Init_FreeType(&ac_comic_library) ||
        FT_New_Face(ac_comic_library,"/fonts/ComicRelief-Regular.ttf",0,&ac_comic_face)) {
      ac_comic_failed=1;return 0;
    }
  }
  int px=(int)fmax(5,fmin(192,round(size)));
  if (ac_comic_px!=px) { FT_Set_Pixel_Sizes(ac_comic_face,0,px); ac_comic_px=px; }
  double cursor=x; int baseline=(int)round(y)+(ac_comic_face->size->metrics.ascender>>6);
  const unsigned char *s=(const unsigned char *)text;
  while(*s) {
    uint32_t code=*s++;
    if ((code&0xe0)==0xc0 && *s) { code=(code&31)<<6;code|=*s++&63; }
    else if ((code&0xf0)==0xe0 && s[0] && s[1]) {
      code=(code&15)<<12;code|=(s[0]&63)<<6;code|=s[1]&63;s+=2;
    } else if ((code&0xf8)==0xf0 && s[0] && s[1] && s[2]) {
      code=(code&7)<<18;code|=(s[0]&63)<<12;code|=(s[1]&63)<<6;code|=s[2]&63;s+=3;
    }
    struct ComicGlyph *cached=&ac_comic_cache[(code*131u+(unsigned)px*17u)%2048];
    if(cached->px!=px || cached->code!=code) {
      if (FT_Load_Char(ac_comic_face,code,FT_LOAD_RENDER|FT_LOAD_TARGET_NORMAL)) continue;
      FT_GlyphSlot glyph=ac_comic_face->glyph;FT_Bitmap *bm=&glyph->bitmap;
      size_t bytes=(size_t)bm->width*bm->rows;
      unsigned char *coverage=bytes?malloc(bytes):NULL;
      if(bytes && !coverage)continue;
      for(unsigned row=0;row<bm->rows;row++)
        memcpy(coverage+row*bm->width,bm->buffer+row*bm->pitch,bm->width);
      free(cached->coverage);
      *cached=(struct ComicGlyph){code,px,(int)bm->width,(int)bm->rows,
        glyph->bitmap_left,glyph->bitmap_top,baseline-(int)round(y),
        (double)glyph->linearHoriAdvance/65536.0,coverage};
    }
    int left=(int)round(cursor)+cached->left, top=baseline-cached->top;
    for(int row=0;row<cached->height;row++) {
      int yy=top+row;if(yy<0 || yy>=fb->height)continue;
      for(int col=0;col<cached->width;col++) {
        int xx=left+col;if(xx<0 || xx>=fb->width)continue;
        unsigned a=cached->coverage[row*cached->width+col];if(!a)continue;
        uint32_t *dst=fb->pixels+yy*fb->stride+xx, old=*dst;
        unsigned inv=255-a;
        *dst=0xff000000u|(((r*a+((old>>16)&255)*inv+127)/255)<<16)|
          (((g*a+((old>>8)&255)*inv+127)/255)<<8)|((b*a+(old&255)*inv+127)/255);
      }
    }
    cursor+=cached->advance;
  }
  return 1;
}
#else
static int ac_comic_draw(ACFramebuffer *fb,const char *t,double x,double y,double size,
    uint8_t r,uint8_t g,uint8_t b) { (void)fb;(void)t;(void)x;(void)y;(void)size;(void)r;(void)g;(void)b;return 0; }
#endif
#endif
