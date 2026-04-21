// png_writer.c — Write an ARGB framebuffer to PNG using native macOS APIs.
//
// CoreGraphics expects planar row data with a bitmap info flag describing
// channel order. Our framebuffer is `uint32_t`s laid out as 0xAARRGGBB on
// little-endian, which on-disk reads as bytes BB,GG,RR,AA. The matching
// bitmap-info constant is
//     kCGImageAlphaPremultipliedFirst | kCGBitmapByteOrder32Little
// so CGImageCreate interprets the bytes correctly without us having to
// repack to straight RGBA.
#include "png_writer.h"

#include <CoreFoundation/CoreFoundation.h>
#include <CoreGraphics/CoreGraphics.h>
#include <ImageIO/ImageIO.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int png_write_argb(const char *out_path,
                   const uint32_t *argb_pixels,
                   int width, int height, int stride_pixels) {
    if (!out_path || !argb_pixels || width <= 0 || height <= 0) return 0;

    size_t row_bytes = (size_t)stride_pixels * 4;
    CGDataProviderRef provider = CGDataProviderCreateWithData(
        NULL, argb_pixels, row_bytes * (size_t)height, NULL);
    if (!provider) {
        fprintf(stderr, "[png] CGDataProviderCreateWithData failed\n");
        return 0;
    }

    CGColorSpaceRef cs = CGColorSpaceCreateDeviceRGB();
    CGImageRef img = CGImageCreate(
        (size_t)width, (size_t)height,
        /*bits per component*/ 8,
        /*bits per pixel   */ 32,
        row_bytes, cs,
        kCGImageAlphaPremultipliedFirst | kCGBitmapByteOrder32Little,
        provider, NULL, false, kCGRenderingIntentDefault);
    CGColorSpaceRelease(cs);
    CGDataProviderRelease(provider);
    if (!img) {
        fprintf(stderr, "[png] CGImageCreate failed\n");
        return 0;
    }

    CFStringRef path_cf = CFStringCreateWithCString(
        kCFAllocatorDefault, out_path, kCFStringEncodingUTF8);
    CFURLRef url = CFURLCreateWithFileSystemPath(
        kCFAllocatorDefault, path_cf, kCFURLPOSIXPathStyle, false);
    CFRelease(path_cf);
    if (!url) {
        fprintf(stderr, "[png] CFURLCreateWithFileSystemPath failed\n");
        CGImageRelease(img);
        return 0;
    }

    CGImageDestinationRef dst = CGImageDestinationCreateWithURL(
        url, CFSTR("public.png"), 1, NULL);
    CFRelease(url);
    if (!dst) {
        fprintf(stderr, "[png] CGImageDestinationCreateWithURL failed\n");
        CGImageRelease(img);
        return 0;
    }

    CGImageDestinationAddImage(dst, img, NULL);
    bool ok = CGImageDestinationFinalize(dst);
    CFRelease(dst);
    CGImageRelease(img);
    if (!ok) {
        fprintf(stderr, "[png] CGImageDestinationFinalize failed\n");
        return 0;
    }
    return 1;
}
