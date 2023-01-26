#ifndef PDTVTPAPER_EPDGL_H
#define PDTVTPAPER_EPDGL_H

#include <Arduino.h>
#include <vector>
#include <stack>
#include "gfxfont.h"

#define EPDGL_DEBUG Serial.printf

namespace epdgl {

struct Coord {
    int16_t x, y;
    Coord(int16_t x, int16_t y) : x(x), y(y) {};
    Coord operator+(const Coord &r) const { return Coord(x + r.x, y + r.y); }
    Coord &operator+=(const Coord &r) {
        x += r.x;
        y += r.y;
        return *this;
    }
    Coord operator-() const { return Coord(-x, -y); }
    Coord operator-(const Coord &r) const { return Coord(x - r.x, y - r.y); }
    Coord &operator-=(const Coord &r) {
        x -= r.x;
        y -= r.y;
        return *this;
    }
    Coord operator*(const int8_t r) const { return Coord(x * r, y * r); }
    Coord &operator*=(const int8_t r) {
        x *= r;
        y *= r;
        return *this;
    }
    Coord operator/(const int8_t r) const { return Coord(x / r, y / r); }
    Coord &operator/=(const int8_t r) {
        x /= r;
        y /= r;
        return *this;
    }
    Coord operator/(const float r) const { return Coord((x / r) + .5f, (y / r) + .5f); }
    Coord &operator/=(const float r) {
        x = ((x / r) + .5f);
        y = ((y / r) + .5f);
        return *this;
    }
};

struct Rect {
    int16_t x, y, w, h;
    Rect(int16_t x, int16_t y, int16_t w, int16_t h) : x(x), y(y), w(w), h(h) {}
    Rect(const Coord &c, int16_t w, int16_t h) : x(c.x), y(c.y), w(w), h(h) {}
    Rect(const Coord &tl, const Coord &br) : x(tl.x), y(tl.y), w(br.x - tl.x + 1), h(br.y - tl.y + 1) {}
    Coord c0() const { return Coord(x, y); }
    Coord c1() const { return Coord(x + w - 1, y + h - 1); }
    // get a point in the rect, {0,0} means top left, {1,1} means bottom right
    Coord operator*(std::pair<float, float> scale) const { return Coord(x + (w - 1) * scale.first, y + (h - 1) * scale.second); }
    // get a shifted rect
    Rect operator+(const Coord &r) const { return Rect(x + r.x, y + r.y, w, h); } // operator- omitted
    // shift the rect
    Rect &operator+=(const Coord &r) {
        x += r.x;
        y += r.y;
        return *this;
    }
    // get a sub rect, ({.5, 0},{1,1}) would result in the right half of the rect
    Rect sub(std::pair<float, float> c0, std::pair<float, float> c1) const { return Rect((*this) * c0, (*this) * c1); }
    Rect expand(int16_t ph, int16_t pv) const { return Rect(x - ph, y - pv, w + 2 * ph, h + 2 * pv); } // idk what is this for
    void include(const Coord &c) {
        if (c.x < x) x = c.x;
        if (c.y < y) y = c.y;
        if (c.x > x + w - 1) w = c.x - x + 1;
        if (c.y > y + h - 1) h = c.y - y + 1;
    }
    void include(const Rect &r) {
        include(r.c0());
        include(r.c1());
    }
};

class ColorSpace {
public:
    const uint8_t bpp;
    virtual uint32_t convert(uint16_t rgb565) const { return ((rgb565 << 16) | 0xFF); }
    virtual void blend(const uint32_t &src, uint32_t &dst) { dst = src; }
protected:
    ColorSpace(uint8_t bpp = 0) : bpp(bpp) {}
};

// example
class OneBit : public ColorSpace {
public:
    OneBit() : ColorSpace(1) {}
    uint32_t convert(uint16_t rgb555) const override { return (rgb555 ? 0xFFFFFFFF : 0x000000FF); }
};

class Greyscale : public ColorSpace {
public:
    Greyscale() : ColorSpace(8) {}
    uint32_t convert(uint16_t rgb555) const override {
        float r = float(((rgb555 & 0b1111100000000000) >> 8) + ((rgb555 & 0b1111100000000000) >> 13)) / 256.f;
        float g = float(((rgb555 & 0b0000011111100000) >> 3) + ((rgb555 & 0b0000011111100000) >> 9)) / 256.f;
        float b = float(((rgb555 & 0b0000000000011111) << 3) + ((rgb555 & 0b0000000000011111) >> 2)) / 256.f;
        return (std::min(uint32_t((0.2126f * r + 0.7152f * g + 0.0722f * b) * 256.f), uint32_t(0xFF)) << 24) | 0xFF; // opaque
    }
};

class Bitmap;

class Shape {
public:
    enum Type {
        SHAPE_RECTANGLE, SHAPE_CIRCLE
    } type;
    Rect rect;
    Bitmap* fill;
    Shape* parent = nullptr; // null: top level shape, right below Object
    uint8_t changed = false; // only set by parent and it's object parent
    std::vector<Shape> children; // left public for PROS :) (since this is a vector, indices shouldn't change. this means you can save index and random access shapes for edit)
    Shape(Type type, int16_t x, int16_t y, int16_t w, int16_t h, Bitmap* fill);
    Shape(Type type, const Rect &rect, Bitmap* fill);
    ~Shape() { setFill(nullptr); }
    void setFill(Bitmap* bitmap);
    uint32_t getPixel(uint32_t x, uint32_t y, ColorSpace colorSpace, uint32_t baseColor) const;
    void setPixel(uint32_t x, uint32_t y, uint32_t color); // if Bitmap was SolidColor, a new bitmap will be allocated, filled, then setPixel (probably slow, be careful)
    void setColor(uint32_t color); // will remove existing bitmap if no more references. keep a copy (of shape with that bitmap set as fill) if needed

    bool intersect(int16_t a, int16_t b) const {
        if (type == SHAPE_RECTANGLE)
            return ((a >= rect.x) && (a <= rect.x + rect.w - 1) && (b >= rect.y) && (b <= rect.y + rect.h - 1));
        else // TODO: if new shape types are added...
            return ((float(a) - rect.x) * (float(a) - rect.x) + (float(b) - rect.y) * (float(b) - rect.y)) <= (float(rect.w) / 2.f) * (float(rect.w) / 2.f);
    }
    void reportChanged() { if (parent) parent->reportChanged(); else changed = true; }
    uint16_t pushShape(const Shape &shape) { // apparently even iterators get invalidated, index the way to go
        children.push_back(shape);
        return children.size() - 1;
    }
    void popShape() { children.pop_back(); }
    void clearShapes() { children.clear(); }
};

class Bitmap {
public:
    std::vector<Shape*> references;
    /// @param allocateSize 0: don't allocate, -1 = 0xFFFFFFFF: continuous memory, other values: split into blocks, allocateSize must be a multiple of 4 AND bpp!
    Bitmap(Shape* owner, uint16_t w, uint16_t h, uint8_t bpp, uint32_t allocateSize = 4096) : width(w), height(h), bpp(bpp), allocateSize(allocateSize) {
        references.push_back(owner); // at least one shape owner so bitmap lifespan gets managed by shapes, which don't use new and pointers
        owner->setFill(this);
        if (allocateSize == 0) return; // no alloc
        if (bpp == 0) bpp = 1;
        uint32_t bytes = (uint32_t(w) * h * bpp + 7) / 8;
        if (allocateSize == -1) {
            EPDGL_DEBUG("Allocating %lld bytes of continuous memory\n", bytes);
            bitmap = heap_caps_malloc(bytes, MALLOC_CAP_32BIT);
            if (!bitmap)
                EPDGL_DEBUG("Failed to allocate memory. Bitmap will show cross pattern\n");
            return;
        }
        if ((this->allocateSize % 4) || (this->allocateSize % bpp)) {
            this->allocateSize -= (this->allocateSize % (4 * bpp)); // technically we should mod by lcm(4, bpp), doesn't matter...
            EPDGL_DEBUG("Allocate size must be a multiple of 4 and bpp!\nUsing %ulld\n", this->allocateSize);
        }
        uint16_t fullBlocksCnt = bytes / this->allocateSize;
        uint16_t remainderBlockSize = bytes - (uint32_t(fullBlocksCnt) * this->allocateSize);
        EPDGL_DEBUG("Allocating %lld bytes of memory in %lld byte chunks\n", bytes, this->allocateSize);
        bitmap = heap_caps_malloc((fullBlocksCnt + (remainderBlockSize ? 1 : 0)) * sizeof(uint8_t*), MALLOC_CAP_32BIT); // allocate memory to store pointers to blocks
        if (!bitmap) {
            EPDGL_DEBUG("Failed to allocate memory. Bitmap will show cross pattern\n", bytes);
            return;
        }
        for (uint16_t b = 0; b < fullBlocksCnt; b++) {
            ((uint32_t**) bitmap)[b] = (uint32_t*) (heap_caps_malloc(this->allocateSize, MALLOC_CAP_32BIT));
            if (!((uint32_t**) bitmap)[b]) {
                EPDGL_DEBUG("Failed to allocate memory. Bitmap will show cross pattern\n", bytes);
                for (uint16_t fb = 0; fb < b; fb++)
                    heap_caps_free(((uint8_t**) bitmap)[fb]);
                heap_caps_free(bitmap);
                bitmap = nullptr;
                return;
            }
        }
        // allocate remainder block
        if (remainderBlockSize) {
            ((uint32_t**) bitmap)[fullBlocksCnt] = (uint32_t*) (heap_caps_malloc(remainderBlockSize, MALLOC_CAP_32BIT));
            if (!((uint32_t**) bitmap)[fullBlocksCnt]) {
                EPDGL_DEBUG("Failed to allocate memory. Bitmap will show cross pattern\n", bytes);
                for (uint16_t fb = 0; fb < fullBlocksCnt; fb++)
                    heap_caps_free(((uint32_t**) bitmap)[fb]);
                heap_caps_free(bitmap);
                bitmap = nullptr;
            }
        }
    }
    ~Bitmap() {
        if (!bitmap) return;
        if (allocateSize != 0 && allocateSize != -1) { // allocated and not continuous
            uint32_t blocks = ((uint32_t(width) * height * bpp + 7) / 8 + allocateSize - 1) / allocateSize;
            for (uint16_t b = 0; b < blocks; b++)
                heap_caps_free(((uint8_t**) bitmap)[b]);
        }
        heap_caps_free(bitmap); // both split and continuous
    }
    void remove() {
        for (auto &ref: references) ref->fill = nullptr;
        delete this;
    }
    void changed() { for (auto &ref: references) ref->reportChanged(); }

    // hi 3 byte is the pixel data aligned left. lo 1 byte is alpha, 0xFF if opaque (if bpp > 24 then all 4 bytes are pixel data)
    virtual uint32_t getPixel(uint32_t x, uint32_t y) {
        if (x >= width || y >= height) // param already takes unsigned
            return 0; // transparent
        if (!bitmap) return (((x + y) & 0b110) || ((x - y) & 0b110)) ? 0x000000FF : 0xFFFFFFFF; // white cross pattern on black (I hope)
        uint32_t bitIdx = (y * width + x) * bpp; // of the left most bit of the pixel
        return (((allocateSize == 0
                  ? ((uint32_t*) bitmap)[bitIdx >> 5]
                  : (((uint32_t**) bitmap)[(bitIdx >> 3) / allocateSize])[((bitIdx >> 3) % allocateSize) >> 2]) // won't cross blocks because size is a multiple of bpp and 4
                << (bitIdx & 0b11111)
                ) & pixelMask(bpp) // bpp 1s + 0s
               ) | 0xFF; // opaque
    }

    virtual void setPixel(uint32_t x, uint32_t y, uint32_t color) { // I don't care about the color format, I just take the first bpp bits
        if (x >= width || y >= height) // param already takes unsigned
            return;
        if (!bitmap) return;
        uint32_t bitIdx = (y * width + x) * bpp; // of the left most bit of the pixel
        uint32_t* dest = (allocateSize == 0
                          ? &(((uint32_t*) bitmap)[bitIdx >> 5])
                          : &((((uint32_t**) bitmap)[(bitIdx >> 3) / allocateSize])[((bitIdx >> 3) % allocateSize) >> 2]));
        *dest &= ~(pixelMask(bpp) >> (bitIdx & 0b11111)); // rshift 1s + bpp 0s + 1s
        *dest |= (color & pixelMask(bpp)) >> (bitIdx & 0b11111);
        changed();
    }

    /// BITMAP DRAWING FUNCTIONS (mainly from Adafruit_GFX) (no alpha, just bpp bits of whatever data)
    __attribute__((always_inline)) void drawPixel(int16_t x, int16_t y, uint32_t color) { setPixel(x, y, color); }
    void fillRect(int16_t x, int16_t y, int16_t w, int16_t h, uint32_t color) {
        for (int16_t xx = x; xx < x + w; xx++)
            for (int16_t yy = y; yy < y + h; yy++)
                drawPixel(xx, yy, color);
    }
    void fillAll(uint32_t color) { fillRect(0, 0, width, height, color); } // is this a good name?
    __attribute__((always_inline)) void drawLine(int16_t x0, int16_t y0, int16_t x1, int16_t y1, uint32_t color) {
        int16_t steep = abs(y1 - y0) > abs(x1 - x0), temp;
        if (steep) {
            temp = x0;
            x0 = y0;
            y0 = temp;
            temp = x1;
            x1 = y1;
            y1 = temp;
        }
        if (x0 > x1) {
            temp = x0;
            x0 = x1;
            x1 = temp;
            temp = y0;
            y0 = y1;
            y1 = temp;
        }
        int16_t dx, dy;
        dx = x1 - x0;
        dy = abs(y1 - y0);
        int16_t err = dx / 2;
        int16_t ystep;
        if (y0 < y1) ystep = 1;
        else ystep = -1;
        for (; x0 <= x1; x0++) {
            if (steep) drawPixel(y0, x0, color);
            else drawPixel(x0, y0, color);
            err -= dy;
            if (err < 0) {
                y0 += ystep;
                err += dx;
            }
        }
    };
    void drawRect(int16_t x, int16_t y, int16_t w, int16_t h, uint32_t color) {
        for (int16_t xx = x; xx < x + w; xx++) {
            drawPixel(xx, y, color);
            drawPixel(xx, y + h - 1, color);
        }
        for (int16_t yy = y + 1; yy < y + h - 1; yy++) {
            drawPixel(x, yy, color);
            drawPixel(x + w - 1, yy, color);
        }
    }
    void drawCircle(int16_t x0, int16_t y0, int16_t r, uint32_t color) {
        int16_t f = 1 - r;
        int16_t ddF_x = 1;
        int16_t ddF_y = -2 * r;
        int16_t x = 0;
        int16_t y = r;
        drawPixel(x0, y0 + r, color);
        drawPixel(x0, y0 - r, color);
        drawPixel(x0 + r, y0, color);
        drawPixel(x0 - r, y0, color);
        while (x < y) {
            if (f >= 0) {
                y--;
                ddF_y += 2;
                f += ddF_y;
            }
            x++;
            ddF_x += 2;
            f += ddF_x;
            drawPixel(x0 + x, y0 + y, color);
            drawPixel(x0 - x, y0 + y, color);
            drawPixel(x0 + x, y0 - y, color);
            drawPixel(x0 - x, y0 - y, color);
            drawPixel(x0 + y, y0 + x, color);
            drawPixel(x0 - y, y0 + x, color);
            drawPixel(x0 + y, y0 - x, color);
            drawPixel(x0 - y, y0 - x, color);
        }
    }
    void fillCircle(int16_t x0, int16_t y0, int16_t r, uint32_t color) {
        for (uint16_t yy = y0 - r; yy < y0 + r + 1; yy++)
            drawPixel(x0, yy, color);
        fillCircleHelper(x0, y0, r, 3, 0, color);
    }
    void drawTriangle(int16_t x0, int16_t y0, int16_t x1, int16_t y1, int16_t x2, int16_t y2, uint32_t color) {
        drawLine(x0, y0, x1, y1, color);
        drawLine(x1, y1, x2, y2, color);
        drawLine(x2, y2, x0, y0, color);
    }
    void fillTriangle(int16_t x0, int16_t y0, int16_t x1, int16_t y1, int16_t x2, int16_t y2, uint32_t color) {
        int16_t a, b, y, last, temp;
        if (y0 > y1) {
            temp = y0;
            y0 = y1;
            y1 = temp;
            temp = x0;
            x0 = x1;
            x1 = temp;
        }
        if (y1 > y2) {
            temp = y2;
            y2 = y1;
            y1 = temp;
            temp = x2;
            x2 = x1;
            temp = x1;
        }
        if (y0 > y1) {
            temp = y0;
            y0 = y1;
            y1 = temp;
            temp = x0;
            x0 = x1;
            x1 = temp;
        }
        if (y0 == y2) {
            a = b = x0;
            if (x1 < a)
                a = x1;
            else if (x1 > b)
                b = x1;
            if (x2 < a)
                a = x2;
            else if (x2 > b)
                b = x2;
            for (uint16_t xx = a; xx < b + 1; xx++)
                drawPixel(xx, y0, color);
            return;
        }
        int16_t dx01 = x1 - x0, dy01 = y1 - y0, dx02 = x2 - x0, dy02 = y2 - y0, dx12 = x2 - x1, dy12 = y2 - y1;
        int32_t sa = 0, sb = 0;
        if (y1 == y2) last = y1;
        else last = y1 - 1;
        for (y = y0; y <= last; y++) {
            a = x0 + sa / dy01;
            b = x0 + sb / dy02;
            sa += dx01;
            sb += dx02;
            if (a > b) {
                temp = a;
                a = b;
                b = temp;
            }
            for (uint16_t xx = a; xx < b + 1; xx++)
                drawPixel(xx, y, color);
        }
        sa = (int32_t) dx12 * (y - y1);
        sb = (int32_t) dx02 * (y - y0);
        for (; y <= y2; y++) {
            a = x1 + sa / dy12;
            b = x0 + sb / dy02;
            sa += dx12;
            sb += dx02;
            if (a > b) {
                temp = a;
                a = b;
                b = temp;
            }
            for (uint16_t xx = a; xx < b + 1; xx++)
                drawPixel(xx, y, color);
        }
    }
    void drawRoundRect(int16_t x, int16_t y, int16_t w, int16_t h, int16_t r, uint32_t color) {
        int16_t max_radius = ((w < h) ? w : h) / 2; // 1/2 minor axis
        if (r > max_radius)
            r = max_radius;
        for (int16_t xx = x + r; xx < x + w - r; xx++) {
            drawPixel(xx, y, color);
            drawPixel(xx, y + h - 1, color);
        }
        for (int16_t yy = y + r; yy < y + h - r; yy++) {
            drawPixel(x, yy, color);
            drawPixel(x + w - 1, yy, color);
        }
        drawCircleHelper(x + r, y + r, r, 1, color);
        drawCircleHelper(x + w - r - 1, r + r, r, 2, color);
        drawCircleHelper(x + w - r - 1, r + h - r - 1, r, 4, color);
        drawCircleHelper(x + r, y + h - r - 1, r, 8, color);
    }
    void fillRoundRect(int16_t x, int16_t y, int16_t w, int16_t h, int16_t r, uint32_t color) {
        int16_t max_radius = ((w < h) ? w : h) / 2;
        if (r > max_radius)
            r = max_radius;
        fillRect(x + r, y, w - 2 * r, h, color);
        fillCircleHelper(x + w - r - 1, y + r, r, 1, h - 2 * r - 1, color);
        fillCircleHelper(x + r, y + r, r, 2, h - 2 * r - 1, color);
    }
    /// draw bitmap functions
    void setCursor(int16_t x, int16_t y) {
        cursor_x = x;
        cursor_y = y;
    }
    void drawChar(int16_t x, int16_t y, unsigned char c, uint32_t color) {
        if (!gfxFont) return;
        c -= (uint8_t) pgm_read_byte(&gfxFont->first);
        GFXglyph* glyph = gfxFont->glyph + c;
        uint8_t* bm = gfxFont->bitmap;
        uint16_t bo = pgm_read_word(&glyph->bitmapOffset);
        uint8_t w = pgm_read_byte(&glyph->width), h = pgm_read_byte(&glyph->height);
        int8_t xo = pgm_read_byte(&glyph->xOffset), yo = pgm_read_byte(&glyph->yOffset);
        uint8_t xx, yy, bits = 0, bit = 0;
        for (yy = 0; yy < h; yy++) {
            for (xx = 0; xx < w; xx++) {
                if (!(bit++ & 7))
                    bits = pgm_read_byte(&bm[bo++]);
                if (bits & 0x80)
                    drawPixel(x + xo + xx, y + yo + yy, color);
                bits <<= 1;
            }
        }
    }
    void write(uint8_t c, uint32_t color) {
        if (c == '\n') {
            cursor_x = 0;
            cursor_y += (uint8_t) pgm_read_byte(&gfxFont->yAdvance);
        }
        else if (c != '\r') {
            uint8_t first = pgm_read_byte(&gfxFont->first);
            if ((c >= first) && (c <= (uint8_t) pgm_read_byte(&gfxFont->last))) {
                GFXglyph* glyph = gfxFont->glyph + c - first;
                uint8_t w = pgm_read_byte(&glyph->width),
                        h = pgm_read_byte(&glyph->height);
                if ((w > 0) && (h > 0)) { // Is there an associated bitmap?
                    int16_t xo = (int8_t) pgm_read_byte(&glyph->xOffset); // sic
                    drawChar(cursor_x, cursor_y, c, color);
                }
                cursor_x += (uint8_t) pgm_read_byte(&glyph->xAdvance);
            }
        }
    }
    void charBounds(unsigned char c, int16_t* x, int16_t* y, int16_t* minx, int16_t* miny, int16_t* maxx, int16_t* maxy) {
        if (!gfxFont) return;
        if (c == '\n') {
            *x = 0;
            *y += (uint8_t) pgm_read_byte(&gfxFont->yAdvance);
        }
        else if (c != '\r') {
            uint8_t first = pgm_read_byte(&gfxFont->first),
                    last = pgm_read_byte(&gfxFont->last);
            if ((c >= first) && (c <= last)) {
                GFXglyph* glyph = gfxFont->glyph + c - first;
                uint8_t gw = pgm_read_byte(&glyph->width),
                        gh = pgm_read_byte(&glyph->height),
                        xa = pgm_read_byte(&glyph->xAdvance);
                int8_t xo = pgm_read_byte(&glyph->xOffset),
                        yo = pgm_read_byte(&glyph->yOffset);
                int16_t x1 = *x + xo, y1 = *y + yo, x2 = x1 + gw - 1, y2 = y1 + gh - 1;
                if (x1 < *minx)
                    *minx = x1;
                if (y1 < *miny)
                    *miny = y1;
                if (x2 > *maxx)
                    *maxx = x2;
                if (y2 > *maxy)
                    *maxy = y2;
                *x += xa;
            }
        }

    }
    void getTextBounds(const char* str, int16_t x, int16_t y, int16_t* x1, int16_t* y1, uint16_t* w, uint16_t* h) {
        uint8_t c;
        int16_t minx = 0x7FFF, miny = 0x7FFF, maxx = -1, maxy = -1;
        *x1 = x;
        *y1 = y;
        *w = *h = 0;
        while ((c = *str++))
            charBounds(c, &x, &y, &minx, &miny, &maxx, &maxy);
        if (maxx >= minx) {
            *x1 = minx;
            *w = maxx - minx + 1;
        }
        if (maxy >= miny) {
            *y1 = miny;
            *h = maxy - miny + 1;
        }
    }
private:
    __attribute__((always_inline)) void drawCircleHelper(int16_t x0, int16_t y0, int16_t r, uint8_t cornername, uint32_t color) {
        int16_t f = 1 - r;
        int16_t ddF_x = 1;
        int16_t ddF_y = -2 * r;
        int16_t x = 0;
        int16_t y = r;
        while (x < y) {
            if (f >= 0) {
                y--;
                ddF_y += 2;
                f += ddF_y;
            }
            x++;
            ddF_x += 2;
            f += ddF_x;
            if (cornername & 0x4) {
                drawPixel(x0 + x, y0 + y, color);
                drawPixel(x0 + y, y0 + x, color);
            }
            if (cornername & 0x2) {
                drawPixel(x0 + x, y0 - y, color);
                drawPixel(x0 + y, y0 - x, color);
            }
            if (cornername & 0x8) {
                drawPixel(x0 - y, y0 + x, color);
                drawPixel(x0 - x, y0 + y, color);
            }
            if (cornername & 0x1) {
                drawPixel(x0 - y, y0 - x, color);
                drawPixel(x0 - x, y0 - y, color);
            }
        }
    }
    __attribute__((always_inline)) void fillCircleHelper(int16_t x0, int16_t y0, int16_t r, uint8_t cornername, int16_t delta, uint32_t color) {
        int16_t f = 1 - r;
        int16_t ddF_x = 1;
        int16_t ddF_y = -2 * r;
        int16_t x = 0;
        int16_t y = r;
        int16_t px = x;
        int16_t py = y;
        delta++; // Avoid some +1's in the loop
        while (x < y) {
            if (f >= 0) {
                y--;
                ddF_y += 2;
                f += ddF_y;
            }
            x++;
            ddF_x += 2;
            f += ddF_x;
            if (x < (y + 1)) {
                if (cornername & 1)
                    for (uint16_t yy = y0 - y; yy < y0 + y + delta; yy++)
                        drawPixel(x0 + x, yy, color);
                if (cornername & 2)
                    for (uint16_t yy = y0 - y; yy < y0 + y + delta; yy++)
                        drawPixel(x0 - x, yy, color);
            }
            if (y != py) {
                if (cornername & 1)
                    for (uint16_t yy = y0 - px; yy < y0 + px + delta; yy++)
                        drawPixel(x0 + py, yy, color);
                if (cornername & 2)
                    for (uint16_t yy = y0 - px; yy < y0 + px + delta; yy++)
                        drawPixel(x0 - py, yy, color);
                py = y;
            }
            px = x;
        }
    }
    /// END OF BITMAP DRAWING FUNCTIONS

public:
    uint16_t width; // plz don't change it if bitmap allocated
    uint16_t height;
    uint8_t bpp;
    void* bitmap = nullptr;
private:
    uint32_t allocateSize;
    constexpr uint32_t pixelMask(uint8_t bits) const { return ~((1 << (32 - bits)) - 1); }
    GFXfont* gfxFont = nullptr;
    int16_t cursor_x;
    int16_t cursor_y;
};

class SolidColor : public Bitmap {
public:
    uint32_t color; // used for SolidColor
    SolidColor(Shape* owner, uint16_t w, uint16_t h, uint32_t color) : Bitmap(owner, w, h, 0) { this->color = color; }
    // no "setColor" here, if we want to change color, just allocate a new SolidColor and switch over
    uint32_t getPixel(uint32_t x, uint32_t y) override { return (x >= Bitmap::width || y >= Bitmap::height) ? 0 : color; }
    void setPixel(uint32_t x, uint32_t y, uint32_t color) override {}; // SolidColor to Bitmap conversion is done by Shape (involves changing references)(this shouldn't be called)
};

Shape::Shape(Type type, int16_t x, int16_t y, int16_t w, int16_t h, Bitmap* fill) : type(type), rect(x, y, w, h), fill(fill) {
    if (fill) fill->references.push_back(this); // add reference
}
Shape::Shape(Type type, const Rect &rect, Bitmap* fill) : type(type), rect(rect), fill(fill) {
    if (fill) fill->references.push_back(this); // add reference
}
void Shape::setFill(Bitmap* bitmap) {
    if (this->fill == bitmap) return;
    if (this->fill) { // remove reference
        if (this->fill->references.erase(std::find(this->fill->references.begin(), this->fill->references.end(), this)) == this->fill->references.end())
            EPDGL_DEBUG("Shape bitmap reference not found\n");
        if (this->fill->references.empty()) // no more references
            delete this->fill;
    }
    this->fill = bitmap;
    if (this->fill) this->fill->references.push_back(this); // add reference
}
uint32_t Shape::getPixel(uint32_t x, uint32_t y, ColorSpace colorSpace, uint32_t baseColor) const { /// TODO: this seems wrong...
    if (fill)
        colorSpace.blend(fill->getPixel(x, y), baseColor);
    for (auto &shape: children)
        colorSpace.blend(shape.getPixel(x, y, colorSpace, baseColor), baseColor);
    return baseColor;
}
void Shape::setPixel(uint32_t x, uint32_t y, uint32_t color) {
    return fill->setPixel(x, y, color);
    // here used to have "if setPixel on SolidColor, switch to bitmap, fillAll and then setPixel", but it made everything not elegant. also setPixel sets the base
    // bitmap disregarding the children shapes is not elegant enough that I don't even care anymore
}
void Shape::setColor(uint32_t color) {
    new SolidColor(this, rect.w, rect.h, color); // to avoid casting to SolidColor (scary!) let's just allocate a new one and switch over
    reportChanged();
}

class Object {
public:
    // use new to create objects
    Object(Object* parent) : parent(parent) { if (parent) parent->addChild(*this); }
    virtual ~Object() {
        for (auto &child: children)
            delete child;
    }
    void remove() {
        if (parent) parent->removeChild(*this);
        delete this; // delete children without parent->removeChild
    }
    Object* parent = nullptr;
    bool redrawRequested = true;
    void addChild(Object &child) { children.push_back(&child); }
    bool removeChild(Object &child) { return children.erase(std::find(children.begin(), children.end(), &child)) != children.end(); }
    virtual void preDraw() {};
    virtual void postDraw() {};
    virtual void processChild(Object* child) {};
    virtual void processDrawnChild(Object* child) {};

    void draw() {
        preDraw();
        for (auto &child: children) {
            processChild(child);
            if (child->redrawRequested) {
                child->draw();
                processDrawnChild(child);
            }
        }
        postDraw();
        redrawRequested = false;
    }

    void requestRedraw() { /// TODO: request rect
        redrawRequested = true;
        if (parent) parent->requestRedraw();
    }
private:
    std::vector<Object*> children;
};

// A single header graphics library that does a lot
class EpdGL {
};
}

#endif //PDTVTPAPER_EPDGL_H
