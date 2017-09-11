/*
 * Copyright (C) 2002-2017 ProcessOne, SARL. All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <erl_interface.h>
#include <ei.h>
#include <unistd.h>
#include <arpa/inet.h>
#include "gd.h"
#include "jpeglib.h"
#include "png.h"
#include "webp/decode.h"

/* Maximum resolution is 25Mpx */
#define MAX_RESOLUTION 25000000
#define PNG 'p'
#define JPEG 'j'
#define WEBP 'w'

int is_known(char format)
{
  return format == PNG || format == JPEG || format == WEBP;
}

/*
  The following unbelievable crap is needed to support
  retarded errors processing mechanism in libjpeg
*/
struct eimp_jpeg_error_mgr {
  struct jpeg_error_mgr pub;
  jmp_buf setjmp_buffer;
};

typedef struct eimp_jpeg_error_mgr * eimp_jpeg_error_ptr;

void eimp_jpeg_error_exit(j_common_ptr cinfo)
{
  eimp_jpeg_error_ptr myerr = (eimp_jpeg_error_ptr) cinfo->err;
  longjmp(myerr->setjmp_buffer, 1);
}

int check_jpeg_header(uint8_t *buf, size_t size)
{
  struct jpeg_decompress_struct cinfo;
  struct eimp_jpeg_error_mgr jerr;
  int ret = 0;

  cinfo.err = jpeg_std_error(&jerr.pub);
  jerr.pub.error_exit = eimp_jpeg_error_exit;
  if (setjmp(jerr.setjmp_buffer)) {
    jpeg_destroy_decompress(&cinfo);
    return 0;
  }

  jpeg_create_decompress(&cinfo);
  jpeg_mem_src(&cinfo, buf, size);
  if (jpeg_read_header(&cinfo, TRUE) == JPEG_HEADER_OK) {
    size_t width = cinfo.image_width;
    size_t height = cinfo.image_height;
    if (width * height < MAX_RESOLUTION)
      ret = 1;
  };
  jpeg_destroy_decompress(&cinfo);
  return ret;
}
/* End of retardation */

int check_webp_header(uint8_t *buf, size_t size)
{
  int width, height;
  if (WebPGetInfo(buf, size, &width, &height)) {
    if (width * height < MAX_RESOLUTION)
      return 1;
  }
  return 0;
}

int check_png_header(uint8_t *buf, size_t size)
{
  uint32_t width, height;
  int ret = 0;

  /* libpng's API is as shitty as libjpeg's one,
     so we just check 8-byte header using using libpng's
     png_sig_cmp() and then decode width and height manually
     from the very first mandatory chunk (IHDR), which is
     always placed in the same place and is always of
     the same size: even if someone wants to fool us,
     gdImageCreateFromPngPtr() will fail later */
  if (size > 24) {
    if (!png_sig_cmp(buf, 0, 8)) {
      memcpy(&width, buf+16, 4);
      memcpy(&height, buf+20, 4);
      if (ntohl(width) * ntohl(height) < MAX_RESOLUTION)
	ret = 1;
    }
  }
  return ret;
}

gdImagePtr decode(uint8_t format, uint8_t *buf, size_t size)
{
  switch (format) {
  case WEBP:
    if (check_webp_header(buf, size))
      return gdImageCreateFromWebpPtr(size, buf);
    else
      return NULL;
  case PNG:
    if (check_png_header(buf, size))
      return gdImageCreateFromPngPtr(size, buf);
    else
      return NULL;
  case JPEG:
    if (check_jpeg_header(buf, size))
      return gdImageCreateFromJpegPtr(size, buf);
    else
      return NULL;
  default:
    return NULL;
  }
}

void *encode(uint8_t format, gdImagePtr im, int *size)
{
  switch (format) {
  case WEBP:
    return gdImageWebpPtr(im, size);
  case PNG:
    return gdImagePngPtr(im, size);
  case JPEG:
    return gdImageJpegPtr(im, size, -1);
  default:
    return NULL;
  }
}

static int read_buf(void *data, int len)
{
  int i, got = 0;
  uint8_t *buf = data;
  do {
    if ((i = read(STDIN_FILENO, buf+got, len-got)) <= 0) {
      if (i == 0) return got;
      if (errno != EINTR)
	return got;
      i = 0;
    }
    got += i;
  } while (got < len);
  return (len);
}

static int write_buf(void *data, int len)
{
  int i, done = 0;
  uint8_t *buf = data;
  do {
    if ((i = write(STDOUT_FILENO, buf+done, len-done)) < 0) {
      if (errno != EINTR)
	return (i);
      i = 0;
    }
    done += i;
  } while (done < len);
  return (len);
}

int write_data(uint8_t *pid, uint8_t *buf, size_t size)
{
  size_t pid_size = pid[0] + 1;
  uint32_t tag = htonl(pid_size+size+1);
  if (write_buf(&tag, 4) == 4) {
    if (write_buf(pid, pid_size) == pid_size) {
      if (write_buf("0", 1) == 1) {
	if (write_buf(buf, size) == size) {
	  return 1;
	}
      }
    }
  }
  return 0;
}

int write_error(uint8_t *pid, char *s)
{
  size_t size = strlen(s);
  size_t pid_size = pid[0] + 1;
  uint32_t tag = htonl(pid_size+size+1);
  if (write_buf(&tag, 4) == 4) {
    if (write_buf(pid, pid_size) == pid_size) {
      if (write_buf("1", 1) == 1) {
	if (write_buf(s, size) == size) {
	  return 1;
	}
      }
    }
  }
  return 0;
}

int convert(uint8_t *pid, uint8_t from, uint8_t to, uint8_t *ibuf, size_t isize)
{
  gdImagePtr im;
  int osize, write_result;

  if (is_known(from) && is_known(to)) {
    im = decode(from, ibuf, isize);
    if (!im)
      return write_error(pid, "decode_failure");
    
    uint8_t *obuf = encode(to, im, &osize);
    gdImageDestroy(im);
    if (!obuf)
      return write_error(pid, "encode_failure");

    write_result = write_data(pid, obuf, osize);
    gdFree(obuf);
    return write_result;
  } else {
    return write_error(pid, "unsupported_format");
  }
}

void loop(void)
{
  uint32_t tag;
  size_t size, pid_size;
  int result;

  do {
    result = 0;
    if (read_buf(&tag, 4) == 4) {
      size = ntohl(tag);
      if (size > 0) {
	uint8_t *buf = malloc(size);
	if (buf) {
	  if (read_buf(buf, size) == size) {
	    pid_size = buf[0];
	    if (size >= pid_size + 3) {
	      result = convert(buf, buf[pid_size + 1], buf[pid_size + 2],
			       buf+pid_size+3, size-pid_size-3);
	    }
	  }
	}
	free(buf);
      }
    }
  } while(result);
}

int main(int argc, char *argv[])
{
  erl_init(NULL, 0);
  loop();
  return 0;
}
