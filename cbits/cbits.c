#include <string.h>

int
hs_text_containers_memcmp(const void *s1, const size_t s1ofs, const void *s2, const size_t s2ofs, const size_t n)
{
  return memcmp(s1+s1ofs, s2+s2ofs, n);
}
