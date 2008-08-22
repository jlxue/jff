#ifndef SITE_H__
#define SITE_H__

#ifdef __cplusplus
extern "C" {
#endif

typedef struct site_s site_t;

site_t*
site_open(const char* file);

int
site_close(site_t site);



#ifdef __cplusplus
}
#endif

#endif /* SITE_H__ */

