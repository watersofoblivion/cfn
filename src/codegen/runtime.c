/*
 * Reference Runtime Implementation in C
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>

#include <curl/curl.h>
#include <jansson.h>

// Processing loop
static int process;

// Garbage Collector
static void *base, *reset, *next, *end;
static size_t allocated;

typedef struct alloc {
  void *ptr;
  size_t size;
  struct alloc *next;
} alloc_t;

static alloc_t *allocs;

// CURL
static CURL *curl;
static CURLcode curl_res;
static char curl_err[CURL_ERROR_SIZE];

// Environment variable names
#define ENV_VAR_HANDLER ("_HANDLER")
#define ENV_VAR_AWS_REGION ("AWS_REGION")
#define ENV_VAR_AWS_EXECUTION_ENV ("AWS_EXECUTION_ENV")
#define ENV_VAR_AWS_LAMBDA_FUNCTION_NAME ("AWS_LAMBDA_FUNCTION_NAME")
#define ENV_VAR_AWS_LAMBDA_FUNCTION_MEMORY_SIZE ("AWS_LAMBDA_FUNCTION_MEMORY_SIZE")
#define ENV_VAR_AWS_LAMBDA_FUNCTION_VERSION ("AWS_LAMBDA_FUNCTION_VERSION")
#define ENV_VAR_AWS_LAMBDA_LOG_GROUP_NAME ("AWS_LAMBDA_LOG_GROUP_NAME")
#define ENV_VAR_AWS_LAMBDA_LOG_STREAM_NAME ("AWS_LAMBDA_LOG_STREAM_NAME")
#define ENV_VAR_AWS_ACCESS_KEY_ID ("AWS_ACCESS_KEY_ID")
#define ENV_VAR_AWS_SECRET_ACCESS_KEY ("AWS_SECRET_ACCESS_KEY")
#define ENV_VAR_AWS_SESSION_TOKEN ("AWS_SESSION_TOKEN")
#define ENV_VAR_AWS_LAMBDA_RUNTIME_API ("AWS_LAMBDA_RUNTIME_API")
#define ENV_VAR_LAMBDA_TASK_ROOT ("LAMBDA_TASK_ROOT")
#define ENV_VAR_LAMBDA_RUNTIME_DIR ("LAMBDA_RUNTIME_DIR")
#define ENV_VAR_TZ ("TZ")

// Environment values
static const char *_handler;
static const char *aws_region;
static const char *aws_execution_env;
static const char *aws_lambda_function_name, *aws_lambda_function_memory_size, *aws_lambda_function_version;
static const char *aws_lambda_log_group_name, *aws_lambda_log_stream_name;
static const char *aws_access_key_id, *aws_secret_access_key, *aws_session_token;
static const char *aws_lambda_runtime_api;
static const char *lambda_task_root, *lambda_runtime_dir;
static const char *tz;

// Base URL
static char *base_url;
static size_t base_url_len;

// GC allocation functions
static void *cfnpp_malloc(size_t);
static void *cfnpp_malloc_with_realloc(size_t);
static void *cfnpp_calloc_with_realloc(size_t, size_t);
static void *cfnpp_realloc(void *, size_t);
static char *cfnpp_strdup(const char *);
static void cfnpp_free(void *);
static int cfnpp_gc(void);

// Initialization functions
int cfnpp_init(void);
static inline int cfnpp_init_env(void);
static inline const char *cfnpp_init_getenv(const char *, const char *);
static inline int cfnpp_init_gc(void);
static inline int cfnpp_init_base_url(void);
static inline int cfnpp_init_curl(void);
static inline int cfnpp_init_jansson(void);
static inline int cfnpp_init_shutdown_hook(void);
static inline int cfnpp_init_perm_gen(void);
int cfnpp_close_perm_gen(void);
int cfnpp_init_main_gen(void);
static inline int cfnpp_init_error(const char *msg, const char *type);

// URL helpers
static inline char *cfnpp_build_path(char *prefix, char *id, char *suffix);
static inline char *cfnpp_next_invocation_path(void);
static inline char *cfnpp_init_error_path(void);
static inline char *cfnpp_invocation_response_path(char *);
static inline char *cfnpp_invocation_error_path(char *);

static inline char *cfnpp_relative_url(char *path);
static inline char *cfnpp_next_invocation_url(void);
static inline char *cfnpp_init_error_url(void);
static inline char *cfnpp_invocation_response_url(char *);
static inline char *cfnpp_invocation_error_url(char *);

// Event processing functions
int cfnpp_next(json_t *req);
int cfnpp_success(json_t *resp);
int cfnpp_failure(const char *msg, const char *type);
int cfnpp_finalize(void);

// Signal handler
void cfnpp_stop(int);

// Shutdown functions
int cfnpp_shutdown(void);
static inline int cfnpp_shutdown_curl(void);
static inline int cfnpp_shutdown_gc(void);

/*************
 * Stub main *
 *************/

int main(void);
int main(void)
{
  int i;
  json_t *json;

  if (cfnpp_init()) {
    printf("Initialization error\n");

    if (cfnpp_init_error("Initialization error", "Unknown")) {
      printf("Error sending initialization failure\n");
    }

    return EXIT_FAILURE;
  }

  printf("(Initialize top-level vars, performing allocations)\n");
  cfnpp_malloc(10);
  cfnpp_malloc(20);
  cfnpp_malloc(30);

  if (cfnpp_close_perm_gen()) {
    printf("Error closing perm gen\n");

    if (cfnpp_init_error("Could not close perm gen", "GC")) {
      printf("Error sending gc failure\n");
    }

    return EXIT_FAILURE;
  }

  printf("(Perform stop-copy pass over top-level vars, performing allocations and compactions)\n");
  cfnpp_malloc(5);
  cfnpp_malloc(10);
  cfnpp_malloc(15);

  if (cfnpp_init_main_gen()) {
    printf("Error initializing main gen\n");

    if (cfnpp_init_error("Could not initialize main gen", "GC")) {
      printf("Error sending gc failure\n");
    }

    return EXIT_FAILURE;
  }

  while (process) {
    if (cfnpp_next(json)) {
      printf("Error fetching next event\n");
      break;
    }

    if (i % 2 == 0) {
      if (cfnpp_success(json)) {
        printf("Error sending success response\n");
        break;
      }
    } else {
      if (cfnpp_failure("Error message", "Error type")) {
        printf("Error sending failure response\n");
        break;
      }
    }

    if (cfnpp_finalize()) {
      printf("Error finalizing event\n");
      break;
    }
  }

  if (cfnpp_shutdown()) {
    printf("Shutdown error\n");
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}

/*********************
 * Memory Management *
 *********************/

static void *cfnpp_malloc(size_t size)
{
  void *ptr = next;

  printf("Allocating %zu bytes of memory...", size);

  if (next + size > end) {
    printf("out of memory\n");
    return NULL;
  }
  printf("ok\n");

  next += size;
  allocated += size;
  printf("- Base, Reset, Next, End, Allocated: %p, %p, %p, %p, %zu\n", base, reset, next, end, allocated);

  printf("- Returning %p\n", ptr);
  return ptr;
}

static void *cfnpp_malloc_with_realloc(size_t size)
{
  void *ptr;
  size_t alloc_size = sizeof(alloc_t);

  printf("Allocating %zu bytes of re-allocable memory...\n", size);
  ptr = cfnpp_malloc(size);

  if (ptr) {
    printf("- Allocating %zu bytes of memory for alloc node...\n", alloc_size);
    alloc_t *alloc = cfnpp_malloc(alloc_size);
    if (!alloc) {
      printf("Could not allocate alloc node\n");
      return NULL;
    }

    printf("- Adding alloc node to head of linked list...");
    alloc->ptr = ptr;
    alloc->size = size;
    alloc->next = allocs;
    allocs = alloc;
    printf("ok\n");
  }

  printf("- Returning %p\n", ptr);
  return ptr;
}

static void *cfnpp_calloc_with_realloc(size_t nmemb, size_t size)
{
  void *ptr;
  size_t total_size = nmemb * size;

  printf("Allocating and clearing re-allocable memory for %zu members of %zu bytes each (%zu bytes total)...\n", nmemb, size, total_size);
  ptr = cfnpp_malloc_with_realloc(nmemb * size);

  if (ptr) {
    printf("- Clearing %zu bytes of memory...", total_size);
    ptr = memset(ptr, 0, nmemb * size);
    printf("ok\n");
  }

  printf("- Returning %p\n", ptr);
  return ptr;
}

static void *cfnpp_realloc(void *ptr, size_t newsize)
{
  alloc_t *alloc = allocs;
  void *newptr;

  printf("Re-allocating pointer %p to size %zu\n", ptr, newsize);

  printf("- Searching allocations list for pointer %p", ptr);
  while (alloc) {
    if (alloc->ptr == ptr) {
      printf("found\n");

      printf("  - Checking size if newsize (%zu) fits into current size (%zu)...", alloc->size, newsize);
      if (alloc->size <= newsize) {
        printf("yes\n");
        printf("- Returning %p\n", alloc->ptr);
        return alloc->ptr;
      }
      printf("no\n");

      printf("  - Allocating %zu bytes of memory for new space\n", newsize);
      newptr = cfnpp_malloc_with_realloc(newsize);
      if (!newptr) {
        return newptr;
      }
      printf("  - Copying %zu bytes from old location (%p) to new location (%p)\n", alloc->size, alloc->ptr, newptr);
      newptr = memcpy(newptr, alloc->ptr, alloc->size);

      printf("  - Resetting allocation to point to %p with size %zu\n", newptr, newsize);
      alloc->ptr = newptr;
      alloc->size = newsize;

      printf("- Returning %p\n", newptr);
      return newptr;
    } else {
      printf(".");
      alloc = alloc->next;
    }
  }

  printf("not found\n");
  return NULL;
}

static char *cfnpp_strdup(const char *str)
{
  size_t len = strlen(str);

  printf("Duplicating string \"%s\" of length %zu\n", str, len);
  void *ptr = cfnpp_malloc_with_realloc(len);
  if (!ptr) {
    return (char *)ptr;
  }

  printf("- Copying %zu bytes from old string (%p) to new (%p)\n", len, str, ptr);
  ptr = memcpy(ptr, str, len);
  return (char *)ptr;
}

static void cfnpp_free(void *ptr)
{
  printf("Freeing %p\n", ptr);
}

static int cfnpp_gc(void)
{
  size_t real_allocated = next - reset;

  printf("Performing full GC of %zu bytes\n", allocated);
  if (allocated != real_allocated) {
    printf("!!! GC BUG: cfnpp_malloc(): %zu bytes allocated, next - reset: %zu bytes allocated !!!\n", allocated, real_allocated);
    return EXIT_FAILURE;
  }

  printf("- Resetting next from %p to %p...", next, reset);
  next = reset;
  printf("ok\n");

  return EXIT_SUCCESS;
}

/******************
 * Initialization *
 ******************/

int cfnpp_init(void)
{
  printf("Initializing AWS Lambda Runtime\n");

  if (cfnpp_init_env()) {
    printf("Error initializing environment\n");
    return EXIT_FAILURE;
  }
  if (cfnpp_init_gc()) {
    printf("Error initializing heap\n");
    return EXIT_FAILURE;
  }
  if (cfnpp_init_base_url()) {
    printf("Error initializing base URL\n");
    return EXIT_FAILURE;
  }
  if (cfnpp_init_curl()) {
    printf("Error initializing CURL\n");
    return EXIT_FAILURE;
  }
  if (cfnpp_init_jansson()) {
    printf("Error initializing Jansson\n");
    return EXIT_FAILURE;
  }
  if (cfnpp_init_shutdown_hook()) {
    printf("Error initializing shutdown hook\n");
    return EXIT_FAILURE;
  }
  if (cfnpp_init_perm_gen()) {
    printf("Error initializing perm gen\n");
    return EXIT_FAILURE;
  }

  printf("AWS Lambda Runtime pre-initialized\n");
  return EXIT_SUCCESS;
}

static inline int cfnpp_init_env(void)
{
  printf("Initializing AWS Lambda Environment\n");

  _handler = cfnpp_init_getenv("Handler", ENV_VAR_HANDLER);

  aws_region = cfnpp_init_getenv("Region", ENV_VAR_AWS_REGION);

  aws_execution_env = cfnpp_init_getenv("AWS Execution Environment", ENV_VAR_AWS_EXECUTION_ENV);

  aws_lambda_function_name = cfnpp_init_getenv("AWS Lambda Function Name", ENV_VAR_AWS_LAMBDA_FUNCTION_NAME);
  aws_lambda_function_memory_size = cfnpp_init_getenv("AWS Lambda Function Memory in MB", ENV_VAR_AWS_LAMBDA_FUNCTION_MEMORY_SIZE);
  aws_lambda_function_version = cfnpp_init_getenv("AWS Lambda Function Version", ENV_VAR_AWS_LAMBDA_FUNCTION_VERSION);

  aws_lambda_log_group_name = cfnpp_init_getenv("AWS Lambda Log Group Name", ENV_VAR_AWS_LAMBDA_LOG_GROUP_NAME);
  aws_lambda_log_stream_name = cfnpp_init_getenv("AWS Lambda Log Stream Name", ENV_VAR_AWS_LAMBDA_LOG_STREAM_NAME);

  aws_access_key_id = cfnpp_init_getenv("AWS Access Key ID", ENV_VAR_AWS_ACCESS_KEY_ID);
  aws_secret_access_key = cfnpp_init_getenv("AWS Secret Access Key", ENV_VAR_AWS_SECRET_ACCESS_KEY);
  aws_session_token = cfnpp_init_getenv("AWS Session Token", ENV_VAR_AWS_SESSION_TOKEN);

  aws_lambda_runtime_api = cfnpp_init_getenv("AWS Lambda Runtime API", ENV_VAR_AWS_LAMBDA_RUNTIME_API);

  lambda_task_root = cfnpp_init_getenv("Lambda Task Root", ENV_VAR_LAMBDA_TASK_ROOT);
  lambda_runtime_dir = cfnpp_init_getenv("Lambda Runtime Directory", ENV_VAR_LAMBDA_RUNTIME_DIR);

  tz = cfnpp_init_getenv("Time Zone", ENV_VAR_TZ);

  printf("AWS Lambda Environment initialized\n");

  return EXIT_SUCCESS;
}

static inline const char *cfnpp_init_getenv(const char *name, const char *var)
{
  const char *value;

  printf("- %s (${%s})...", name, var);
  value = getenv(var);
  printf("\"%s\"\n", value);

  return value;
}

static inline int cfnpp_init_gc(void)
{
  int mb = atoi(aws_lambda_function_memory_size) - 1;
  size_t bytes = mb * 1024 * 1024;

  printf("Initializing Heap of %d MB (%zu bytes)...\n", mb, bytes);
  base = reset = next = malloc(bytes);
  if (!base) {
    perror("malloc");
    return EXIT_FAILURE;
  }
  end = base + bytes;

  printf("- Heap initialized\n");
  printf("- Base, Reset, Next, End, Allocated: %p, %p, %p, %p, %zu\n", base, reset, next, end, allocated);

  return EXIT_SUCCESS;
}

static inline int cfnpp_init_base_url(void)
{
  const char *prefix = "http://", *suffix = "/2018-06-01/";
  size_t prefix_len, runtime_api_len, suffix_len;

  printf("Creating base URL\n");

  prefix_len = strlen(prefix);
  runtime_api_len = strlen(aws_lambda_runtime_api);
  suffix_len = strlen(suffix);
  base_url_len = prefix_len + runtime_api_len + suffix_len + 1;

  printf("- Lengths of prefix, API, suffix, and base URL: %zu, %zu, %zu, %zu\n", prefix_len, runtime_api_len, suffix_len, base_url_len);

  printf("- Allocating base URL\n");
  base_url = (char*)cfnpp_malloc(base_url_len);
  if (!base_url) {
    printf("- Could not allocate memory for base URL\n");
    return EXIT_FAILURE;
  }
  base_url[base_url_len - 1] = 0;

  printf("- Copying base URL\n");
  strncpy(base_url, prefix, prefix_len);
  strncpy(base_url + prefix_len, aws_lambda_runtime_api, runtime_api_len);
  strncpy(base_url + prefix_len + runtime_api_len, suffix, suffix_len);

  printf("- Base URL set to \"%s\"\n", base_url);

  return EXIT_SUCCESS;
}

static inline int cfnpp_init_curl(void)
{
  int init_err;

  printf("Initializing CURL...\n");

  printf("- Global...");
  init_err = curl_global_init_mem(CURL_GLOBAL_ALL,
    cfnpp_malloc_with_realloc,
    cfnpp_free,
    cfnpp_realloc,
    cfnpp_strdup,
    cfnpp_calloc_with_realloc
  );
  if (init_err) {
    printf("error: %d\n", init_err);
    return EXIT_FAILURE;
  }
  printf("ok\n");

  printf("- Easy interface...");
  if (!(curl = curl_easy_init())) {
    printf("error\n");
    return EXIT_FAILURE;
  }
  printf("ok\n");

  printf("  - Option: No progress...");
  curl_easy_setopt(curl, CURLOPT_NOPROGRESS, 1);
  printf("ok\n");

  printf("  - Option: Error buffer...");
  curl_easy_setopt(curl, CURLOPT_ERRORBUFFER, curl_err);
  printf("ok\n");

  printf("- CURL initialized\n");
  return EXIT_SUCCESS;
}

static inline int cfnpp_init_jansson(void)
{
  printf("Initializing Jansson...");
  json_set_alloc_funcs(cfnpp_malloc, cfnpp_free);
  printf("ok\n");

  return EXIT_SUCCESS;
}

static inline int cfnpp_init_shutdown_hook(void)
{
  printf("Initializing shutdown hook...\n");

  printf("- Setting signal handler...");
  signal(SIGTERM, cfnpp_stop);
  printf("ok\n");

  printf("- Enabling processing loop...");
  process = 1;
  printf("ok\n");

  return EXIT_SUCCESS;
}

static inline int cfnpp_init_perm_gen(void)
{
  size_t real_allocated = next - base;

  printf("Closing GC system generation with %zu bytes, initializing perm gen\n", allocated);
  if (allocated != real_allocated) {
    printf("!!! GC BUG: cfnpp_malloc(): %zu bytes allocated, next - base: %zu bytes allocated !!!\n", allocated, real_allocated);
    return EXIT_FAILURE;
  }

  allocated = 0;
  base = next;
  reset = next;

  printf("- Base, Reset, Next, End, Allocated: %p, %p, %p, %p, %zu\n", base, reset, next, end, allocated);
  printf("- Perm gen initialized\n");
  return EXIT_SUCCESS;
}

int cfnpp_close_perm_gen(void)
{
  size_t real_allocated = next - base;

  printf("Closing GC perm gen with %zu bytes, preparing for stop-copy pass\n", allocated);
  if (allocated != real_allocated) {
    printf("!!! GC BUG: cfnpp_malloc(): %zu bytes allocated, next - base: %zu bytes allocated !!!\n", allocated, real_allocated);
    return EXIT_FAILURE;
  }

  allocated = 0;
  reset = next;

  printf("- Base, Reset, Next, End, Allocated: %p, %p, %p, %p, %zu\n", base, reset, next, end, allocated);
  printf("- Perm gen closed\n");
  return EXIT_SUCCESS;
}

int cfnpp_init_main_gen(void)
{
  size_t real_allocated = next - reset;
  size_t available = reset - base;

  printf("Relocating GC'ed perm gen of %zu bytes, initializing main gen\n", allocated);
  if (allocated != real_allocated) {
    printf("!!! GC BUG: cfnpp_malloc(): %zu bytes allocated, next - reset: %zu bytes allocated !!!\n", allocated, real_allocated);
    return EXIT_FAILURE;
  }
  if (available < allocated) {
    printf("!!! GC BUG: %zu bytes available for perm gen of size %zu\n", available, allocated);
    return EXIT_FAILURE;
  }

  printf("- Relocating %zu bytes from %p to %p (%zu bytes available)...", allocated, reset, base, available);
  base = memcpy(base, reset, allocated);
  printf("ok\n");

  reset = base + allocated;
  next = reset;
  allocated = 0;

  printf("- Base, Reset, Next, End, Allocated: %p, %p, %p, %p, %zu\n", base, reset, next, end, allocated);
  printf("- Perm gen relocated and main gen initialized.  GC initialization complete.\n");
  return EXIT_SUCCESS;
}

static inline int cfnpp_init_error(const char *msg, const char *type)
{
  // json_t *err_json;
  // json_t *msg_json, *type_json;
  // char *err;
  //
  // size_t runtime_api_len;
  //
  // printf("Sending initialization error \"%s\" of type \"%s\"\n", msg, type);
  //
  // if (!(err_json = json_object())) {
  //   printf("- Error creating JSON object\n");
  //   return EXIT_FAILURE;
  // }
  //
  // if (!(msg_json = json_string(msg))) {
  //   printf("- Error creating JSON message string\n");
  //   return EXIT_FAILURE;
  // }
  // if (json_object_set(err, "errorMessage", msg_json) == -1) {
  //   printf("- Error setting error message\n");
  //   return EXIT_FAILURE;
  // }
  //
  // if (!(type_json = json_string(type))) {
  //   printf("- Error creating JSON type string\n");
  //   return EXIT_FAILURE;
  // }
  // if (json_object_set(err, "errorType", type_json) == -1) {
  //   printf("- Error setting error type\n");
  //   return EXIT_FAILURE;
  // }
  //
  // if (!(err = json_dumps(err, JSON_COMPACT|JSON_ENSURE_ASCII|JSON_SORT_KEYS|JSON_ESCAPE_SLASH))) {
  //   printf("- Error dumping JSON\n");
  //   return EXIT_FAILURE;
  // }
  //
  //
  //
  // curl_easy_setopt(curl, CURLOPT_URL, "");
  // curl_easy_setopt(curl, CURLOPT_POST, "");
  // curl_easy_setopt(curl, CURLOPT_HTTPHEADER, "");

  return EXIT_SUCCESS;
}

/********************
 * Event Processing *
 ********************/

static inline char *cfnpp_build_path(char *prefix, char *id, char *suffix)
{
  char *path;
  size_t prefix_len, id_len, suffix_len, path_len;

  printf("Building path\n");

  prefix_len = strlen(prefix);
  id_len = strlen(id);
  suffix_len = strlen(suffix);
  path_len = prefix_len + id_len + suffix_len + 1;

  printf("- Allocating %zu bytes for path\n", path_len);
  path = (char *)cfnpp_malloc(path_len);
  if (!path) {
    printf("- Could not allocate path\n");
  }
  path[path_len - 1] = 0;

  printf("- Copying path\n");
  memcpy(path, prefix, prefix_len);
  memcpy(path + prefix_len, id, id_len);
  memcpy(path + prefix_len + id_len, suffix, suffix_len);

  printf("- Path is \"%s\"\n", path);
  return path;
}

static inline char *cfnpp_next_invocation_path()
{
  return "/runtime/invocation/next";
}

static inline char *cfnpp_init_error_path()
{
  return "/runtime/init/error";
}

static inline char *cfnpp_invocation_response_path(char *aws_request_id)
{
  return cfnpp_build_path("/runtime/invocation/", aws_request_id, "/response");
}

static inline char *cfnpp_invocation_error_path(char *aws_request_id)
{
  return cfnpp_build_path("/runtime/invocation/", aws_request_id, "/error");
}

static inline char *cfnpp_relative_url(char *path)
{
  char *url;
  size_t path_len, url_len;

  printf("Building relative URL with path \"%s\"\n", path);

  path_len = strlen(path);
  url_len = base_url_len + path_len + 1;

  printf("- Allocating %zu bytes for URL\n", url_len);
  url = (char *)cfnpp_malloc(url_len);
  if (!url) {
    printf("- Could not allocate space for URL\n");
    return NULL;
  }
  url[url_len - 1] = 0;

  printf("- Building URL\n");
  memcpy(url, base_url, base_url_len);
  memcpy(url + base_url_len, path, path_len);

  printf("- URL is \"%s\"\n", url);

  return url;
}

static inline char *cfnpp_next_invocation_url()
{
  return cfnpp_relative_url(cfnpp_next_invocation_path());
}

static inline char *cfnpp_init_error_url()
{
  return cfnpp_relative_url(cfnpp_init_error_path());
}

static inline char *cfnpp_invocation_response_url(char *aws_request_id)
{
  return cfnpp_relative_url(cfnpp_invocation_response_path(aws_request_id));
}

static inline char *cfnpp_invocation_error_url(char *aws_request_id)
{
  return cfnpp_relative_url(cfnpp_invocation_error_path(aws_request_id));
}

int cfnpp_next(json_t *req)
{

  return EXIT_SUCCESS;
}

int cfnpp_success(json_t *resp)
{

  return EXIT_SUCCESS;
}

int cfnpp_failure(const char *msg, const char *type)
{

  return EXIT_SUCCESS;
}

int cfnpp_finalize(void)
{
  printf("Finalizing request\n");

  if (cfnpp_gc()) {
    printf("Error performing GC\n");
    return EXIT_FAILURE;
  }

  printf("Request finalized\n");
  return EXIT_SUCCESS;
}

void cfnpp_stop(int signum)
{
  printf("Received signal %d", signum);

  if (signum == SIGTERM) {
    printf("TERM signal.  Shutting down event processing loop...");
    process = 0;
    printf("ok\n");
  }
}

/************
 * Shutdown *
 ************/

int cfnpp_shutdown(void)
{
  printf("Shutting down AWS Lambda runtime...\n");

  if (cfnpp_shutdown_curl()) {
    printf("Error shutting down curl\n");
    return EXIT_FAILURE;
  }

  if (cfnpp_shutdown_gc()) {
    printf("Error shutting down GC\n");
    return EXIT_FAILURE;
  }

  printf("Runtime terminated\n");
  return EXIT_SUCCESS;
}

static inline int cfnpp_shutdown_curl(void)
{
  printf("Shutting down CURL...\n");

  printf("- Easy interface...");
  curl_easy_cleanup(curl);
  printf("ok\n");

  printf("- Global...");
  curl_global_cleanup();
  printf("ok\n");

  printf("- CURL shut down\n");
  return EXIT_SUCCESS;
}

static inline int cfnpp_shutdown_gc(void)
{
  printf("Shutting down GC...");
  free(base);
  printf("ok\n");

  return EXIT_SUCCESS;
}
