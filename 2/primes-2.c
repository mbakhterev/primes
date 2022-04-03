#include <stdio.h>                                                                                                     
#include <stdlib.h>                                                                                                    
#include <unistd.h>                                                                                                    
                                                                                                                       
typedef struct {                                                                                                       
  long *restrict buf;                                                                                                  
  long len;                                                                                                            
  long cursor;                                                                                                         
} Array;                                                                                                               
                                                                                                                       
static Array make_array() {                                                                                            
  const long pagelen = sysconf(_SC_PAGESIZE) / sizeof(long);                                                           
  return (Array) {                                                                                                     
      .buf = malloc(pagelen * sizeof(long)),                                                                           
      .len = pagelen,                                                                                                  
      .cursor = 0 };                                                                                                   
}                                                                                                                      
                                                                                                                       
#define unlikely(X) __builtin_expect((X), 0)                                                                           
                                                                                                                       
static void push(const long val, Array *restrict const A) {                                                            
  if (unlikely(A->cursor >= A->len)) {                                                                                 
    A->len += sysconf(_SC_PAGESIZE) / sizeof(long);                                                                    
    A->buf = realloc(A->buf, A->len * sizeof(long));                                                                   
  }                                                                                                                    
  A->buf[A->cursor++] = val;                                                                                           
}                                                                                                                      
                                                                                                                       
static long counting_free(Array *restrict const A) {                                                                   
  free(A->buf);                                                                                                        
  A->buf = NULL;                                                                                                       
  return A->cursor;
}                                                                                                                      
                                                                                                                       
static int is_prime(const long n, const Array *restrict const P) {                                                     
  for (int i = 0; i < P->cursor; i++) {                                                                                
    const long p = P->buf[i];                                                                                          
    if (p > n / 2) return 1;                                                                                           
    if (n % p == 0) return 0;                                                                                          
  }                                                                                                                    
  return 1;                                                                                                            
}                                                                                                                      
                                                                                                                       
int main(int argc, char *argv[]) {                                                                                     
  if (argc < 2) {                                                                                                      
    fprintf(stderr, "Specify limit\n");                                                                                
    return 1;                                                                                                          
  }                                                                                                                    
  const long N = atol(argv[1]);                                                                                        
                                                                                                                       
  Array primes = make_array();                                                                                         
  push(2, &primes);                                                                                                    
  for(long n = 3; n <= N; n += 2)                                                                                      
    if (is_prime(n, &primes)) push(n, &primes);                                                                        
                                                                                                                       
  printf("%ld\n", counting_free(&primes));                                                                             
  return 0;                                                                                                            
}
