
#include "genussfold.c"

int main () {
  char *p = calloc (10000, sizeof(char));
  int *q = calloc (10000, sizeof(int));
  long long n;
  long long e;
  long long i;
  while (1==scanf ("%9999s", p)) { // only GNU C
    n = strlen(p);
    for (i=0;i<n;i++) {
      q[i] = toupper(p[i]);
    }
    e = pseudoknot (n, q);
    printf ("%s\n%lld\n", p, e);
  };
  return 0;
}

