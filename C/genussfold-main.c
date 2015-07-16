
#include "genussfold.c"

int main () {
  char *p = calloc (10000, sizeof(char));
  int n;
  int e;
  int i;
  while (1==scanf ("%9999s", p)) { // only GNU C
    n = strlen(p);
    for (i=0;i<n;i++) {
      p[i] = toupper(p[i]);
    }
    e = pseudoknot (n, p);
    printf ("%s\n%d\n", p, e);
  };
  return 0;
}

