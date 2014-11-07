#include <stdio.h>
#include <string.h>
#define LEN 5
int main(int argc, char *argv[]) {
  char pass[LEN + 1];
  pass[0] = 'a';
  pass[1] = '\0';
  while (1) {
    printf("%s\n", pass);
    int length = strlen(pass);
    int i;
    for (i = length - 1; i >= 0; i--) {
      if (pass[i] != 'z') {
	pass[i]++;
	int j;
	for (j = length - 1; j > i; j--) pass[j] = 'a';
	break;
      }
      if (i == 0) {
	if (length != LEN) {
	  int k;
	  for (k = 0; k <= length; k++) pass[k] = 'a';
	  pass[length + 1] = '\0';
	  break;
	} else {
	  return 0;
	}
      }
    }
  }
  return 0;
}
