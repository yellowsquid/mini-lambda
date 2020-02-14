#include <stdio.h>
#include <stdlib.h>

#include "lambda.h"

#define HEAP_SIZE 1024

long used = 0;
long capacity = 0;

long *print_int(long *heap, long *stack) {
  printf("%ld\n", *stack);
  *stack = 0;
  return stack;
}

long *print_bool(long *heap, long *stack) {
  if (*stack) {
    puts("true\n");
  } else {
    puts("false\n");
  }
  *stack = 0;
  return stack;
}

long *input_int(long *heap, long *stack) {
  stack--;
  scanf("%ld", stack);
  return stack;
}

long *lambda_alloc(long *heap, long size) {
  if (size + used > capacity) {
    exit(2);
  } else {
    long *out = &heap[used];
    used += size;
    return out;
  }
}

int main(void) {
  long *heap = malloc(HEAP_SIZE * sizeof (*heap));

  if (!heap) {
    return 1;
  }

  used = 0;
  capacity = HEAP_SIZE;
  int out = lambda_main(heap);
  free(heap);
  return out;
}
