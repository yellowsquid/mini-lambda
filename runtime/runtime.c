#include <stdio.h>
#include <stdlib.h>

#include "lambda.h"

#define HEAP_SIZE 1024

long used = 0;
long capacity = 0;

// Get function with:
// - args above stack
// - return address at stack

// Pass back stack with
// - return value at stack
// - return address above stack
// - args above return address


pointer_t print_int(pointer_t heap, pointer_t stack) {
  printf("%ld\n", *(stack + 1));
  *(--stack) = 0;
  return stack;
}

pointer_t print_bool(pointer_t heap, pointer_t stack) {
  if (*(stack + 1)) {
    puts("true");
  } else {
    puts("false");
  }
  *(--stack) = 0;
  return stack;
}

pointer_t input_int(pointer_t heap, pointer_t stack) {
  scanf("%ld", --stack);
  return stack;
}

pointer_t lambda_alloc(pointer_t heap, long size) {
  if (size + used > capacity) {
    exit(2);
  } else {
    pointer_t out = &heap[used];
    used += size;
    return out;
  }
}

int main(void) {
  pointer_t heap = malloc(HEAP_SIZE * sizeof (*heap));

  if (!heap) {
    return 1;
  }

  used = 0;
  capacity = HEAP_SIZE;
  int out = lambda_main(heap);
  free(heap);
  return out;
}
