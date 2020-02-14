TARGETS := add after-return capture cp early-return equal factorial fibb full if ignore \
	mutual-recursion print rebind sub while

CASES := $(foreach dir,$(TARGETS),$(wildcard $(dir)/*.out))

define diff-or-remove =
diff -q $*.out $@ || (rm $@ && false)
endef

SUPERCASES := $(foreach suffix,i0 i1 i2 i3 i4 i5 i6 run,$(CASES:%.out=%.$(suffix)))

all: $(SUPERCASES)

.PHONY: all clean

clean:
	-rm $(SUPERCASES)
	-rm $(foreach suffix,test.S test.exe,$(addsuffix /$(suffix),$(TARGETS)))

$(addsuffix /test.S,$(TARGETS)): %/test.S: %/test.lambda
	$(LAMBDA) $^ -o $@

$(addsuffix /test.exe,$(TARGETS)): %/test.exe: %/test.S $(RUNTIME).o
	$(CC) $(CFLAGS) $^ -o $@

.SECONDEXPANSION:
$(CASES:%.out=%.run): %.run: $$(dir $$@)test.exe %.in %.out
	$< <$*.in >$@ || (rm $@ && false)
	$(diff-or-remove)

$(CASES:%.out=%.i6): %.i6: $$(dir $$@)test.lambda %.in %.out
	$(LAMBDA) -i 6 $< <$*.in >$@ || (rm $@ && false)
	$(diff-or-remove)

$(CASES:%.out=%.i5): %.i5: $$(dir $$@)test.lambda %.in %.out
	$(LAMBDA) -i 5 $< <$*.in >$@ || (rm $@ && false)
	$(diff-or-remove)

$(CASES:%.out=%.i4): %.i4: $$(dir $$@)test.lambda %.in %.out
	$(LAMBDA) -i 4 $< <$*.in >$@ || (rm $@ && false)
	$(diff-or-remove)

$(CASES:%.out=%.i3): %.i3: $$(dir $$@)test.lambda %.in %.out
	$(LAMBDA) -i 3 $< <$*.in >$@ || (rm $@ && false)
	$(diff-or-remove)

$(CASES:%.out=%.i2): %.i2: $$(dir $$@)test.lambda %.in %.out
	$(LAMBDA) -i 2 $< <$*.in >$@ || (rm $@ && false)
	$(diff-or-remove)

$(CASES:%.out=%.i1): %.i1: $$(dir $$@)test.lambda %.in %.out
	$(LAMBDA) -i 1 $< <$*.in >$@ || (rm $@ && false)
	$(diff-or-remove)

$(CASES:%.out=%.i0): %.i0: $$(dir $$@)test.lambda %.in %.out
	$(LAMBDA) -i 0 $< <$*.in >$@ || (rm $@ && false)
	$(diff-or-remove)
