.PHONY: all clean
CXX := clang++
PLATFORM := $(shell uname -s)
ifeq ($(PLATFORM),Darwin)
LIB := libstallion_sbin.dylib
SHARED_FLAGS := -dynamiclib
else
LIB := libstallion_sbin.so
SHARED_FLAGS := -shared -fPIC
endif
all: $(LIB)
$(LIB): sbin.cpp sbin.h ../engine/src/bitboard.h ../engine/src/defs.h Makefile
	@set -eu; sbin_build="$@.tmp.$$$$"; trap 'rm -f "$$sbin_build"' EXIT; \
	$(CXX) -O3 -std=c++17 $(SHARED_FLAGS) sbin.cpp -o "$$sbin_build"; \
	mv -f "$$sbin_build" "$@"
clean:
	rm -f $(LIB)
