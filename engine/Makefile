.DEFAULT_GOAL := build
HOST_PLATFORM := $(if $(filter Darwin,$(shell uname -s)),mac,linux)
.PHONY: build default all mac linux windows android clean asan debug

build default: $(HOST_PLATFORM)

mac:
	$(MAKE) -f Makefile.mac

linux:
	$(MAKE) -f Makefile.linux

windows:
	$(MAKE) -f Makefile.windows

android:
	$(MAKE) -f Makefile.android

all: mac linux windows android

clean:
	$(MAKE) -f Makefile.clean all

asan debug:
	$(MAKE) -f Makefile.$(HOST_PLATFORM) asan
