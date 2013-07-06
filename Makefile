.NOTPARALLEL:

.PHONY: default
default: build lint test-par

############################################################
##  CONFIGURATION / SETUP

config ?= debug
test_log_dir ?= test_log

OUTPUT_PATH := .dist
HADDOCK_TMP := .dist.haddock
EXECUTABLES := sp spaceport-shake-tests

OUTBIN_PATH := $(OUTPUT_PATH)/build

UNAME := $(shell uname -s)

ifeq (,$(findstring CYGWIN,$(UNAME))$(findstring MINGW,$(UNAME)))
	OUTBIN_EXTENSION :=
else
	OUTBIN_EXTENSION := .exe
	CABAL_CONFIG_FLAGS += "--extra-lib-dirs=$(PWD)/vendor/dnssd/Win32/"
endif

define exepath
$(OUTBIN_PATH)/$1/$1$(OUTBIN_EXTENSION)
endef

EXECUTABLE_PATHS := $(foreach exe,$(EXECUTABLES),$(call exepath,$(exe)))

CABAL ?= cabal
CABAL_FLAGS += --builddir=$(OUTPUT_PATH)

# FIXME Is there a standard name?
CABAL_CONFIG_FLAGS +=

ifeq (debug,$(config))
	CABAL_CONFIG_FLAGS += --disable-optimization --disable-library-profiling
else ifeq (profile-slow,$(config))
	CABAL_CONFIG_FLAGS += --disable-optimization --enable-library-profiling --enable-executable-profiling -fprofile
else ifeq (profile,$(config))
	CABAL_CONFIG_FLAGS += --enable-optimization=1 --enable-library-profiling --enable-executable-profiling -fprofile
else ifeq (release,$(config))
	CABAL_CONFIG_FLAGS += --enable-optimization=2
else
	error := $(error "invalid config $(config); config can be one of: debug profile profile-slow release")
endif

CABAL_HADDOCK_FLAGS += $(CABAL_FLAGS) --builddir=$(HADDOCK_TMP)
HADDOCK_FLAGS += --ignore-all-exports

RTS_FLAGS ?=

RUNTIME_FLAGS = +RTS $(RTS_FLAGS) -RTS

############################################################
##  SETUP RULES

.PHONY: deps configure

deps:
	$(CABAL) install $(CABAL_FLAGS) --only-dependencies

configure:
	$(CABAL) configure $(CABAL_FLAGS) $(CABAL_CONFIG_FLAGS)

############################################################
##  BUILD RULES

.PHONY: all clean build

all: $(EXECUTABLE_PATHS)

clean:
	$(CABAL) clean $(CABAL_FLAGS)
	$(CABAL) clean $(CABAL_HADDOCK_FLAGS)

# Force rebuild of build stamp.
.PHONY: src/ProgramInfo.hs
src/ProgramInfo.hs:
	@touch "$@"

build: src/ProgramInfo.hs
	$(CABAL) build $(CABAL_FLAGS) --builddir=$(OUTPUT_PATH)
ifeq (release,$(config))
	@echo "Stripping executables..."
	@strip $(foreach exe,$(EXECUTABLES),$(call exepath,$(exe)))
endif

define executable
$0_exepath := $$(call exepath,$1)
.PHONY: $$($0_exepath)
$$($0_exepath): build
endef

$(foreach exe,$(EXECUTABLES),$(eval $(call executable,$(exe))))

############################################################
##  TEST RULES

.PHONY: test test-par
test: test-shake test-version test-build test-parallel
test-par: test-shake test-version test-build-par test-parallel

.PHONY: test-shake
test-shake: $(call exepath,spaceport-shake-tests)
	"$<"

.PHONY: test-version
test-version: $(call exepath,sp)
	./test/version.sh $(call exepath,sp)

#

BUILD_TESTS := \
	a2j \
	android \
	android-debug-keystore \
	android-resources \
	asset-ignore \
	asset-include \
	cdn-update \
	change-entry-point \
	codesign \
	database \
	dotfiles \
	duplicate-swf-bug \
	embed \
	ios \
	ios-assets \
	loading-screen \
	manifest \
	output-cruft \
	sgftool \
	spsx \
	swc

.PHONY: test-build test-build-
test-build: $(foreach script,$(BUILD_TESTS),test-build-$(script))
test-build-: $(foreach script,$(BUILD_TESTS),test-build-$(script)-)

define test_par_entry
echo "./test/$1/$2.sh $(call exepath,sp)" >> "$@"
endef

test-build-par.lst: Makefile
	@rm -f "$@"
	@$(foreach script,$(BUILD_TESTS),$(call test_par_entry,build,$(script));)

.PHONY: test-build-par
#test-build-par: $(call exepath,sp) test-build-par.lst
test-build-par: test-build-par.lst
	./scripts/parallel.sh "$(test_log_dir)" test-build-par.lst

define test_body
test-$1-$2-:
	./test/$1/$2.sh $3

test-$1-$2: $3
	./test/$1/$2.sh $3
endef

$(foreach script,$(BUILD_TESTS),$(eval $(call test_body,build,$(script),$(call exepath,sp))))

#

PARALLEL_TESTS := failure success

.PHONY: test-parallel
test-parallel: $(foreach script,$(PARALLEL_TESTS),test-parallel-$(script))

$(foreach script,$(PARALLEL_TESTS),$(eval $(call test_body,parallel,$(script),scripts/parallel.sh)))

############################################################
##  MISC RULES

.PHONY: haddock lint hlint
haddock:
	$(CABAL) $(CABAL_HADDOCK_FLAGS) configure -fdochack
	$(CABAL) $(CABAL_HADDOCK_FLAGS) haddock --haddock-options $(HADDOCK_FLAGS)

lint: hlint

hlint:
	@echo Linting...
	@find src/ -name '*.hs' -print0 | xargs -0 hlint \
		--cpp-define HLINT --cpp-define DNSSD_CALL=stdcall \
		--cpp-define USE_NAILGUN \
		-i'Use camelCase' -i'Use record patterns' -i'Use on' -i'Use String' -i'Use :' -- >&2
