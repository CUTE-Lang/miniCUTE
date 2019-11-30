.DEFAULT_GOAL := all

# Variables in Implicit Rules
AR := llvm-ar
ARFLAGS :=
CC := clang
LDFLAGS += -fuse-ld=lld
DOXYGEN := doxygen
DOXYGENFLAGS :=
LLC := llc
LLCFLAGS :=
OPT := opt
OPTFLAGS :=

# Variables of Directories
CONFIG_DIR := config

LIB_DIR := lib
BUILD_DIR := build
DOC_DIR := doc

SCRATCH_DIR := scratch

# Extra Makefiles
MAKEFILES := clean.mk exe_ext.mk support_check.mk

include $(addprefix $(CONFIG_DIR)/,$(MAKEFILES))

# Functions
in_build_dir = $(addprefix $(BUILD_DIR)/,$(1))
one_to_another = $(call in_build_dir,$(1:%$(2)=%$(3)))
c_to_obj = $(call one_to_another,$(1),.c,.o)
ll_to_bc = $(call one_to_another,$(1),.ll,.bc)
ll_to_obj = $(call one_to_another,$(1),.ll,.o)

c_to_dep = $(1:%.c=%.d)
bc_to_obj = $(1:%.bc=%.o)

every = $(wildcard $(1)/*$(2) $(1)/**/*$(2))

# Variables of Files
LIB_C_SRC := $(call every,$(LIB_DIR),.c)
LIB_C_OBJ := $(call c_to_obj,$(LIB_C_SRC))
LIB_C_DEP := $(call c_to_dep,$(LIB_C_SRC))

LIB_C_LIB := libminicute.a

SCRATCH_SRC := $(call every,$(SCRATCH_DIR),.ll)
SCRATCH_OBJ := $(call ll_to_obj,$(SCRATCH_SRC))
SCRATCH_DEP := $(call c_to_dep,$(filter %.c,$(SCRATCH_SRC)))

SCRATCH_OUT := $(call to_exe,scratch)

# Standard .PHONY Tags
.PHONY: all \
        clean \
        distclean \
        mostlyclean \
        maintainer-clean \
        html \
        check

# Non-standard .PHONY Tags
.PHONY: clean_lib \
        clean_dep \
        clean_build \
        clean_doc \
        clean_out


all: $(SCRATCH_OUT)
clean: clean_lib clean_build clean_doc clean_dep clean_out
distclean: clean_build clean_dep
mostlyclean: clean_lib clean_build clean_doc clean_out
maintainer-clean: clean_lib clean_build clean_doc clean_dep clean_out
html:
	@$(DOXYGEN) $(DOXYGENFLAGS)
check: $(SCRATCH_OUT)
	./$<


clean_lib: $(call to_clean,$(LIB_C_LIB))
clean_dep: $(call to_clean,$(LIB_C_DEP))
clean_build: $(call to_clean,$(BUILD_DIR))
clean_doc: $(call to_clean,$(DOC_DIR))
clean_out: $(call to_clean,$(SCRATCH_OUT))


$(SCRATCH_OUT): CFLAGS += -g
$(SCRATCH_OUT): $(SCRATCH_OBJ) $(LIB_C_LIB)
	@mkdir -p $(@D)
	@printf "[ \e[93m%-3s\e[0m ] Build \e[92m%s\e[0m\n" "EXE" "$@"
	$(CC) $(LDFLAGS) $^ $(LDLIBS) -o $@

$(LIB_C_LIB): $(LIB_C_LIB)($(LIB_C_OBJ))
	@printf "[ \e[93m%-3s\e[0m ] Generate an index for \e[92m%s\e[0m\n" "A" "$@"
	ranlib $@

$(LIB_C_LIB)(%.o): private ARFLAGS := -rcU
$(LIB_C_LIB)(%.o): %.o
	@mkdir -p $(@D)
	@printf "[ \e[93m%-3s\e[0m ] Insert \e[92m%s\e[0m into \e[92m%s\e[0m\n" "A" "$^" "$@"
	$(AR) $(ARFLAGS) $@ $^


$(call c_to_obj,%.c): %.c
	@mkdir -p $(@D)
	@printf "[ \e[93m%-3s\e[0m ] Compile \e[92m%s\e[0m to \e[92m%s\e[0m\n" "O" "$<" "$@"
	$(CC) $(CPPFLAGS) $(CFLAGS) -c $< -o $@

$(call ll_to_bc,%.ll): %.ll
	@mkdir -p $(@D)
	@printf "[ \e[93m%-3s\e[0m ] Compile \e[92m%s\e[0m to \e[92m%s\e[0m\n" "BC" "$<" "$@"
	$(OPT) $(OPTFLAGS) $< -o $@

$(call bc_to_obj,%.bc): private LLCFLAGS := -filetype obj
$(call bc_to_obj,%.bc): %.bc
	@printf "[ \e[93m%-3s\e[0m ] Compile \e[92m%s\e[0m to \e[92m%s\e[0m\n" "O" "$<" "$@"
	$(LLC) $(LLCFLAGS) $< -o $@


$(call c_to_dep,%.c): %.c
	@printf "[ \e[93m%-3s\e[0m ] Create \e[92m%s\e[0m for \e[92m%s\e[0m\n" "D" "$@" "$<"
	$(CC) -M $(CPPFLAGS) $< -MQ $(call c_to_obj,$<) -MQ $@ -MF $@

-include $(LIB_C_DEP)
-include $(SCRATCH_DEP)
