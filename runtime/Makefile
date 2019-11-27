ifeq ($(OS), Windows_NT)
EXEC_EXT := exe
else
EXEC_EXT := out
endif

AR := llvm-ar
ARFLAGS := -rcs
CC := clang
LDFLAGS += -fuse-ld=lld
DOXYGEN := doxygen
DOXYGENFLAGS :=
LLC := llc
LLCFLAGS := -filetype obj
OPT := opt
OPTFLAGS :=

LIB_DIR := lib
BUILD_DIR := build
DOC_DIR := doc

SCRATCH_DIR := scratch

LIB_C_SRC := $(wildcard $(LIB_DIR)/*.c $(LIB_DIR)/**/*.c)
LIB_C_OBJ := $(LIB_C_SRC:%.c=$(BUILD_DIR)/%.o)
LIB_C_DEP := $(LIB_C_SRC:%.c=$(BUILD_DIR)/%.d)

LIB_C_LIB := $(BUILD_DIR)/$(LIB_DIR)/libminicute.a

SCRATCH_SRC := $(wildcard $(SCRATCH_DIR)/*.ll $(SCRATCH_DIR)/**/*.ll)
SCRATCH_OBJ := $(SCRATCH_SRC:%.ll=$(BUILD_DIR)/%.o)
SCRATCH_DEP := $($(filter %.c,$(SCRATCH_SRC)):%.c=$(BUILD_DIR)/%.d)

SCRATCH_OUT := $(BUILD_DIR)/scratch.$(EXEC_EXT)

.PHONY: all \
        clean \
        clean_build \
        clean_doc \
        scratch \
        html

ifneq ($(SHELL),)
all: scratch

scratch: $(SCRATCH_OUT)
	@printf ">> \n"
	@printf ">> Execute \e[92m%s\e[0m\n" "$(SCRATCH_OUT)"
	@printf ">> The output of \e[92m%s\e[0m is following:\n" "$(SCRATCH_OUT)"
	@./$(SCRATCH_OUT)

html:
	@$(DOXYGEN) $(DOXYGENFLAGS)

clean: clean_build clean_doc
clean_build:
	@printf ">> [ \e[93m%-4s\e[0m ] Remove \e[91m%s\e[0m\n" "_" "$(BUILD_DIR)"
	@rm -rf $(BUILD_DIR)
clean_doc:
	@printf ">> [ \e[93m%-4s\e[0m ] Remove \e[91m%s\e[0m\n" "_" "$(DOC_DIR)"
	@rm -rf $(DOC_DIR)

$(SCRATCH_OUT): $(SCRATCH_OBJ) $(LIB_C_LIB)
	@mkdir -p $(dir $@)
	@printf ">> [ \e[93m%-4s\e[0m ] Build \e[92m%s\e[0m\n" "EXE" "$@"
	@$(CC) $(LDFLAGS) $^ $(LDLIBS) -o $@

$(LIB_C_LIB): $(LIB_C_OBJ)
	@mkdir -p $(dir $@)
	@printf ">> [ \e[93m%-4s\e[0m ] Build \e[92m%s\e[0m\n" "A" "$@"
	@$(AR) $(ARFLAGS) $@ $^

$(BUILD_DIR)/%.d: %.c
	@mkdir -p $(dir $@)
	@printf ">> [ \e[93m%-4s\e[0m ] Create \e[92m%s\e[0m for \e[92m%s\e[0m\n" "D" "$@" "$<"
	@$(CC) -M $(CPPFLAGS) $< -MQ $(<:%.c=$(BUILD_DIR)/%.o) -MQ $@ -MF $@

$(BUILD_DIR)/%.o: %.c
	@mkdir -p $(dir $@)
	@printf ">> [ \e[93m%-4s\e[0m ] Compile \e[92m%s\e[0m to \e[92m%s\e[0m\n" "O" "$<" "$@"
	@$(CC) $(CPPFLAGS) $(CFLAGS) -c $< -o $@
$(BUILD_DIR)/%.bc: %.ll
	@mkdir -p $(dir $@)
	@printf ">> [ \e[93m%-4s\e[0m ] Compile \e[92m%s\e[0m to \e[92m%s\e[0m\n" "BC" "$<" "$@"
	@$(OPT) $(OPTFLAGS) $< -o $@

%.o: %.bc
	@printf ">> [ \e[93m%-4s\e[0m ] Compile \e[92m%s\e[0m to \e[92m%s\e[0m\n" "O" "$<" "$@"
	@$(LLC) $(LLCFLAGS) $< -o $@

# Following is a bad hack
# Please add clang to our doxygen image
LOAD_DEPS :=
ifeq ($(MAKECMDGOALS),clean)
LOAD_DEPS += yes
endif # ($(MAKECMDGOALS),clean)
ifeq ($(MAKECMDGOALS),html)
LOAD_DEPS += yes
endif # ($(MAKECMDGOALS),html)
ifndef LOAD_DEPS
include $(LIB_C_DEP)
include $(SCRATCH_DEP)
endif # LOAD_DEPS

else # ($(SHELL),)
.PHONY: unsupported

all: unsupported
scratch: unsupported
clean: unsupported
html: unsupported

unsupported:
ifneq ($(findstring unsupported,$(MAKECMDGOALS)),unsupported)
	@printf "This system is not yet supported.\n"
	@printf "\n"
	@printf "  - If you use \e[91mWindows\e[0m,\n"
	@printf "    please use \e[92mCygwin\e[0m or \e[92mMsys\e[0m to install Make.\n"
	@printf "\n"
	@printf "  - If not, please report your issue on our GitHub repository:\n"
	@printf "    \e[1mhttps://github.com/CUTE-Lang/minicute/issues\e[0m\n"
else
	@printf "Do not call \"unsupported\" target directly.\n"
endif
endif # ($(SHELL),)
