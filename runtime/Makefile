ifeq ($(OS), Windows_NT)
EXEC_EXT := exe
else
EXEC_EXT := out
endif
CC := clang

LIB_DIR := lib
BUILD_DIR := build

SCRATCH_DIR := scratch

LIB_C_SRC := $(wildcard $(LIB_DIR)/*.c $(LIB_DIR)/**/*.c)
LIB_C_OBJ := $(LIB_C_SRC:%.c=$(BUILD_DIR)/%.o)
LIB_C_DEP := $(LIB_C_SRC:%.c=$(BUILD_DIR)/%.d)

SCRATCH_SRC := $(wildcard $(SCRATCH_DIR)/*.ll $(SCRATCH_DIR)/**/*.ll)
SCRATCH_OBJ := $(SCRATCH_SRC:%.ll=$(BUILD_DIR)/%.o)
SCRATCH_DEP := $($(filter %.c,$(SCRATCH_SRC)):%.c=$(BUILD_DIR)/%.d)

SCRATCH_OUT := $(BUILD_DIR)/scratch.$(EXEC_EXT)

ifneq ($(SHELL),)

.PHONY: all scratch clean

all: scratch
scratch: $(SCRATCH_OUT)
	@printf ">> \n"
	@printf ">> Execute \x1b[92m%s\x1b[0m\n" "$(SCRATCH_OUT)"
	@printf ">> The output of \x1b[92m%s\x1b[0m is following:\n" "$(SCRATCH_OUT)"
	@./$(SCRATCH_OUT)
clean:
	@printf "Remove \x1b[91m%s\x1b[0m\n" "$(BUILD_DIR)"
	@rm -rf $(BUILD_DIR)

$(SCRATCH_OUT): $(LIB_C_OBJ) $(SCRATCH_OBJ)
	@mkdir -p $(dir $@)
	@printf ">> %-7sBuild \x1b[92m$@\x1b[0m\n" "[O]"
	@$(CC) $(CFLAGS) $(CPPFLAGS) $(LDLIBS) $(LDFLAGS) -o $@ $^

$(BUILD_DIR)/%.d: %.c
	@mkdir -p $(dir $@)
	@set -e
	@printf ">> %-7sCreate dependency file for \x1b[92m%s\x1b[0m\n" "[C]" "$<"
	@$(CC) -M $(CPPFLAGS) $< | sed 's^\($*\)\.o[ :]*^\1.o $@ : ^g' > $@
	@[ -s $@ ] || rm -f $@

$(BUILD_DIR)/%.o: %.c
	@mkdir -p $(dir $@)
	@printf ">> %-7sCompile \x1b[92m%s\x1b[0m\n" "[C]" "$<"
	@$(CC) -c $(CFLAGS) $(CPPFLAGS) -o $@ $<
$(BUILD_DIR)/%.o: %.ll
	@mkdir -p $(dir $@)
	@printf ">> %-7sCompile \x1b[92m%s\x1b[0m\n" "[LL]" "$<"
	@llvm-as < $< | opt -O2 | llc -filetype obj -o $@

ifneq ($(MAKECMDGOALS),clean)

include $(LIB_C_DEP)
include $(SCRATCH_DEP)

endif # ($(MAKECMDGOALS),clean)

else # ($(SHELL),)

.PHONY: all scratch clean unsupported

all: unsupported
scratch: unsupported
clean: unsupported

unsupported:
ifneq ($(MAKECMDGOALS),unsupported)
	@printf "This system is not yet supported.\n"
	@printf "\n"
	@printf "  - If you use \x1b[91mWindows\x1b[0m,\n"
	@printf "    please use \x1b[92mCygwin\x1b[0m or \x1b[92mMsys\x1b[0m to install Make.\n"
	@printf "\n"
	@printf "  - If not, please report your issue on our GitHub repository:\n"
	@printf "    \x1b[1mhttps://github.com/CUTE-Lang/minicute/issues\x1b[0m\n"
else
	@printf "Do not call \"unsupported\" target directly.\n"
endif

endif # ($(SHELL),)
