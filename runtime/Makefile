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

SCRATCH_OUT := $(BUILD_DIR)/scratch.out

ifneq ($(SHELL),)

.PHONY: all scratch clean

all: scratch
scratch: $(SCRATCH_OUT)
	./$(SCRATCH_OUT)
clean:
	rm -rf $(BUILD_DIR)

$(SCRATCH_OUT): $(LIB_C_OBJ) $(SCRATCH_OBJ)
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) $(CPPFLAGS) $(LDLIBS) $(LDFLAGS) -o $@ $^

$(BUILD_DIR)/%.d: %.c
	@mkdir -p $(dir $@)
	@set -e
	$(CC) -M $(CPPFLAGS) $< | sed 's^\($*\)\.o[ :]*^\1.o $@ : ^g' > $@
	@[ -s $@ ] || rm -f $@

$(BUILD_DIR)/%.o: %.c
	@mkdir -p $(dir $@)
	$(CC) -c $(CFLAGS) $(CPPFLAGS) -o $@ $<
$(BUILD_DIR)/%.o: %.ll
	@mkdir -p $(dir $@)
	llvm-as < $< | opt -O2 | llc -filetype obj -o $@

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
	@echo "This system is not yet supported."
	@echo "If you use Windows, please use Cygwin or Msys to install Make."
	@echo "If not, please report your issue on our GitHub repository:"
	@echo ""
	@echo "    https://github.com/CUTE-Lang/minicute/issues"
	@echo ""

endif # ($(SHELL),)
