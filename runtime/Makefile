CC := clang

BUILD_DIR := build
SCRATCH_DIR := scratch

RUNTIME_SRC := $(wildcard *.c)
RUNTIME_OBJ := $(addprefix $(BUILD_DIR)/,$(RUNTIME_SRC:%.c=%.o))
RUNTIME_DEP := $(RUNTIME_SRC:%.c=%.d)

SCRATCH_SRC := $(wildcard $(SCRATCH_DIR)/*.ll)
SCRATCH_OBJ := $(addprefix $(BUILD_DIR)/,$(SCRATCH_SRC:%.ll=%.o))
SCRATCH_DEP := $($(filter %.c,$(SCRATCH_SRC)):%.c=%.d)
SCRATCH_OUT := $(BUILD_DIR)/$(SCRATCH_DIR)/scratch.out

.PHONY: all scratch clean distclean

all: scratch
scratch: $(SCRATCH_OUT)
	./$(SCRATCH_OUT)
clean:
	rm -rf $(BUILD_DIR)
distclean: clean
	rm -f $(RUNTIME_DEP)
	rm -f $(SCRATCH_DEP)

$(SCRATCH_OUT): $(RUNTIME_OBJ) $(SCRATCH_OBJ)
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) $(CPPFLAGS) $(LDLIBS) $(LDFLAGS) -o $@ $^

%.d: %.c
	@set -e
	$(CC) -M $(CPPFLAGS) $< | sed 's/\($*\)\.o[ :]*/\1.o $@ : /g' > $@
	@[ -s $@ ] || rm -f $@

$(BUILD_DIR)/%.o: %.c
	@mkdir -p $(dir $@)
	$(CC) -c $(CFLAGS) $(CPPFLAGS) -o $@ $<
$(BUILD_DIR)/%.o: %.ll
	@mkdir -p $(dir $@)
	llc -filetype obj -o $@ $<

include $(RUNTIME_DEP)
include $(SCRATCH_DEP)
