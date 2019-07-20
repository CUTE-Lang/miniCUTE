CC := clang

C_SRC_DIR := src
C_LIB_DIR := lib
BUILD_DIR := build

SCRATCH_DIR := scratch

C_LIB_SRC := $(wildcard $(C_SRC_DIR)/*.c $(C_SRC_DIR)/**/*.c)
C_LIB_OBJ := $(C_LIB_SRC:$(C_SRC_DIR)/%.c=$(BUILD_DIR)/$(C_LIB_DIR)/%.o)
C_LIB_DEP := $(C_LIB_SRC:$(C_SRC_DIR)/%.c=$(BUILD_DIR)/$(C_LIB_DIR)/%.d)

SCRATCH_SRC := $(wildcard $(SCRATCH_DIR)/*.ll $(SCRATCH_DIR)/**/*.ll)
SCRATCH_OBJ := $(addprefix $(BUILD_DIR)/,$(SCRATCH_SRC:%.ll=%.o))
SCRATCH_DEP := $($(filter %.c,$(SCRATCH_SRC)):%.c=%.d)

SCRATCH_OUT := $(BUILD_DIR)/$(SCRATCH_DIR)/scratch.out

.PHONY: all scratch clean

all: scratch
scratch: $(SCRATCH_OUT)
	./$(SCRATCH_OUT)
clean:
	rm -rf $(BUILD_DIR)

$(SCRATCH_OUT): $(C_LIB_OBJ) $(SCRATCH_OBJ)
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) $(CPPFLAGS) $(LDLIBS) $(LDFLAGS) -o $@ $^

$(BUILD_DIR)/$(C_LIB_DIR)/%.d: $(C_SRC_DIR)/%.c
	@mkdir -p $(dir $@)
	@set -e
	$(CC) -M $(CPPFLAGS) $< | sed 's^\($*\)\.o[ :]*^\1.o $@ : ^g' > $@
	@[ -s $@ ] || rm -f $@

$(BUILD_DIR)/$(C_LIB_DIR)/%.o: $(C_SRC_DIR)/%.c
	@mkdir -p $(dir $@)
	$(CC) -c $(CFLAGS) $(CPPFLAGS) -o $@ $<
$(BUILD_DIR)/$(SCRATCH_DIR)/%.o: $(SCRATCH_DIR)/%.ll
	@mkdir -p $(dir $@)
	llc -filetype obj -o $@ $<

ifneq ($(MAKECMDGOALS),clean)
include $(C_LIB_DEP)
include $(SCRATCH_DEP)
endif
