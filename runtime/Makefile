CC := clang

BUILD_DIR := build
TEST_DIR := test

RUNTIME_SRC := $(wildcard *.c)
RUNTIME_OBJ := $(addprefix $(BUILD_DIR)/,$(RUNTIME_SRC:%.c=%.o))
RUNTIME_DEP := $(RUNTIME_SRC:%.c=%.d)

TEST_SRC := $(wildcard $(TEST_DIR)/*.ll)
TEST_OBJ := $(addprefix $(BUILD_DIR)/,$(TEST_SRC:%.ll=%.o))
TEST_DEP := $($(filter %.c,$(TEST_SRC)):%.c=%.d)
TEST_OUT := $(BUILD_DIR)/$(TEST_DIR)/test.out

.PHONY: all test clean distclean

all: test
test: $(TEST_OUT)
	./$(TEST_OUT)
clean:
	rm -rf $(BUILD_DIR)
distclean: clean
	rm -f $(RUNTIME_DEP)
	rm -f $(TEST_DEP)

$(TEST_OUT): $(RUNTIME_OBJ) $(TEST_OBJ)
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
include $(TEST_DEP)
