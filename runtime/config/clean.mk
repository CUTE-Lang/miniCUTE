to_clean = $(addsuffix .clean,$(1))

.PHONY: $(call to_clean,%)

$(call to_clean,%):
	-rm -rf $(basename $@)
