to_clean = $(addsuffix .clean,$(1))
from_clean = $(basename $(1))

.PHONY: $(call to_clean,%)

$(call to_clean,%):
	rm -rf $(call from_clean,$@)
