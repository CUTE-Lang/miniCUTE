.PHONY: support_check

*: support_check

ifneq ($(SHELL),)
support_check:
ifeq ($(findstring support_check,$(MAKECMDGOALS)),support_check)
	$(error Do not call "support_check" target directly)
endif
else # ($(SHELL),)
support_check:
ifneq ($(findstring support_check,$(MAKECMDGOALS)),support_check)
	@printf "This system is not yet supported.\n"
	@printf "\n"
	@printf "  - If you use \e[91mWindows\e[0m,\n"
	@printf "    please use \e[92mCygwin\e[0m or \e[92mMsys\e[0m to install Make.\n"
	@printf "\n"
	@printf "  - If not, please report your issue on our GitHub repository:\n"
	@printf "    \e[1mhttps://github.com/CUTE-Lang/minicute/issues\e[0m\n"
	$(error Unsupported system)
else
	$(error Do not call "support_check" target directly)
endif
endif # ($(SHELL),)
