ifeq ($(OS), Windows_NT)
EXE_EXT := .exe
else
EXE_EXT := .out
endif

to_exe = $(addsuffix $(EXE_EXT),$(1))
