prefix = /vol/haptex/releases/nightly/trusty/x86_64/
rst = ../

all: rsbag-renderer

rsbag-renderer: export RST = $(rst)
rsbag-renderer: $(concat $< .asd) *.lisp 
	PATH=$(prefix)/bin:$$PATH cl                                                    \
	  -Q                                                  \
	  -S "(:source-registry                               \
	       (:directory \""$$(pwd)"\")                     \
	       (:directory \""$$(pwd)/../rsbag-helper"\")     \
	       :inherit-configuration)"                       \
	  -s $@                                               \
	  --dump ! --output $@ -r "rsbag-renderer:main"

.PHONY: all
