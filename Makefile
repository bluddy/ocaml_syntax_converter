
.PHONY: convert
convert:
	ocamlbuild convert.native && \
	if [ ! -f convert ]; then \
	  ln -s convert.native convert; \
	fi

clean:
	ocamlbuild -clean
