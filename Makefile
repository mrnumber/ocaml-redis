all:
	make -C src $@

%:
	cd src && make $@
