NAME = redis
SRCS := redis.ml
CMAS := $(NAME).cma
CMXAS := $(NAME).cmxa
CMIS := $(SRCS:.ml=.cmi)
LIBS = $(CMAS) $(CMXAS)
BUILD = $(addprefix _build/,$(LIBS)) $(addprefix _build/,$(CMIS)) _build/$(NAME).a

.PHONY: all install uninstall clean

all: $(NAME).mllib
	ocamlbuild -tag debug $(LIBS)

install:
	ocamlfind install $(NAME) $(BUILD) META

uninstall:
	ocamlfind remove $(NAME)

clean:
	rm $(NAME).mllib
	ocamlbuild -clean

$(NAME).mllib:
	echo $(SRCS) | sed -e "s/ /\n/g" | sed -e "s/^./\U&/g" | sed -e "s/\.ml//g" > $@