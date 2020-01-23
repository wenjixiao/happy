SRC_DIR = src
INCLUDE_DIR = include
LIB_DIR = lib
BIN_DIR = ebin

SOURCES = $(wildcard $(SRC_DIR)/*.erl)
OBJECTS = $(patsubst %.erl,%.beam,$(SOURCES))

compile: $(OBJECTS)

runserver:
	erl -noshell -pa ${BIN_DIR} -s myserver start -s init stop

runclient:
	erl -noshell -pa ${BIN_DIR} -s myclient start -s init stop

clean:
	del ${BIN_DIR}\*.beam

%.beam: %.erl
	erlc -W -I $(INCLUDE_DIR) -o $(BIN_DIR) $< 
