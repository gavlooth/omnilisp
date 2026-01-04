# Pika Parser Integration Makefile
CC = gcc
CFLAGS = -std=c99 -Wall -Wextra -g -I.. -I../pika_c

SRCS = omni_grammar.c pika_reader.c
OBJS = $(SRCS:.c=.o)

# pika_c library
PIKA_OBJ = ../pika_c/pika.o

all: $(OBJS)

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

$(PIKA_OBJ):
	$(MAKE) -C ../pika_c

clean:
	rm -f $(OBJS)

.PHONY: all clean
