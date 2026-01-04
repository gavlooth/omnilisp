# Tower of Interpreters Makefile
CC = gcc
CFLAGS = -std=c99 -Wall -Wextra -g -I.. -I../pika -I../pika_c

SRCS = tower.c
OBJS = $(SRCS:.c=.o)

all: $(OBJS)

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm -f $(OBJS)

.PHONY: all clean
