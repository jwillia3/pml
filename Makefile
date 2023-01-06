test: boot
	./boot test.pml

boot: boot.c
	$(CC) -g -oboot boot.c
