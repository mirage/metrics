.PHONY: all clean test

all:
	jbuilder build --dev

clean:
	jbuilder clean

test:
	jbuilder runtest --dev --force
