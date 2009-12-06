all: callview

callview: main.vala CallView.vala
	valac -g -o callview main.vala CallView.vala --pkg gee-1.0

clean:
	rm -f callview *.vala.c

