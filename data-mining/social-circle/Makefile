all: social social-gp social-pg

social: social.cpp
	g++ -Wall -Wextra -O2 -o $@ -lboost_timer -lboost_system $<

social-gp: social.cpp
	g++ -Wall -Wextra -O2 -o $@ -lboost_timer -lboost_system -lprofiler -ltcmalloc $<

social-pg: social.cpp
	g++ -Wall -Wextra -g -pg -o $@ -lboost_timer -lboost_system $<

clean:
	rm -f social social-gp social-pg
