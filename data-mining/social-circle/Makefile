CXXFLAGS=-Wall -Wextra -std=c++0x

ifdef ENABLE_CPU_TIMER
CXXFLAGS+=-DENABLE_CPU_TIMER
LDFLAGS=-lboost_timer -lboost_system
endif

all: social social-pg

social: social.cpp
	g++ $(CXXFLAGS) -O2 -o $@ $(LDFLAGS) $<

social-pg: social.cpp
	g++ $(CXXFLAGS) -g -pg -o $@ $(LDFLAGS) $<

# Requires google-perftools and libgoogle-perftools-dev
social-gp: social.cpp
	g++ $(CXXFLAGS) -O2 -o $@ $(LDFLAGS) -lprofiler -ltcmalloc $<


clean:
	rm -f social social-gp social-pg
