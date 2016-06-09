CXX=g++
RM=rm -f
INC=-I$(ERL_INCLUDE)
CPPFLAGS=-std=c++11 -Wall -Wextra -Wpedantic -fPIC -O2 $(INC)
LDFLAGS=-fPIC
LDLIBS=

NLOCKS_SRCS=nlocks.cpp
NLOCKS_OBJECT=../priv/nlocks_nif.so

.PHONY: all clean

all: $(NLOCKS_OBJECT)

$(NLOCKS_OBJECT): $(OBJS)
	$(CXX) $(CPPFLAGS) $(LDFLAGS) -shared -o $(NLOCKS_OBJECT) $(NLOCKS_SRCS) $(LDLIBS)

clean:
	$(RM) $(NLOCKS_OBJECT)
