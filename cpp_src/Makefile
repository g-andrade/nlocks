ifeq ($(OS),Windows_NT)
    OS_detected := Windows
else
    OS_detected := $(shell uname -s)
endif

CXX=g++
RM=rm -f
INC=
CPPFLAGS=-std=c++11 -Wall -Wextra -Wpedantic -fPIC -O2 $(INC)
LDLIBS=

ifeq ($(OS_detected), Darwin)
LDFLAGS := -fPIC -flat_namespace -undefined suppress
else
LDFLAGS := -fPIC
endif


NLOCKS_SRCS=nlocks.cpp
NLOCKS_OBJECT=../priv/nlocks_nif.so

.PHONY: all clean

all: $(NLOCKS_OBJECT)

$(NLOCKS_OBJECT): $(OBJS)
	$(CXX) $(CPPFLAGS) $(LDFLAGS) -shared -o $(NLOCKS_OBJECT) $(NLOCKS_SRCS) $(LDLIBS)

clean:
	$(RM) $(NLOCKS_OBJECT)
