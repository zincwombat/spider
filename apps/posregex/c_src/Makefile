
include ../config/include.mk

PRIV_FILES=../priv/posregex_drv.so 


CFLAGS += -I$(ERL_C_INCLUDE_DIR) -I../config -I.


#
# Targets
#

all: $(PRIV_FILES)

clean:
	-rm -f $(PRIV_FILES) posregex_drv.o 


install: 
	install -d $(ERLDIR)/lib/posregex
	cp -r `pwd` $(ERLDIR)/lib/posregex
	cp -r `pwd`/../ebin $(ERLDIR)/lib/posregex
	cp -r `pwd`/../priv $(ERLDIR)/lib/posregex


../priv/posregex_drv.so: posregex_drv.o 
	$(LD_SHARED) -o $@ posregex_drv.o $(LIBS)


posregex_drv.o: posregex_drv.c
	$(CC) -o $@ -c -fpic $(CFLAGS) -DDYNAMIC_DRIVER posregex_drv.c


