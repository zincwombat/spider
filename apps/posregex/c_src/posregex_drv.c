
/*
 * Copyright 2005 Tail-F Systems AB
 * author: klacke@tail-f.com
 */

/* Interface to POSIX regexp API */

#include <sys/types.h>
#include <regex.h>
#include <stdio.h>

#ifndef REG_NOERROR
#define REG_NOERROR 0
#endif

#include "erl_driver.h"

#  define malloc(s)     driver_alloc(s)
#  define realloc(p, s) driver_realloc(p, s)
#  define free(p)       driver_free(p)

#define get_int32(s) ((((unsigned char*) (s))[0] << 24) | \
                      (((unsigned char*) (s))[1] << 16) | \
                      (((unsigned char*) (s))[2] << 8)  | \
                      (((unsigned char*) (s))[3]))

#define put_int32(i, s) {((char*)(s))[0] = (char)((i) >> 24) & 0xff; \
                        ((char*)(s))[1] = (char)((i) >> 16) & 0xff; \
                        ((char*)(s))[2] = (char)((i) >> 8)  & 0xff; \
                        ((char*)(s))[3] = (char)((i)        & 0xff);}




/* these should really be defined in driver.h */
#define LOAD_ATOM(vec, i, atom) \
  (((vec)[(i)] = ERL_DRV_ATOM), \
  ((vec)[(i)+1] = (atom)), \
  (i+2))

#define LOAD_INT(vec, i, val) \
  (((vec)[(i)] = ERL_DRV_INT), \
  ((vec)[(i)+1] = (ErlDrvTermData)(val)), \
  (i+2))

#define LOAD_PORT(vec, i, port) \
  (((vec)[(i)] = ERL_DRV_PORT), \
  ((vec)[(i)+1] = (port)), \
  (i+2))

#define LOAD_PID(vec, i, pid) \
  (((vec)[(i)] = ERL_DRV_PID), \
  ((vec)[(i)+1] = (pid)), \
  (i+2))

#define LOAD_BINARY(vec, i, bin, offs, len) \
  (((vec)[(i)] = ERL_DRV_BINARY), \
  ((vec)[(i)+1] = (ErlDrvTermData)(bin)), \
  ((vec)[(i)+2] = (len)), \
  ((vec)[(i)+3] = (offs)), \
  (i+4))

#define LOAD_STRING(vec, i, str, len) \
  (((vec)[(i)] = ERL_DRV_STRING), \
  ((vec)[(i)+1] = (ErlDrvTermData)(str)), \
  ((vec)[(i)+2] = (len)), \
  (i+3))

#define LOAD_STRING_CONS(vec, i, str, len) \
  (((vec)[(i)] = ERL_DRV_STRING_CONS), \
  ((vec)[(i)+1] = (ErlDrvTermData)(str)), \
  ((vec)[(i)+2] = (len)), \
  (i+3))

#define LOAD_TUPLE(vec, i, size) \
  (((vec)[(i)] = ERL_DRV_TUPLE), \
  ((vec)[(i)+1] = (size)), \
  (i+2))

#define LOAD_LIST(vec, i, size) \
  (((vec)[(i)] = ERL_DRV_LIST), \
  ((vec)[(i)+1] = (size)), \
  (i+2))

static int driver_send_bin();

/* atoms which are sent to erlang */
static ErlDrvTermData am_ok;
static ErlDrvTermData am_value;
static ErlDrvTermData am_error;
static ErlDrvTermData am_enomem;
static ErlDrvTermData am_unknown;

static ErlDrvTermData    am_nomatch;
static ErlDrvTermData    am_badpat;
static ErlDrvTermData    am_ecollate;
static ErlDrvTermData    am_ectype;
static ErlDrvTermData    am_eescape;
static ErlDrvTermData    am_esubreg;
static ErlDrvTermData    am_ebrack;
static ErlDrvTermData    am_eparen;
static ErlDrvTermData    am_ebrace;
static ErlDrvTermData    am_badbr;
static ErlDrvTermData    am_erange;
static ErlDrvTermData    am_espace;
static ErlDrvTermData    am_badrpt;
static ErlDrvTermData    am_eend;
static ErlDrvTermData    am_esize;
static ErlDrvTermData    am_erparen;
static ErlDrvTermData    am_empty;
static ErlDrvTermData    am_badarg;




/* op codes */

#define COMPILE             'c'
#define EXEC                'e'
#define MATCH               'm'



static ErlDrvData posregex_start(ErlDrvPort port, char *buf);
static void posregex_stop(ErlDrvData drv_data);

static ErlDrvEntry posregex_driver_entry;

typedef struct _desc {
    ErlDrvPort     port;
    ErlDrvTermData dport;          /* the port identifier as ErlDrvTermData */
    regex_t re;
    regmatch_t pm[16];
    int compiled;
} Desc;


static ErlDrvData posregex_start(ErlDrvPort port, char *buf)
{
    Desc *d = (Desc*) driver_alloc(sizeof (Desc));
    
    if (d == NULL) 
	return (ErlDrvData) -1;
    d->port = port;
    d->dport = driver_mk_port(port);
    d->compiled = 0;
    return (ErlDrvData) d;
}


static void posregex_stop(ErlDrvData drv_data)
{
    Desc *d = (Desc*) drv_data;
    if (d->compiled)
	regfree(&(d->re));
    driver_free(d);
}


/* send {P, value, Bin} to caller */
static int driver_send_bin(Desc *d, ErlDrvBinary *bin, int len)
{
    int i = 0;
    ErlDrvTermData to, spec[10];

    to = driver_caller(d->port);

    i = LOAD_PORT(spec, i, d->dport);
    i = LOAD_ATOM(spec, i, am_value);
    i = LOAD_BINARY(spec, i, bin, 0, len);
    i = LOAD_TUPLE(spec, i, 3);

    return driver_send_term(d->port, to, spec, i);
}


static int driver_send_pm(Desc *d) 
{
    int i = 0;
    int j = 0;
    ErlDrvTermData to, spec[256];

    to = driver_caller(d->port);
    i = LOAD_PORT(spec, i, d->dport);
    i = LOAD_ATOM(spec, i, am_ok);
    while (d->pm[j].rm_so != -1) {
	 i = LOAD_INT(spec, i, d->pm[j].rm_so);
	 i = LOAD_INT(spec, i, d->pm[j].rm_eo);
	 i = LOAD_TUPLE(spec, i, 2);
	 j++;
     }
     spec[i++] = ERL_DRV_NIL;
     i = LOAD_LIST(spec, i, j+1);
     i = LOAD_TUPLE(spec, i, 3);
    return driver_send_term(d->port, to, spec, i);
}


/* send {P, Int} to caller */
static int driver_send_status(Desc *d, int status)
{
    int i = 0;
    ErlDrvTermData to, spc[10];

    to = driver_caller(d->port);
    i = LOAD_PORT(spc, i, d->dport);
    switch (status) {
    case REG_NOERROR:
	i = LOAD_ATOM(spc, i, am_ok);
	break;
    case REG_NOMATCH:
	i = LOAD_ATOM(spc, i, am_nomatch);
	break;
    case REG_BADPAT:
	i = LOAD_ATOM(spc, i, am_badpat);
	break;
    case REG_ECOLLATE:
	i = LOAD_ATOM(spc, i, am_ecollate);
	break;
    case REG_ECTYPE:
	i = LOAD_ATOM(spc, i, am_ecollate);
	break;
    case REG_EESCAPE:
	i = LOAD_ATOM(spc, i, am_eescape);
	break;
    case REG_ESUBREG:
	i = LOAD_ATOM(spc, i, am_esubreg);
	break;
    case REG_EBRACK:
	i = LOAD_ATOM(spc, i, am_ebrack);
	break;
    case REG_EPAREN:
	i = LOAD_ATOM(spc, i, am_eparen);
	break;
    case REG_EBRACE:
	i = LOAD_ATOM(spc, i, am_ebrace);
	break;
    case REG_BADBR:
	i = LOAD_ATOM(spc, i, am_badbr);
	break;
    case REG_ERANGE:
	i = LOAD_ATOM(spc, i, am_erange);
	break;
    case REG_ESPACE:
	i = LOAD_ATOM(spc, i, am_espace);
	break;
    case REG_BADRPT:
	i = LOAD_ATOM(spc, i, am_badrpt);
	break;
#ifdef REG_EEND
    case REG_EEND:
	i = LOAD_ATOM(spc, i, am_eend);
	break;
#endif
#ifdef REG_ESIZE
    case REG_ESIZE:
	i = LOAD_ATOM(spc, i, am_esize);
	break;
#endif
#ifdef REG_ERPAREN
    case REG_ERPAREN:
	i = LOAD_ATOM(spc, i, am_erparen);
	break;
#endif
#ifdef REG_EMPTY
    case REG_EMPTY:
	i = LOAD_ATOM(spc, i, am_empty);
	break;
#endif
#ifdef REG_INVARG
    case REG_INVARG:
	i = LOAD_ATOM(spc, i, am_badarg);
	break;
#endif
    default:
	i = LOAD_ATOM(spc, i, am_unknown);
    }
    i = LOAD_TUPLE(spc, i, 2);

    return driver_send_term(d->port, to, spc, i);
}



/* send {P, error, Error} to caller */
static int driver_send_error(Desc *d, ErlDrvTermData *am)
{
    int i = 0;
    ErlDrvTermData to, spec[8];

    to = driver_caller(d->port);

    i = LOAD_PORT(spec, i, d->dport);
    i = LOAD_ATOM(spec, i, am_error);
    i = LOAD_ATOM(spec, i, *am);
    i = LOAD_TUPLE(spec, i, 3);

    return driver_send_term(d->port, to, spec, i);
}


static void posregex_from_erlang(ErlDrvData drv_data, char *buf, int len)
{
    int status;
    unsigned int op = get_int32(buf);
    unsigned int flags = get_int32(buf+4);
    Desc *d = (Desc*) drv_data;

    switch(op) {
    case COMPILE: 
	if (d->compiled) {
	    driver_send_status(d, -1);
	    return;
	}
	status = regcomp(&d->re, buf+8, flags);
	d->compiled = 1;
	driver_send_status(d, status);
	break;
    case MATCH:
	/* just check for match */
	status = regexec(&d->re, buf+8, (size_t) 0, NULL, flags);
	driver_send_status(d, status);
	break;
    case EXEC: {
	status = regexec(&d->re, buf+8, (size_t) 16, &d->pm[0], flags);
	if (status != 0) {
	    driver_send_status(d, status);
	    return;
	}
	driver_send_pm(d);
	return;
    }
    }
}

static void posregex_finish()
{
    return;
}


/*
 * Initialize and return a driver entry struct
 */

DRIVER_INIT(posregex_drv)
{


    am_ok           = driver_mk_atom("ok");
    am_error        = driver_mk_atom("error");
    am_enomem       = driver_mk_atom("enomem");
    am_unknown      = driver_mk_atom("unknown");

    am_nomatch = driver_mk_atom("nomatch");
    am_badpat = driver_mk_atom("badpat");
    am_ecollate = driver_mk_atom("ecollate");
    am_ectype = driver_mk_atom("ectype");
    am_eescape = driver_mk_atom("eescape");
    am_esubreg = driver_mk_atom("esubreg");
    am_ebrack = driver_mk_atom("ebrack");
    am_eparen = driver_mk_atom("eparen");
    am_ebrace = driver_mk_atom("ebrace");
    am_badbr = driver_mk_atom("badbr");
    am_erange = driver_mk_atom("erange");
    am_espace = driver_mk_atom("espace");
    am_badrpt = driver_mk_atom("badrpt");
    am_empty  = driver_mk_atom("empty");
    am_badarg = driver_mk_atom("badarg");

    /* gnu extensions */
    am_eend = driver_mk_atom("eend");
    am_esize = driver_mk_atom("esize");
    am_erparen = driver_mk_atom("erparen");

    posregex_driver_entry.init         = NULL;   /* Not used */
    posregex_driver_entry.start        = posregex_start;
    posregex_driver_entry.stop         = posregex_stop;
    posregex_driver_entry.output       = posregex_from_erlang;
    posregex_driver_entry.ready_input  = NULL;
    posregex_driver_entry.ready_output = NULL;
    posregex_driver_entry.driver_name  = "posregex_drv";
    posregex_driver_entry.finish       = posregex_finish;
    posregex_driver_entry.outputv      = NULL;

    posregex_driver_entry.extended_marker=ERL_DRV_EXTENDED_MARKER;		/* added TOH 1/7/2012 */
    posregex_driver_entry.major_version=ERL_DRV_EXTENDED_MAJOR_VERSION;	/* added TOH 1/7/2012 */
    posregex_driver_entry.minor_version=ERL_DRV_EXTENDED_MINOR_VERSION;	/* added TOH 1/7/2012 */

    return &posregex_driver_entry;
}

