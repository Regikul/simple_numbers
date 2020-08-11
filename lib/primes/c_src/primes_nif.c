#include "erl_nif.h"
#include <math.h>

#define TRUE (mk_atom(env, "true"))
#define FALSE (mk_atom(env, "false"))

ERL_NIF_TERM mk_atom(ErlNifEnv* env, const char* atom);
ERL_NIF_TERM mk_error(ErlNifEnv* env, const char* mesg);

ERL_NIF_TERM mk_atom(ErlNifEnv* env, const char* atom) {
    ERL_NIF_TERM ret;

    if(!enif_make_existing_atom(env, atom, &ret, ERL_NIF_LATIN1)) {
        return enif_make_atom(env, atom);
    }

    return ret;
}

ERL_NIF_TERM mk_error(ErlNifEnv* env, const char* mesg) {
    return enif_make_tuple2(env, mk_atom(env, "error"), mk_atom(env, mesg));
}

static ERL_NIF_TERM verify(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    unsigned int number = 0;
    int limit = 0;
    int i = 1;

    if(argc != 1) {
        return enif_make_badarg(env);
    }

    if(!enif_is_number(env, argv[0])) {
        return enif_make_badarg(env);
    }

    if(!enif_get_uint(env, argv[0], &number)) {
        return enif_make_badarg(env);
    }

    limit = sqrt(number);

    if (number < 2) {
        return FALSE;
    } else if (number == 2) {
        return TRUE;
    } else while (++i <= limit) {
        if (number % i == 0) {
            return FALSE;
        }
    };
    return TRUE;
}

static ErlNifFunc nif_funcs[] = {
    {"verify", 1, verify}
};

ERL_NIF_INIT(primes, nif_funcs, NULL, NULL, NULL, NULL);