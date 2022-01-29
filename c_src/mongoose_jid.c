#include "erl_nif.h"
#include <stdio.h>
#include <string.h>

ERL_NIF_TERM
mk_error(ErlNifEnv* env)
{
    ERL_NIF_TERM ret;
    enif_make_existing_atom(env, "error", &ret, ERL_NIF_LATIN1);
    return ret;
}

static ERL_NIF_TERM
from_binary_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    if(argc != 1) {
        return enif_make_badarg(env);
    }

    const ERL_NIF_TERM arg = argv[0];
    ErlNifBinary bin;
    if (!enif_inspect_binary(env, arg, &bin)) {
        return enif_make_badarg(env);
    }

    const size_t size = bin.size;
    unsigned commercial_at = -1;
    unsigned slash = size;
    for (unsigned i = 0; i < size ; ++i) {
        switch(bin.data[i]) {
            case '/':
                if (slash == size) {
                    slash = i;
                    goto end_loop;
                    // If we found a slash, we don't care about commercial_ats anymore
                    // https://tools.ietf.org/html/rfc7622#section-3.2
                }
                break;
            case '@':
                if (commercial_at == (unsigned)-1) {
                    commercial_at = i;
                } else
                    return mk_error(env);
                break;
        }
    }
end_loop:
    if (commercial_at == 0 || slash == 0) {
        return mk_error(env);
    }

    size_t host_size = slash - commercial_at - 1;
    if (host_size == 0) return mk_error(env);
    ERL_NIF_TERM host = enif_make_sub_binary(env, arg, commercial_at + 1, host_size);

    size_t res_size = slash >= size - 1 ? 0 : size - slash - 1;
    ERL_NIF_TERM resource = enif_make_sub_binary(env, arg, slash + 1, res_size);

    size_t user_size = commercial_at == (unsigned)-1 ? 0 : commercial_at;
    ERL_NIF_TERM user = enif_make_sub_binary(env, arg, 0, user_size);

    return enif_make_tuple3(env, user, host, resource);
}

static ErlNifFunc jid_nif_funcs[] = {
    {"from_binary_nif", 1, from_binary_nif}
};

static int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info) {
    return 0;
};

ERL_NIF_INIT(jid, jid_nif_funcs, NULL, NULL, upgrade, NULL);
