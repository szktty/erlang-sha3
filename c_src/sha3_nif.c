#include "erl_nif.h"
#include "KeccakNISTInterface.h"

typedef struct nif_hash_context     nif_hash_context;

struct nif_hash_context {
  int bitlen;
  hashState state;
};

static void sha3_resource_cleanup(ErlNifEnv* env, void* arg);
static ERL_NIF_TERM nif_hash_init(ErlNifEnv* env, int argc,
    const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_hash_update(ErlNifEnv* env, int argc,
    const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_hash_final(ErlNifEnv* env, int argc,
    const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_hash(ErlNifEnv* env, int argc,
    const ERL_NIF_TERM argv[]);

static ErlNifFunc nif_funcs[] =
{
  {"hash_init", 1, nif_hash_init},
  {"hash_update", 2, nif_hash_update},
  {"hash_final", 1, nif_hash_final},
  {"hash", 2, nif_hash}
};

static ErlNifResourceType *sha3_resource_type;

static void
sha3_resource_cleanup(ErlNifEnv* env, void* arg)
{
  /* do nothing */
}

static ERL_NIF_TERM
nif_hash_init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM ctxt_term;
  nif_hash_context *ctxt;
  int bitlen;

  if (!enif_get_int(env, argv[0], &bitlen))
    return 0;

  if (bitlen != 224 && bitlen != 256 && bitlen != 384 && bitlen != 512)
    return 0;

  ctxt = enif_alloc_resource(sha3_resource_type, sizeof(nif_hash_context));
  ctxt->bitlen = bitlen;
  Init(&ctxt->state, bitlen);
  ctxt_term = enif_make_resource(env, ctxt);
  enif_release_resource(ctxt);

  return ctxt_term;
}

static ERL_NIF_TERM
nif_hash_update(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM ctxt_term;
  ErlNifBinary src_bin;
  nif_hash_context *ctxt, *new;
  hashState state;

  if (!enif_get_resource(env, argv[0], sha3_resource_type, (void **)&ctxt) ||
      !enif_inspect_binary(env, argv[1], &src_bin))
    return 0;

  state = ctxt->state;
  Update(&state, src_bin.data, src_bin.size * 8);
  new = enif_alloc_resource(sha3_resource_type, sizeof(nif_hash_context));
  new->bitlen = ctxt->bitlen;
  new->state = state;
  ctxt_term = enif_make_resource(env, new);
  enif_release_resource(new);

  return ctxt_term;
}

static ERL_NIF_TERM
nif_hash_final(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM digest_term;
  ErlNifBinary digest_bin;
  nif_hash_context *ctxt;
  hashState state;

  if (!enif_get_resource(env, argv[0], sha3_resource_type, (void **)&ctxt))
    return 0;

  state = ctxt->state;
  enif_alloc_binary(ctxt->bitlen / 8, &digest_bin);
  Final(&state, digest_bin.data);
  digest_term = enif_make_binary(env, &digest_bin);
  enif_release_binary(&digest_bin);

  return digest_term;
}

static ERL_NIF_TERM
nif_hash(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM digest_term;
  ErlNifBinary src_bin, digest_bin;
  int bitlen;

  if (!enif_get_int(env, argv[0], &bitlen) ||
      !enif_inspect_binary(env, argv[1], &src_bin))
    return 0;

  if (bitlen != 224 && bitlen != 256 && bitlen != 384 && bitlen != 512)
    return 0;

  enif_alloc_binary(bitlen / 8, &digest_bin);
  Hash(bitlen, src_bin.data, src_bin.size * 8, digest_bin.data);
  digest_term = enif_make_binary(env, &digest_bin);
  enif_release_binary(&digest_bin);

  return digest_term;
}

static int
on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
  ErlNifResourceFlags flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;

  sha3_resource_type = enif_open_resource_type(env, NULL, "sha3_resource",
      &sha3_resource_cleanup, flags, NULL);
  if (sha3_resource_type == NULL)
    return -1;
  else
    return 0;
}

static void
on_unload(ErlNifEnv* env, void* priv_data)
{
  /* do nothing */
}

ERL_NIF_INIT(sha3, nif_funcs, &on_load, NULL, NULL, &on_unload);

