-module(sha3).

-export([hash_init/1, hash_update/2, hash_final/1, hash/2]).

-export([hexhash/2]).

-on_load(init/0).

-type bitlen() :: 224 | 256 | 384 | 512.

-type context() :: binary().
%% State of hash operation return.

-type digest() :: <<_:224>> | <<_:256>> | <<_:384>> | <<_:512>>.

-export_type([bitlen/0, context/0, digest/0]).

-define(nif_stub, nif_stub_error(?LINE)).
nif_stub_error(Line) ->
    erlang:nif_error({nif_not_loaded,module,?MODULE,line,Line}).

init() ->
    PrivDir = case code:priv_dir(?MODULE) of
                  {error, bad_name} ->
                      EbinDir = filename:dirname(code:which(?MODULE)),
                      AppPath = filename:dirname(EbinDir),
                      filename:join(AppPath, "priv");
                  Path ->
                      Path
              end,
    erlang:load_nif(filename:join(PrivDir, sha3_nif), 0).

%% @doc Returns a new context for hash operation.
%%      Bit length of digest (`BitLen') must be one of 224, 256, 384 and 512.
%% @see hash_update/2
-spec hash_init(bitlen()) -> context().
hash_init(_BitLen) ->
    ?nif_stub.

%% @doc Updates the digest by `Context' generated with `hash_init/1'
%%      using the given `Data' and returns a new updated context.
%%      `Data' can be any length.
%%      The returned context can e used `hash_update/2' or `hash_final/1'.
%% @see hash_final/1
-spec hash_update(context(), binary()) -> context().
hash_update(_Context, _Binary) ->
    ?nif_stub.

%% @doc Finalizes the hash operation with `Context' and
%%      returns a message digest.
%%      Length of the digest is determined by an argument of `hash_init/1'.
-spec hash_final(context()) -> digest().
hash_final(_Context) ->
    ?nif_stub.

%% @doc Computes a message digest from `Binary'.
%%      Bit length of digest (`BitLen') must be one of 224, 256, 384 and 512.
-spec hash(bitlen(), binary()) -> digest().
hash(_BitLen, _Binary) ->
    ?nif_stub.

-spec hexhash(bitlen(), binary()) -> binary().
hexhash(Bitlen, Binary) ->
    Hash = hash(Bitlen, Binary),
    list_to_binary(hex2bin:bin_to_hexstr(Hash)).
