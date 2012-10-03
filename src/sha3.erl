-module(sha3).

-export([hash_init/1, hash_update/2, hash_final/1, hash/2]).

-on_load(init/0).

-type bitlen() :: 224 | 256 | 384 | 512.
-type context() :: binary().
-type digest() :: <<_:224>> | <<_:256>> | <<_:384>> | <<_:512>>.

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

-spec hash_init(bitlen()) -> context().
hash_init(_BitLen) ->
    ?nif_stub.

-spec hash_update(context(), binary()) -> context().
hash_update(_Context, _Binary) ->
    ?nif_stub.

-spec hash_final(context()) -> digest().
hash_final(_Context) ->
    ?nif_stub.

-spec hash(bitlen(), binary()) -> digest().
hash(_BitLen, _Binary) ->
    ?nif_stub.

