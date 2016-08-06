-module(sha3_tests).

-include_lib("eunit/include/eunit.hrl").

simple_data() ->
    <<16#00112233445566778899AABBCCDDEEFF:128>>.

simple_digest() ->
    <<16#038907E89C919CD8F90A7FBC5A88FF9278108DAEF3EBCDA0CEB383E1:224>>.

simple_test() ->
    Digest = sha3:hash(224, simple_data()),
    Expected = simple_digest(),
    ?assertEqual(Expected, Digest).

update_test() ->
    Context1 = sha3:hash_init(224),
    Context2 = sha3:hash_update(Context1, simple_data()),
    Digest = sha3:hash_final(Context2),
    Expected = simple_digest(),
    ?assertEqual(Expected, Digest).

update_context_test() ->
    Context1 = sha3:hash_init(224),
    Context2 = sha3:hash_update(Context1, simple_data()),
    Context3 = sha3:hash_update(Context1, simple_data()),
    Digest1 = sha3:hash_final(Context2),
    Digest2 = sha3:hash_final(Context3),
    Expected = simple_digest(),
    ?assertEqual(Expected, Digest1),
    ?assertEqual(Expected, Digest2).

hash_224_test() ->
    ?assertEqual(<<16#038907E89C919CD8F90A7FBC5A88FF9278108DAEF3EBCDA0CEB383E1:224>>,
        sha3:hash(224, <<16#00112233445566778899AABBCCDDEEFF:128>>)).

hash_256_test() ->
    ?assertEqual(<<16#22BCE46032802AF0ABFACF3768F7BE04A34F5F01DF60F44FFD52D3CA937350C0:256>>,
        sha3:hash(256, <<16#00112233445566778899AABBCCDDEEFF:128>>)).

hash_384_test() ->
    ?assertEqual(<<16#25FAC1ADECBE1B254976FE32C2FE78829B23D7D84316141ECD208D6806A9DB4352A014ADA4106BA0D210DDA0FD18E150:384>>,
        sha3:hash(384, <<16#00112233445566778899AABBCCDDEEFF:128>>)).

hash_512_test() ->
    ?assertEqual(<<16#94EE7851163C39C3489373AA0BF885D95925EAD7484C586D2E0D01D9C8069D3C30E2EEA2DC63A91B517FE53E43A31D764A2154A2DA92876366B138ABC4406805:512>>,
        sha3:hash(512, <<16#00112233445566778899AABBCCDDEEFF:128>>)).

hexhash_224_test() ->
    ?assertEqual(<<"038907E89C919CD8F90A7FBC5A88FF9278108DAEF3EBCDA0CEB383E1">>,
        sha3:hexhash(224, <<16#00112233445566778899AABBCCDDEEFF:128>>)).

hexhash_256_test() ->
    ?assertEqual(<<"22BCE46032802AF0ABFACF3768F7BE04A34F5F01DF60F44FFD52D3CA937350C0">>,
        sha3:hexhash(256, <<16#00112233445566778899AABBCCDDEEFF:128>>)).

hexhash_384_test() ->
    ?assertEqual(<<"25FAC1ADECBE1B254976FE32C2FE78829B23D7D84316141ECD208D6806A9DB4352A014ADA4106BA0D210DDA0FD18E150">>,
        sha3:hexhash(384, <<16#00112233445566778899AABBCCDDEEFF:128>>)).

hexhash_512_test() ->
    ?assertEqual(<<"94EE7851163C39C3489373AA0BF885D95925EAD7484C586D2E0D01D9C8069D3C30E2EEA2DC63A91B517FE53E43A31D764A2154A2DA92876366B138ABC4406805">>,
        sha3:hexhash(512, <<16#00112233445566778899AABBCCDDEEFF:128>>)).

