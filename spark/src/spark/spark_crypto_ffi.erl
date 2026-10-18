-module(spark_crypto_ffi).
-export([hash/3, hmac/3, names/0]).

%% Length is how many bytes a shake digest should have; other digests have
%% one length and ignore it
hash(Name, Data, Length) ->
    case algorithm(string:lowercase(Name)) of
        {ok, Shake} when Shake =:= shake128; Shake =:= shake256 ->
            {ok, crypto:hash_xof(Shake, Data, 8 * shake_length(Shake, Length))};
        {ok, Algorithm} ->
            {ok, crypto:hash(Algorithm, Data)};
        error ->
            {error, nil}
    end.

shake_length(shake128, none) -> 16;
shake_length(shake256, none) -> 32;
shake_length(_, {some, Length}) -> Length.

hmac(Name, Key, Data) ->
    case algorithm(string:lowercase(Name)) of
        {ok, Shake} when Shake =:= shake128; Shake =:= shake256 ->
            {error, nil};
        {ok, Algorithm} ->
            {ok, crypto:mac(hmac, Algorithm, Key, Data)};
        error ->
            {error, nil}
    end.

%% node's names for the digests, as openssl spells them
names() ->
    [Name || {Name, _} <- table()].

%% Name is in lower case; the usual ones skip the search
algorithm(<<"sha256">>) -> {ok, sha256};
algorithm(<<"sha1">>) -> {ok, sha};
algorithm(<<"md5">>) -> {ok, md5};
algorithm(<<"sha512">>) -> {ok, sha512};
algorithm(Name) -> algorithm(Name, table()).

algorithm(_, []) -> error;
algorithm(Name, [{Known, Algorithm} | Rest]) ->
    case string:lowercase(Known) =:= Name of
        true -> {ok, Algorithm};
        false -> algorithm(Name, Rest)
    end.

table() ->
    [{<<"RSA-MD5">>, md5},
     {<<"RSA-RIPEMD160">>, ripemd160},
     {<<"RSA-SHA1">>, sha},
     {<<"RSA-SHA1-2">>, sha},
     {<<"RSA-SHA224">>, sha224},
     {<<"RSA-SHA256">>, sha256},
     {<<"RSA-SHA3-224">>, sha3_224},
     {<<"RSA-SHA3-256">>, sha3_256},
     {<<"RSA-SHA3-384">>, sha3_384},
     {<<"RSA-SHA3-512">>, sha3_512},
     {<<"RSA-SHA384">>, sha384},
     {<<"RSA-SHA512">>, sha512},
     {<<"RSA-SHA512/224">>, sha512_224},
     {<<"RSA-SHA512/256">>, sha512_256},
     {<<"RSA-SM3">>, sm3},
     {<<"blake2b-512">>, blake2b},
     {<<"blake2b512">>, blake2b},
     {<<"blake2s-256">>, blake2s},
     {<<"blake2s256">>, blake2s},
     {<<"id-rsassa-pkcs1-v1_5-with-sha3-224">>, sha3_224},
     {<<"id-rsassa-pkcs1-v1_5-with-sha3-256">>, sha3_256},
     {<<"id-rsassa-pkcs1-v1_5-with-sha3-384">>, sha3_384},
     {<<"id-rsassa-pkcs1-v1_5-with-sha3-512">>, sha3_512},
     {<<"md5">>, md5},
     {<<"md5WithRSAEncryption">>, md5},
     {<<"ripemd">>, ripemd160},
     {<<"ripemd-160">>, ripemd160},
     {<<"ripemd160">>, ripemd160},
     {<<"ripemd160WithRSA">>, ripemd160},
     {<<"rmd160">>, ripemd160},
     {<<"sha-1">>, sha},
     {<<"sha-224">>, sha224},
     {<<"sha-256">>, sha256},
     {<<"sha-384">>, sha384},
     {<<"sha-512">>, sha512},
     {<<"sha-512/224">>, sha512_224},
     {<<"sha-512/256">>, sha512_256},
     {<<"sha1">>, sha},
     {<<"sha1WithRSAEncryption">>, sha},
     {<<"sha2-224">>, sha224},
     {<<"sha2-256">>, sha256},
     {<<"sha2-384">>, sha384},
     {<<"sha2-512">>, sha512},
     {<<"sha2-512/224">>, sha512_224},
     {<<"sha2-512/256">>, sha512_256},
     {<<"sha224">>, sha224},
     {<<"sha224WithRSAEncryption">>, sha224},
     {<<"sha256">>, sha256},
     {<<"sha256WithRSAEncryption">>, sha256},
     {<<"sha3-224">>, sha3_224},
     {<<"sha3-256">>, sha3_256},
     {<<"sha3-384">>, sha3_384},
     {<<"sha3-512">>, sha3_512},
     {<<"sha384">>, sha384},
     {<<"sha384WithRSAEncryption">>, sha384},
     {<<"sha512">>, sha512},
     {<<"sha512-224">>, sha512_224},
     {<<"sha512-224WithRSAEncryption">>, sha512_224},
     {<<"sha512-256">>, sha512_256},
     {<<"sha512-256WithRSAEncryption">>, sha512_256},
     {<<"sha512WithRSAEncryption">>, sha512},
     {<<"shake-128">>, shake128},
     {<<"shake-256">>, shake256},
     {<<"shake128">>, shake128},
     {<<"shake256">>, shake256},
     {<<"sm3">>, sm3},
     {<<"sm3WithRSAEncryption">>, sm3}].
