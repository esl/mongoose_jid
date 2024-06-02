-module(jid_gen).
-compile([export_all, nowarn_export_all]).

-include_lib("proper/include/proper.hrl").

jid_type() ->
    oneof([jid_struct(), ljid_struct(), simple_bare_jid_struct()]).

jid_struct() ->
    oneof([
           ?LET({U, S, R}, {username(), domain(), resource()}, {jid, U, S, R}),
           ?LET({U, S}, {username(), domain()}, {jid, U, S, <<>>}),
           ?LET({U, S, R}, {username(), domain(), resource()}, {jid, U, S, R}),
           ?LET({S, R}, {domain(), resource()}, {jid, <<>>, S, R}),
           ?LET(S, domain(), {jid, <<>>, S, <<>>})
          ]).

ljid_struct() ->
    oneof([
           ?LET({U, S, R}, {username(), domain(), resource()}, {U, S, R}),
           ?LET({U, S}, {username(), domain()}, {U, S, <<>>}),
           ?LET({U, S, R}, {username(), domain(), resource()}, {U, S, R}),
           ?LET({S, R}, {domain(), resource()}, {<<>>, S, R})
          ]).

simple_bare_jid_struct() ->
    oneof([
           ?LET({U, S}, {username(), domain()}, {U, S}),
           ?LET(S, domain(), {<<>>, S})
          ]).

from_jid() ->
    oneof([
            {jid_gen:username(), jid_gen:domain(), jid_gen:resource()},
            {<<>>, jid_gen:domain(), jid_gen:resource()},
            {jid_gen:username(), jid_gen:domain()},
            {jid, jid_gen:username(), jid_gen:domain(), jid_gen:resource()}
          ]).

jid() ->
    oneof([full_jid(), bare_jid(), domain(), service()]).

bare_jid() ->
    ?LET({Username, Domain}, {username(), domain()},
         <<Username/binary, $@, Domain/binary>>).

full_jid() ->
    ?LET({BareJid, Resource}, {bare_jid(), resource()},
         <<BareJid/binary, $/, Resource/binary>>).

username() ->
    ?SIZED(S, always_correct_xmpp_binary(S)).

domain() ->
    ?SIZED(S, always_correct_xmpp_binary(round(S*1.5))).

service() ->
    ?LET({Domain, Resource}, {domain(), resource()},
         <<Domain/binary, $/, Resource/binary>>).

resource() ->
    ?SIZED(S, always_correct_xmpp_binary(round(S*1.7))).

maybe_valid_jid() ->
    oneof([jid(), invalid_jid()]).

invalid_jid() ->
    oneof([invalid_full_jid(), invalid_bare_jid()]).

invalid_bare_jid() ->
    %%Oh yes, jids like domain/resource are allowed in both ejabberd and MongooseIM
    ?LET({U, S}, {?SUCHTHAT(E, invalid_username(), size(E) == 1 orelse binary:matches(E, <<"/">>) == []),
                  maybe_valid_domain()},
         <<U/binary, $@, S/binary>>).

invalid_full_jid() ->
    ?LET({BareJid, R}, {invalid_bare_jid(), resource()},
         <<BareJid/binary, $/, R/binary>>).

maybe_valid_username() ->
    oneof([username(), <<>>, invalid_username()]).

invalid_username() ->
    invalid_xmpp_binary(prohibited_output_node()).

maybe_valid_resource() ->
    oneof([resource(), <<>>, invalid_resource()]).

invalid_resource() ->
    invalid_xmpp_binary([<<238,190,187>>]). %<<"\x{EFBB}"/utf8>>

maybe_valid_domain() ->
    oneof([domain(), <<>>, invalid_domain()]).

invalid_domain() ->
    invalid_resource().


always_correct_xmpp_binary(S) ->
    ?LET(Str, always_correct_xmpp_string(S), list_to_binary(Str)).

allowed_output() ->
    oneof([choose($a, $z),
           choose($A, $Z),
           oneof([$., $-, $_]),
           choose($0, $9)]).

always_correct_xmpp_string(S) ->
    [allowed_output() || _ <- lists:seq(1, S)].

invalid_xmpp_binary(ProhibitedOutput) ->
    ?LET({NotAllowed, Str},
         {oneof(ProhibitedOutput),
          frequency([{1, []}, {5, maybe_invalid_xmpp_string(ProhibitedOutput)}])},
         erlang:iolist_to_binary([NotAllowed | Str])).

maybe_invalid_xmpp_string(ProhibitedOutput) ->
      list(
        oneof([allowed_output(),
               oneof(ProhibitedOutput)])).

prohibited_output_node() ->
    [$", $&, $', $/, $:, $<, $>, $@, " "].
