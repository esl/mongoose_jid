-module(jid_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("jid.hrl").

-export([all/0, groups/0, init_per_suite/1, end_per_suite/1]).

all() -> [
          {group, common}
         ].

groups() ->
    [
     {common, [parallel],
      [
       empty_server_fails,
       multiple_at_fails_as_invalid_jid,
       to_binary_with_binary_does_nothing,
       to_binary_can_convert_all_types_of_jids,
       binary_to_jid_succeeds_with_valid_binaries,
       binary_to_jid_fails_with_invalid_binaries,
       binary_to_jid_fails_with_empty_binary,
       binary_noprep_to_jid_succeeds_with_valid_binaries,
       binary_noprep_to_jid_fails_with_invalid_binaries,
       binary_noprep_to_jid_fails_with_empty_binary,
       make_jid_fails_on_binaries_that_are_too_long,
       make_is_independent_of_the_input_format,
       make_noprep_is_independent_of_the_input_format,
       jid_to_lower_fails_if_any_binary_is_invalid,
       jid_replace_resource_failes_for_invalid_resource,
       jid_replace_resource_noprep_failes_for_invalid_resource,
       nodeprep_fails_with_too_long_username,
       nameprep_fails_with_too_long_domain,
       resourceprep_fails_with_too_long_resource,
       from_binary_fails_with_too_long_input,
       nodeprep_fails_with_incorrect_username,
       resourceprep_fails_with_incorrect_resource,
       nameprep_fails_with_incorrect_domain,
       is_nodename_fails_for_empty_binary,
       compare_bare_jids,
       compare_bare_jids_doesnt_depend_on_the_order,
       compare_bare_with_jids_structs_and_bare_jids,
       binary_to_bare_equals_binary_and_then_bare,
       to_bare_binary_equals_to_bare_then_to_binary,
       to_bare_binary_fails_on_invalid_jids,
       to_lower_to_bare_equals_to_bare_to_lower,
       make_to_lus_equals_to_lower_to_lus,
       make_bare_like_make_with_empty_resource,
       make_empty_domain,
       make_bare_empty_domain,
       make_noprep_empty_domain,
       getters_equal_struct_lookup,
       equivalent_good,
       equivalent_bad
      ]}
    ].

init_per_suite(C) ->
    {ok, _} = application:ensure_all_started(jid),
    C.

end_per_suite(C) ->
    C.

empty_server_fails(_C) ->
    ?assertEqual(error, jid:from_binary(<<"$@/">>)).

multiple_at_fails_as_invalid_jid(_C) ->
    ?assertEqual(error, jid:from_binary(<<"jids@have@no@feelings">>)).

to_binary_with_binary_does_nothing(_C) ->
    Prop = ?FORALL(Bin, binary(),
                   Bin =:= jid:to_binary(Bin)),
    run_property(Prop, 50, 1, 100).

to_binary_can_convert_all_types_of_jids(_C) ->
    Prop = ?FORALL(BinJid, (jid_gen:jid_type()),
                   is_binary(jid:to_binary(BinJid))),
    run_property(Prop, 200, 1, 100).

binary_to_jid_succeeds_with_valid_binaries(_C) ->
    Prop = ?FORALL(BinJid, (jid_gen:jid()),
                   (is_record(jid:from_binary(BinJid), jid))),
    run_property(Prop, 200, 1, 100).

binary_to_jid_fails_with_invalid_binaries(_C) ->
    Prop = ?FORALL(BinJid, jid_gen:invalid_jid(),
                   error == jid:from_binary(BinJid)),
    run_property(Prop, 200, 1, 100).

binary_to_jid_fails_with_empty_binary(_) ->
    error = jid:from_binary(<<>>).

binary_noprep_to_jid_succeeds_with_valid_binaries(_) ->
    Prop = ?FORALL(BinJid, (jid_gen:jid()),
                   (is_record(jid:from_binary_noprep(BinJid), jid))),
    run_property(Prop, 200, 1, 100).

binary_noprep_to_jid_fails_with_invalid_binaries(_C) ->
    Prop = ?FORALL(Bin, jid_gen:jid(),
                   error == jid:from_binary_noprep(Bin)),
    run_property(Prop, 5, 3071, 5048).

binary_noprep_to_jid_fails_with_empty_binary(_) ->
    error = jid:from_binary_noprep(<<>>).

make_jid_fails_on_binaries_that_are_too_long(_) ->
    Prop = ?FORALL({U, S, R},
                   {jid_gen:username(), jid_gen:domain(), jid_gen:resource()},
                   case element_length_is_too_big([U,S,R]) of
                        true -> error == jid:make(U,S,R);
                        false -> is_record(jid:make(U,S,R), jid)
                   end),
    run_property(Prop, 50, 500, 1500).

make_is_independent_of_the_input_format(_) ->
    Prop = ?FORALL({U, S, R},
                   {jid_gen:username(), jid_gen:domain(), jid_gen:resource()},
                   jid:make(U,S,R) == jid:make({U,S,R})),
    run_property(Prop, 100, 1, 500).

make_noprep_is_independent_of_the_input_format(_) ->
    Prop = ?FORALL({U, S, R},
                   {jid_gen:username(), jid_gen:domain(), jid_gen:resource()},
                   jid:make_noprep(U,S,R) == jid:make_noprep({U,S,R})),
    run_property(Prop, 10, 1, 500).

element_length_is_too_big(Els) ->
    lists:any(fun(El) -> size(El) >= 1024 end, Els).

jid_to_lower_fails_if_any_binary_is_invalid(_) ->
    Prop = ?FORALL({U, S, R},
                   {jid_gen:maybe_valid_username(), jid_gen:maybe_valid_domain(), jid_gen:maybe_valid_resource()},
                   case jid:to_lower({U, S, R}) of
                       {LU, LS, LR} ->
                           jid:nodeprep(U) == LU andalso
                           jid:nameprep(S) == LS andalso
                           jid:resourceprep(R) == LR;
                       error ->
                           jid:nodeprep(U) == error orelse
                           jid:nameprep(S) == error orelse
                           jid:resourceprep(R) == error
                   end),

    run_property(Prop, 150, 1, 42).

nodeprep_fails_with_too_long_username(_C) ->
    Prop = ?FORALL(Bin, jid_gen:username(),
                   error == jid:nodeprep(Bin)),
    run_property(Prop, 10, 1024, 2048).

nameprep_fails_with_too_long_domain(_C) ->
    Prop = ?FORALL(Bin, jid_gen:domain(),
                   error == jid:nameprep(Bin)),
    run_property(Prop, 10, 1024, 2048).

resourceprep_fails_with_too_long_resource(_C) ->
    Prop = ?FORALL(Bin, jid_gen:resource(),
                   error == jid:resourceprep(Bin)),
    run_property(Prop, 10, 1024, 2048).

from_binary_fails_with_too_long_input(_C) ->
    Prop = ?FORALL(Bin, jid_gen:jid(),
                   error == jid:from_binary(Bin)),
    run_property(Prop, 5, 3071, 5048).

jid_replace_resource_failes_for_invalid_resource(_) ->
    Prop = ?FORALL({BinJid, MaybeCorrectRes},
                   {jid_gen:bare_jid(), jid_gen:maybe_valid_resource()},
                   jid_replace_resource(BinJid, MaybeCorrectRes)),
    run_property(Prop, 100, 1, 42).

jid_replace_resource_noprep_failes_for_invalid_resource(_) ->
    Prop = ?FORALL({BinJid, MaybeCorrectRes},
                   {jid_gen:bare_jid(), jid_gen:resource()},
                   jid_replace_resource_noprep(BinJid, MaybeCorrectRes)),
    run_property(Prop, 100, 1, 42).

jid_replace_resource_noprep(BinJid, Res) ->
    Jid = jid:from_binary(BinJid),
    Jid2 = jid:replace_resource_noprep(Jid, Res),
    check_jid_replace_resource_output(Res, Jid2).

jid_replace_resource(BinJid, Res) ->
    Jid = jid:from_binary(BinJid),
    Jid2 = jid:replace_resource(Jid, Res),
    check_jid_replace_resource_output(Res, Jid2).

check_jid_replace_resource_output(Resource, error) ->
    jid:resourceprep(Resource) == error;
check_jid_replace_resource_output(Resource, #jid{}) ->
    jid:resourceprep(Resource) =/= error.


nodeprep_fails_with_incorrect_username(_) ->
    Prop = ?FORALL(Bin, jid_gen:invalid_username(),
                   error == jid:nodeprep(Bin)),
    run_property(Prop, 100, 1, 500).

resourceprep_fails_with_incorrect_resource(_) ->
    Prop = ?FORALL(Bin, jid_gen:invalid_resource(),
                   error == jid:resourceprep(Bin)),
    run_property(Prop, 100, 1, 500).

nameprep_fails_with_incorrect_domain(_) ->
    Prop = ?FORALL(Bin, jid_gen:invalid_domain(),
                   error == jid:nameprep(Bin)),
    run_property(Prop, 100, 1, 500).

is_nodename_fails_for_empty_binary(_) ->
    false = jid:is_nodename(<<>>).

compare_bare_jids_doesnt_depend_on_the_order(_) ->
    Prop = ?FORALL(Val,
                   oneof([
                          {jid_gen:username(), jid_gen:domain(), jid_gen:resource()},
                          jid_gen:maybe_valid_jid()
                         ]),
                   begin
                       {AA, BB} = case Val of
                                      A when is_tuple(A) -> {A, A};
                                      X when is_binary(X) -> {jid:from_binary(X), jid:from_binary(X)}
                                  end,
                       equals(jid:are_bare_equal(BB, AA),
                              jid:are_bare_equal(AA, BB))
                   end),
    run_property(Prop, 200, 1, 100).

compare_bare_with_jids_structs_and_bare_jids(_) ->
    Prop = ?FORALL({U, S, R}, {jid_gen:username(), jid_gen:domain(), jid_gen:resource()},
                   begin
                       AA = {U, S, R},
                       BB = jid:make_noprep(U, S, R),
                       equals(jid:are_bare_equal(BB, AA),
                              jid:are_bare_equal(AA, BB))
                   end),
    run_property(Prop, 200, 1, 100).

compare_bare_jids(_) ->
    Prop = ?FORALL(Val, oneof([
                               {jid_gen:maybe_valid_jid(), jid_gen:maybe_valid_jid()},
                               jid_gen:maybe_valid_jid()
                              ]),
                   begin
                       {A, B} = case Val of {X, Y} -> {X, Y}; X -> {X, X} end,
                       AA = jid:from_binary(A),
                       BB = jid:from_binary(B),
                       equals(jid:are_equal(jid:to_bare(AA), jid:to_bare(BB)),
                              jid:are_bare_equal(AA, BB))
                   end),
    run_property(Prop, 200, 1, 100).

binary_to_bare_equals_binary_and_then_bare(_) ->
    Prop = ?FORALL(A, jid_gen:maybe_valid_jid(),
                   equals(jid:to_bare(jid:from_binary(A)), jid:binary_to_bare(A))),
    run_property(Prop, 200, 1, 100).

to_bare_binary_equals_to_bare_then_to_binary(_) ->
    Prop = ?FORALL(A, jid_gen:jid_type(),
                   equals(jid:to_binary(jid:to_bare(A)), jid:to_bare_binary(A))),
    run_property(Prop, 200, 1, 100).

to_bare_binary_fails_on_invalid_jids(_) ->
    ?assertEqual(<<"a@a">>, jid:to_bare_binary(<<"a@a/a">>)),
    ?assertEqual(error, jid:to_bare_binary(<<"aa@/aa">>)).

to_lower_to_bare_equals_to_bare_to_lower(_) ->
    Prop = ?FORALL(JID, oneof([jid_gen:jid_struct(),
                                     {jid_gen:maybe_valid_username(),
                                      jid_gen:maybe_valid_domain(),
                                      jid_gen:resource()}]),
                   equals(jid:to_bare(jid:to_lower(JID)),
                          jid:to_lower(jid:to_bare(JID)))),
    run_property(Prop, 200, 1, 100).

make_to_lus_equals_to_lower_to_lus(_) ->
    Prop = ?FORALL({U, S, R}, {jid_gen:maybe_valid_username(),
                               jid_gen:maybe_valid_domain(),
                               jid_gen:maybe_valid_resource()},
                   equals(jid:to_lus(jid:make(U, S, R)),
                          jid:to_lus(jid:to_lower({U, S, R})))),
    run_property(Prop, 200, 1, 100).

make_bare_like_make_with_empty_resource(_) ->
    Prop = ?FORALL({U, S}, {jid_gen:maybe_valid_username(),
                            jid_gen:maybe_valid_domain()},
                   equals(jid:make_bare(U, S),
                          jid:make(U, S, <<>>))),
    run_property(Prop, 200, 1, 100).

make_empty_domain(_) ->
    ?assertEqual(error, jid:make(jid_gen:username(), <<>>, jid_gen:resource())),
    ?assertEqual(error, jid:make({jid_gen:username(), <<>>, jid_gen:resource()})).

make_bare_empty_domain(_) ->
    ?assertEqual(error, jid:make_bare(jid_gen:username(), <<>>)).

make_noprep_empty_domain(_) ->
    Prop = ?FORALL({U, S, R},
                   {jid_gen:username(), <<>>, jid_gen:resource()},
                    jid:make_noprep(U, S, R) == jid:make_noprep({U, S, R})),
    run_property(Prop, 10, 1, 500).

getters_equal_struct_lookup(_) ->
    Jid = jid:from_binary(<<"alice@localhost/res1">>),
    ?assertEqual(jid:luser(Jid), Jid#jid.luser),
    ?assertEqual(jid:lserver(Jid), Jid#jid.lserver),
    ?assertEqual(jid:lresource(Jid), Jid#jid.lresource),
    LJid = jid:to_lower(Jid),
    ?assertEqual(jid:luser(LJid), Jid#jid.luser),
    ?assertEqual(jid:lserver(LJid), Jid#jid.lserver),
    ?assertEqual(jid:lresource(LJid), Jid#jid.lresource),
    US = jid:to_lus(Jid),
    ?assertEqual(jid:luser(US), Jid#jid.luser),
    ?assertEqual(jid:lserver(US), Jid#jid.lserver),
    ?assertEqual(jid:lresource(US), <<>>).

equivalent_good(_C) ->
    Prop = ?FORALL(Jid, jid_gen:jid(), from_binary(Jid) =:= jid:from_binary(Jid)),
    run_property(Prop, 500, 1, 100).

equivalent_bad(_C) ->
    Prop = ?FORALL(Jid, jid_gen:invalid_jid(), from_binary(Jid) =:= jid:from_binary(Jid)),
    run_property(Prop, 500, 1, 100).

%% HELPERS
run_property(Prop, NumTest, StartSize, StopSize) ->
    Res = proper:quickcheck(Prop, [verbose, long_result,
                                   {numtests, NumTest},
                                   {start_size, StartSize},
                                   {max_size, StopSize}]),
    ct:pal("Result of the property is ~p~n", [Res]),
    ?assert(Res).

%% Original code
from_binary(J) when is_binary(J), byte_size(J) < 3100 ->
    case binary_to_jid1(J, J, 0) of
        {U, H, R} -> jid:make(U, H, R);
        error -> error
    end;
from_binary(_) ->
    error.

binary_to_jid1(_, <<$@, _J/binary>>, 0) ->
    error;
binary_to_jid1(Jid, <<$@, J/binary>>, N) ->
    binary_to_jid2(Jid, J, N, 0);
binary_to_jid1(_, <<$/, _J/binary>>, 0) ->
    error;
binary_to_jid1(Jid, <<$/, _/binary>>, N) ->
    {<<>>,
     erlang:binary_part(Jid, {0, N}),
     erlang:binary_part(Jid, {N + 1, byte_size(Jid) - N - 1})};
binary_to_jid1(Jid, <<_C, J/binary>>, N) ->
    binary_to_jid1(Jid, J, N + 1);
binary_to_jid1(_, <<>>, 0) ->
    error;
binary_to_jid1(J, <<>>, _) ->
    {<<>>, J, <<>>}.

binary_to_jid2(_, <<$@, _J/binary>>, _N, _S) ->
    error;
binary_to_jid2(_, <<$/, _J/binary>>, _N, 0) ->
    error;
binary_to_jid2(Jid, <<$/, _/binary>>, N, S) ->
    {erlang:binary_part(Jid, {0, N}),
     erlang:binary_part(Jid, {N + 1, S}),
     erlang:binary_part(Jid, {N + S + 2, byte_size(Jid) - N - S - 2})};
binary_to_jid2(Jid, <<_C, J/binary>>, N, S) ->
    binary_to_jid2(Jid, J, N, S + 1);
binary_to_jid2(_, <<>>, _N, 0) ->
    error;
binary_to_jid2(Jid, <<>>, N, S) ->
    {erlang:binary_part(Jid, {0, N}),
     erlang:binary_part(Jid, {N + 1, S}),
     <<>>}.
