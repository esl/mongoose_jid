-module(jid_SUITE).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").
-include("jid.hrl").

-export([all/0, groups/0, init_per_suite/1, end_per_suite/1]).

-export([
         binary_to_jid_succeeds_with_valid_binaries/1,
         binary_to_jid_fails_with_invalid_binaries/1,
         binary_to_jid_fails_with_empty_binary/1,
         binary_noprep_to_jid_succeeds_with_valid_binaries/1,
         binary_noprep_to_jid_fails_with_empty_binary/1,
         make_jid_fails_on_binaries_that_are_too_long/1,
         make_is_independent_of_the_input_format/1,
         make_noprep_and_make_have_equal_raw_jid/1,
         make_noprep_is_independent_of_the_input_format/1,
         jid_to_lower_fails_if_any_binary_is_invalid/1,
         jid_replace_resource_failes_for_invalid_resource/1,
         jid_replace_resource_noprep_failes_for_invalid_resource/1,
         nodeprep_fails_with_too_long_username/1,
         nameprep_fails_with_too_long_domain/1,
         resourceprep_fails_with_too_long_resource/1,
         from_binary_fails_with_too_long_input/1,
         nodeprep_fails_with_incorrect_username/1,
         resourceprep_fails_with_incorrect_resource/1,
         nameprep_fails_with_incorrect_domain/1,
         is_nodename_fails_for_empty_binary/1,
         compare_bare_jids/1,
         compare_bare_jids_doesnt_depend_on_the_order/1,
         compare_bare_with_jids_structs_and_bare_jids/1,
         binary_to_bare_equals_binary_and_then_bare/1,
         to_lower_to_bare_equals_to_bare_to_lower/1,
         make_to_lus_equals_to_lower_to_lus/1
        ]).

-export([
         with_nif_to_binary/1,
         with_nif_from_binary/1
        ]).


all() -> [
          {group, common},
          {group, old_comparison}
         ].

groups() ->
    [
     {common, [parallel], [
                           binary_to_jid_succeeds_with_valid_binaries,
                           binary_to_jid_fails_with_invalid_binaries,
                           binary_to_jid_fails_with_empty_binary,
                           binary_noprep_to_jid_succeeds_with_valid_binaries,
                           binary_noprep_to_jid_fails_with_empty_binary,
                           make_jid_fails_on_binaries_that_are_too_long,
                           make_is_independent_of_the_input_format,
                           make_noprep_and_make_have_equal_raw_jid,
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
                           to_lower_to_bare_equals_to_bare_to_lower,
                           make_to_lus_equals_to_lower_to_lus
                          ]},
     {old_comparison, [parallel], [
                                   with_nif_from_binary,
                                   with_nif_to_binary
                                  ]}
    ].

init_per_suite(C) ->
    {ok, _} = application:ensure_all_started(jid),
    C.

end_per_suite(C) ->
    C.

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

binary_noprep_to_jid_fails_with_empty_binary(_) ->
    error = jid:from_binary(<<>>).

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

make_noprep_and_make_have_equal_raw_jid(_) ->
    Prop = ?FORALL({U, S, R},
                   {jid_gen:username(), jid_gen:domain(), jid_gen:resource()},
                   begin
                       #jid{user = U, server = S, resource = R} = jid:make(U, S, R),
                       #jid{user = U, server = S, resource = R} = jid:make_noprep(U, S, R),
                       true
                   end
                  ),
    run_property(Prop, 10, 1, 500).


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



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Original code kept for documentation purposes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

with_nif_to_binary(_C) ->
    Prop = ?FORALL(L, jid_gen:jid(), from_binary(L) =:= jid:from_binary(L)),
    run_property(Prop, 100, 1, 42).

with_nif_from_binary(_C) ->
    Prop = ?FORALL(L, jid_gen:from_jid(), to_binary(L) =:= jid:to_binary(L)),
    run_property(Prop, 100, 1, 42).

% Some property based testing to check for equivalence
% Original code
-spec from_binary(binary()) ->  error | jid:jid().
from_binary(J) ->
    binary_to_jid1(J, []).

-spec binary_to_jid1(binary(), [byte()]) -> 'error' | jid:jid().
binary_to_jid1(<<$@, _J/binary>>, []) ->
    error;
binary_to_jid1(<<$@, J/binary>>, N) ->
    binary_to_jid2(J, lists:reverse(N), []);
binary_to_jid1(<<$/, _J/binary>>, []) ->
    error;
binary_to_jid1(<<$/, J/binary>>, N) ->
    binary_to_jid3(J, [], lists:reverse(N), []);
binary_to_jid1(<<C, J/binary>>, N) ->
    binary_to_jid1(J, [C | N]);
binary_to_jid1(<<>>, []) ->
    error;
binary_to_jid1(<<>>, N) ->
    jid:make(<<>>, list_to_binary(lists:reverse(N)), <<>>).

%% @doc Only one "@" is admitted per JID
-spec binary_to_jid2(binary(), [byte()], [byte()]) -> 'error' | jid:jid().
binary_to_jid2(<<$@, _J/binary>>, _N, _S) ->
    error;
binary_to_jid2(<<$/, _J/binary>>, _N, []) ->
    error;
binary_to_jid2(<<$/, J/binary>>, N, S) ->
    binary_to_jid3(J, N, lists:reverse(S), []);
binary_to_jid2(<<C, J/binary>>, N, S) ->
    binary_to_jid2(J, N, [C | S]);
binary_to_jid2(<<>>, _N, []) ->
    error;
binary_to_jid2(<<>>, N, S) ->
    jid:make(list_to_binary(N), list_to_binary(lists:reverse(S)), <<>>).

-spec binary_to_jid3(binary(), [byte()], [byte()], [byte()]) -> 'error' | jid:jid().
binary_to_jid3(<<C, J/binary>>, N, S, R) ->
    binary_to_jid3(J, N, S, [C | R]);
binary_to_jid3(<<>>, N, S, R) ->
    jid:make(list_to_binary(N), list_to_binary(S), list_to_binary(lists:reverse(R))).

-spec to_binary(jid:simple_jid() | jid:simple_bare_jid() | jid:jid()) ->  binary().
to_binary(Jid) when is_binary(Jid) ->
    % sometimes it is used to format error messages
    Jid;
to_binary(#jid{user = User, server = Server, resource = Resource}) ->
    to_binary({User, Server, Resource});
to_binary({User, Server}) ->
    to_binary({User, Server, <<>>});
to_binary({Node, Server, Resource}) ->
    S1 = case Node of
             <<>> ->
                 <<>>;
             _ ->
                 <<Node/binary, "@">>
         end,
    S2 = <<S1/binary, Server/binary>>,
    S3 = case Resource of
             <<>> ->
                 S2;
             _ ->
                 <<S2/binary, "/", Resource/binary>>
         end,
    S3.


%% HELPERS
run_property(Prop, NumTest, StartSize, StopSize) ->
    ?assert(proper:quickcheck(Prop, [verbose, long_result,
                                     {numtests, NumTest},
                                     {start_size, StartSize},
                                     {max_size, StopSize}])).
