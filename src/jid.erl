-module(jid).

-export([make/3, make/1, make_bare/2, make_noprep/3, make_noprep/1]).
-export([from_binary/1, from_binary_noprep/1]).
-export([binary_to_bare/1, to_bare_binary/1, to_binary/1]).
-export([are_equal/2, are_bare_equal/2, is_nodename/1]).
-export([nodeprep/1, nameprep/1, resourceprep/1]).
-export([luser/1, lserver/1, lresource/1]).
-export([to_lower/1, to_lus/1, to_bare/1]).
-export([replace_resource/2, replace_resource_noprep/2]).
-export([str_tolower/1]).

-include("jid.hrl").

-type user()      :: binary().
-type server()    :: binary().
-type resource()  :: binary().
-type luser()     :: binary().
-type lserver()   :: binary().
-type lresource() :: binary().

-type jid() :: #jid{}.
%% JID data structure.
-type ljid() :: {luser(), lserver(), lresource()}.
%% 3-tuple containing the string-prepped binaries.
-type simple_jid() :: {user(), server(), resource()}.
%% 3-tuple containing not-yet string-prepped binaries.
-type simple_bare_jid() :: {luser(), lserver()}.
%% 2-tuple containing string-prepped user and server parts.
-type literal_jid() :: binary().
%% literal binary containing a bare or full JID, not ensured to be string-prepped.

-export_type([jid/0,
              ljid/0,
              simple_bare_jid/0,
              simple_jid/0,
              literal_jid/0,
              user/0, server/0, resource/0,
              luser/0, lserver/0, lresource/0
             ]).

-define(SANE_LIMIT, 1024).
-define(XMPP_JID_SIZE_LIMIT, 3071).
% Maximum JID size in octets (bytes) as defined in
% https://tools.ietf.org/html/rfc7622#section-3.1

%% @doc Takes the user, server, and resource parts, and returns a jid record or an error.
-spec make(User :: user(), Server :: server(), Resource :: resource()) -> jid() | error.
make(User, Server, Resource) ->
    case {nodeprep(User), nameprep(Server), resourceprep(Resource)} of
        {error, _, _} -> error;
        {_, error, _} -> error;
        {_, _, error} -> error;
        {LUser, LServer, LRes} ->
            #jid{luser = LUser,
                 lserver = LServer,
                 lresource = LRes}
    end.

%% @equiv make(User, Server, Resource)
-spec make(simple_jid()) ->  jid() | error.
make({User, Server, Resource}) ->
    make(User, Server, Resource).

%% @doc Takes the user and server parts, and returns a jid record with an empty resource, or an error.
-spec make_bare(User :: user(), Server :: server()) -> jid() | error.
make_bare(User, Server) ->
    case {nodeprep(User), nameprep(Server)} of
        {error, _} -> error;
        {_, error} -> error;
        {LUser, LServer} ->
            #jid{luser = LUser,
                 lserver = LServer,
                 lresource = <<>>}
    end.

%% @doc Creates a jid record without validating the input. Useful when input is already trusted.
-spec make_noprep(User     :: luser(),
                  Server   :: lserver(),
                  Resource :: lresource()) -> jid().
make_noprep(LUser, LServer, LResource) ->
    #jid{luser = LUser,
         lserver = LServer,
         lresource = LResource}.

%% @equiv make_noprep(User, Server, Resource)
-spec make_noprep(simple_jid()) -> jid().
make_noprep({LUser, LServer, LResource}) ->
    make_noprep(LUser, LServer, LResource).

%% @doc Compares jid structures according to the RFC, i.e., only after normalisation
-spec are_equal(jid(), jid()) -> boolean().
are_equal(#jid{luser = LUser, lserver = LServer, lresource = LRes},
          #jid{luser = LUser, lserver = LServer, lresource = LRes}) ->
    true;
are_equal(_, _) ->
    false.

%% @doc Returns true if `are_equal(to_bare(A), to_bare(B))'
-spec are_bare_equal(jid() | ljid(), jid() | ljid()) -> boolean().
are_bare_equal(#jid{luser = LUser, lserver = LServer},
               #jid{luser = LUser, lserver = LServer}) ->
    true;
are_bare_equal(#jid{luser = LUser, lserver = LServer}, {LUser, LServer, _}) ->
    true;
are_bare_equal({LUser, LServer, _}, #jid{luser = LUser, lserver = LServer}) ->
    true;
are_bare_equal({LUser, LServer, _}, {LUser, LServer, _}) ->
    true;
are_bare_equal(_, _) ->
    false.

%% @doc Parses a binary and returns a jid record or an error
-spec from_binary(binary()) -> jid() | error.
from_binary(J) when is_binary(J), byte_size(J) < ?XMPP_JID_SIZE_LIMIT ->
    case binary_to_jid1(J, J, 0) of
        {U, H, R} -> make(U, H, R);
        error -> error
    end;
from_binary(_) ->
    error.

%% @doc Parses a binary and returns a jid record or an error, but without normalisation of the parts
-spec from_binary_noprep(binary()) -> jid() | error.
from_binary_noprep(J) when is_binary(J), byte_size(J) < ?XMPP_JID_SIZE_LIMIT ->
    case binary_to_jid1(J, J, 0) of
        {U, S, R} ->
            #jid{luser = U, lserver = S, lresource = R};
        error -> error
    end;
from_binary_noprep(_) ->
    error.

%% @doc Takes a representation of a jid, and outputs such jid as a literal binary.
-spec to_binary(simple_jid() | simple_bare_jid() | jid() | literal_jid()) -> binary().
to_binary({<<>>, Server, <<>>}) ->
    Server;
to_binary({Node, Server, <<>>}) ->
    <<Node/binary, "@", Server/binary>>;
to_binary({<<>>, Server, Resource}) ->
    <<Server/binary, "/", Resource/binary>>;
to_binary({User, Server, Resource}) ->
    <<User/binary, "@", Server/binary, "/", Resource/binary>>;
to_binary({<<>>, Server}) ->
    <<Server/binary>>;
to_binary({Node, Server}) ->
    <<Node/binary, "@", Server/binary>>;
to_binary(#jid{luser = LUser, lserver = LServer, lresource = LResource}) ->
    to_binary({LUser, LServer, LResource});
to_binary(Jid) when is_binary(Jid) ->
    Jid.

%% @doc Takes a representation of a jid, and outputs such jid as a literal binary in bare form.
-spec to_bare_binary(ljid() | simple_bare_jid() | jid() | literal_jid()) -> binary() | error.
to_bare_binary({<<>>, Server}) ->
    <<Server/binary>>;
to_bare_binary({User, Server}) ->
    <<User/binary, "@", Server/binary>>;
to_bare_binary({<<>>, Server, _}) ->
    <<Server/binary>>;
to_bare_binary({User, Server, _}) ->
    <<User/binary, "@", Server/binary>>;
to_bare_binary(#jid{luser = <<>>, lserver = LServer}) ->
    <<LServer/binary>>;
to_bare_binary(#jid{luser = LUser, lserver = LServer}) ->
    <<LUser/binary, "@", LServer/binary>>;
to_bare_binary(BinJid) when is_binary(BinJid) ->
    case binary_to_bare(BinJid) of
        error -> error;
        Jid -> to_binary(Jid)
    end.

%% @doc Returns true if the input is a valid user part
-spec is_nodename(<<>> | binary()) -> boolean().
is_nodename(<<>>) ->
    false;
is_nodename(J) ->
    error =/= nodeprep(J).

%% @private
-spec validate_binary_size(binary()) -> binary() | error;
                          (error) -> error.
validate_binary_size(R) when is_binary(R), byte_size(R) < ?SANE_LIMIT ->
    R;
validate_binary_size(_) ->
    error.

%% @doc Prepares the user part of a jid
-spec nodeprep(user()) -> luser() | error.
nodeprep(S) when is_binary(S), byte_size(S) < ?SANE_LIMIT ->
    R = stringprep:nodeprep(S),
    validate_binary_size(R);
nodeprep(_) ->
    error.

%% @doc Extract the string-prepped user part of the jid
-spec luser(jid() | ljid() | simple_bare_jid()) -> luser().
luser(#jid{luser = LUser}) ->
    LUser;
luser({LUser, _, _}) ->
    LUser;
luser({LUser, _}) ->
    LUser.

%% @doc Extract the string-prepped server part of the jid
-spec lserver(jid() | ljid() | simple_bare_jid()) -> lserver().
lserver(#jid{lserver = LServer}) ->
    LServer;
lserver({_, LServer, _}) ->
    LServer;
lserver({_, LServer}) ->
    LServer.

%% @doc Extract the string-prepped resource part of the jid
-spec lresource(jid() | ljid() | simple_bare_jid()) -> lresource().
lresource(#jid{lresource = LResource}) ->
    LResource;
lresource({_, _, LResource}) ->
    LResource;
lresource({_, _}) ->
    <<>>.

%% @doc Prepares the server part of a jid
-spec nameprep(server()) -> lserver() | error.
nameprep(<<>>) ->
    error;
nameprep(S) when is_binary(S), byte_size(S) < ?SANE_LIMIT ->
    R = stringprep:nameprep(S),
    validate_binary_size(R);
nameprep(_) ->
    error.

%% @doc Prepares the resource part of a jid
-spec resourceprep(resource()) -> lresource() | error.
resourceprep(S) when is_binary(S), byte_size(S) < ?SANE_LIMIT ->
    R = stringprep:resourceprep(S),
    validate_binary_size(R);
resourceprep(_) ->
    error.

%% @doc Returns a jid that contains only prepared strings
-spec to_lower(simple_jid() | jid()) -> error | ljid().
to_lower(#jid{luser = U, lserver = S, lresource = R}) ->
    {U, S, R};
to_lower({U, S, R}) ->
    case {jid:nodeprep(U), jid:nameprep(S), jid:resourceprep(R)}  of
      {LUser, LServer, LResource} when LUser /= error, LServer /= error, LResource /= error ->
        {LUser, LServer, LResource};
      _Error ->
        error
    end.

%% @doc Takes a jid and returns a prepared bare jid
-spec to_lus(jid() | ljid() | simple_bare_jid()) -> simple_bare_jid();
            (error) -> error.
to_lus(#jid{luser = U, lserver = S}) ->
    {U, S};
to_lus({U, S, _}) ->
    {U, S};
to_lus({U, S}) ->
    {U, S};
to_lus(error) ->
    error.

%% @doc Takes a jid and returns the same jid without its resourcepart
-spec to_bare(jid()) -> jid();
             (ljid()) -> ljid();
             (simple_bare_jid()) -> simple_bare_jid();
             (error) -> error.
to_bare(#jid{} = JID) ->
    JID#jid{lresource = <<>>};
to_bare({U, S, _R}) ->
    {U, S, <<>>};
to_bare({U, S}) ->
    {U, S};
to_bare(error) ->
    error.

%% @doc Replaces the resource part of a jid with a new resource
-spec replace_resource(jid(), resource()) -> jid() | error.
replace_resource(#jid{} = JID, Resource) ->
    case resourceprep(Resource) of
        error -> error;
        LResource ->
            JID#jid{lresource = LResource}
    end.

%% @doc Replaces the resource part of a jid with a new resource, but without normalisation
-spec replace_resource_noprep(jid(), resource()) -> jid().
replace_resource_noprep(#jid{} = JID, LResource) ->
    JID#jid{lresource = LResource}.

%% @equiv jid:to_bare(jid:from_binary(BinaryJid))
-spec binary_to_bare(binary()) -> jid() | error.
binary_to_bare(JID) when is_binary(JID) ->
    case from_binary(JID) of
        error ->
            error;
        #jid{} = Result ->
            to_bare(Result)
    end.

%% @doc Lowercases a string using the stringprep algorithm
-spec str_tolower(iodata()) -> binary() | error.
str_tolower(Val) when is_binary(Val); is_list(Val) ->
    stringprep:tolower(Val).

%%%%% Helpers 
binary_to_jid1(_, <<>>, 0) ->
    error;
binary_to_jid1(J, <<>>, _) ->
    {<<>>, J, <<>>};
binary_to_jid1(_, <<$@, _J/binary>>, 0) ->
    error;
binary_to_jid1(Jid, <<$@, J/binary>>, N) ->
    binary_to_jid2(Jid, J, N, 0);
binary_to_jid1(Jid, <<_, $@, J/binary>>, N) ->
    binary_to_jid2(Jid, J, N + 1, 0);
binary_to_jid1(Jid, <<_, _, $@, J/binary>>, N) ->
    binary_to_jid2(Jid, J, N + 2, 0);
binary_to_jid1(Jid, <<_, _, _, $@, J/binary>>, N) ->
    binary_to_jid2(Jid, J, N + 3, 0);
binary_to_jid1(Jid, <<_, _, _, _, $@, J/binary>>, N) ->
    binary_to_jid2(Jid, J, N + 4, 0);
binary_to_jid1(Jid, <<_, _, _, _, _, $@, J/binary>>, N) ->
    binary_to_jid2(Jid, J, N + 5, 0);
binary_to_jid1(Jid, <<_, _, _, _, _, _, $@, J/binary>>, N) ->
    binary_to_jid2(Jid, J, N + 6, 0);
binary_to_jid1(Jid, <<_, _, _, _, _, _, _, $@, J/binary>>, N) ->
    binary_to_jid2(Jid, J, N + 7, 0);
binary_to_jid1(Jid, <<_, _, _, _, _, _, _, _, $@, J/binary>>, N) ->
    binary_to_jid2(Jid, J, N + 8, 0);
binary_to_jid1(_, <<$/, _J/binary>>, 0) ->
    error;
binary_to_jid1(Jid, <<$/, _/binary>>, N) ->
    {<<>>,
     erlang:binary_part(Jid, {0, N}),
     erlang:binary_part(Jid, {N + 1, byte_size(Jid) - N - 1})};
binary_to_jid1(Jid, <<_, $/, _/binary>>, N) ->
    {<<>>,
     erlang:binary_part(Jid, {0, N + 1}),
     erlang:binary_part(Jid, {N + 2, byte_size(Jid) - N - 2})};
binary_to_jid1(Jid, <<_, _, $/, _/binary>>, N) ->
    {<<>>,
     erlang:binary_part(Jid, {0, N + 2}),
     erlang:binary_part(Jid, {N + 3, byte_size(Jid) - N - 3})};
binary_to_jid1(Jid, <<_, _, _, $/, _/binary>>, N) ->
    {<<>>,
     erlang:binary_part(Jid, {0, N + 3}),
     erlang:binary_part(Jid, {N + 4, byte_size(Jid) - N - 4})};
binary_to_jid1(Jid, <<_, _, _, _, $/, _/binary>>, N) ->
    {<<>>,
     erlang:binary_part(Jid, {0, N + 4}),
     erlang:binary_part(Jid, {N + 5, byte_size(Jid) - N - 5})};
binary_to_jid1(Jid, <<_, _, _, _, _, $/, _/binary>>, N) ->
    {<<>>,
     erlang:binary_part(Jid, {0, N + 5}),
     erlang:binary_part(Jid, {N + 6, byte_size(Jid) - N - 6})};
binary_to_jid1(Jid, <<_, _, _, _, _, _, $/, _/binary>>, N) ->
    {<<>>,
     erlang:binary_part(Jid, {0, N + 6}),
     erlang:binary_part(Jid, {N + 7, byte_size(Jid) - N - 7})};
binary_to_jid1(Jid, <<_, _, _, _, _, _, _, $/, _/binary>>, N) ->
    {<<>>,
     erlang:binary_part(Jid, {0, N + 7}),
     erlang:binary_part(Jid, {N + 8, byte_size(Jid) - N - 8})};
binary_to_jid1(Jid, <<_, _, _, _, _, _, _, _, $/, _/binary>>, N) ->
    {<<>>,
     erlang:binary_part(Jid, {0, N + 8}),
     erlang:binary_part(Jid, {N + 9, byte_size(Jid) - N - 9})};
binary_to_jid1(Jid, <<_, _, _, _, _, _, _, _, J/binary>>, N) ->
    binary_to_jid1(Jid, J, N + 8);
binary_to_jid1(Jid, <<_C, J/binary>>, N) ->
    binary_to_jid1(Jid, J, N + 1).

binary_to_jid2(_, <<>>, _N, 0) ->
    error;
binary_to_jid2(Jid, <<>>, N, S) ->
    {erlang:binary_part(Jid, {0, N}),
     erlang:binary_part(Jid, {N + 1, S}),
     <<>>};
binary_to_jid2(_, <<$@, _J/binary>>, _N, _) ->
    error;
binary_to_jid2(_, <<$/, _J/binary>>, _N, 0) ->
    error;
binary_to_jid2(Jid, <<$/, _/binary>>, N, S) ->
    {erlang:binary_part(Jid, {0, N}),
     erlang:binary_part(Jid, {N + 1, S}),
     erlang:binary_part(Jid, {N + S + 2, byte_size(Jid) - N - S - 2})};
binary_to_jid2(Jid, <<A, "/", _/binary>>, N, S)
  when A =/= $@ ->
    {erlang:binary_part(Jid, {0, N}),
     erlang:binary_part(Jid, {N + 1, S + 1}),
     erlang:binary_part(Jid, {N + S + 3, byte_size(Jid) - N - S - 3})};
binary_to_jid2(Jid, <<A, B, "/", _/binary>>, N, S)
  when A =/= $@, B =/= $@ ->
    {erlang:binary_part(Jid, {0, N}),
     erlang:binary_part(Jid, {N + 1, S + 2}),
     erlang:binary_part(Jid, {N + S + 4, byte_size(Jid) - N - S - 4})};
binary_to_jid2(Jid, <<A, B, C, "/", _/binary>>, N, S)
  when A =/= $@, B =/= $@, C =/= $@ ->
    {erlang:binary_part(Jid, {0, N}),
     erlang:binary_part(Jid, {N + 1, S + 3}),
     erlang:binary_part(Jid, {N + S + 5, byte_size(Jid) - N - S - 5})};
binary_to_jid2(Jid, <<A, B, C, D, "/", _/binary>>, N, S)
  when A =/= $@, B =/= $@, C =/= $@, D =/= $@ ->
    {erlang:binary_part(Jid, {0, N}),
     erlang:binary_part(Jid, {N + 1, S + 4}),
     erlang:binary_part(Jid, {N + S + 6, byte_size(Jid) - N - S - 6})};
binary_to_jid2(Jid, <<A, B, C, D, E, "/", _/binary>>, N, S)
  when A =/= $@, B =/= $@, C =/= $@, D =/= $@, E =/= $@ ->
    {erlang:binary_part(Jid, {0, N}),
     erlang:binary_part(Jid, {N + 1, S + 5}),
     erlang:binary_part(Jid, {N + S + 7, byte_size(Jid) - N - S - 7})};
binary_to_jid2(Jid, <<A, B, C, D, E, F, "/", _/binary>>, N, S)
  when A =/= $@, B =/= $@, C =/= $@, D =/= $@, E =/= $@, F =/= $@ ->
    {erlang:binary_part(Jid, {0, N}),
     erlang:binary_part(Jid, {N + 1, S + 6}),
     erlang:binary_part(Jid, {N + S + 8, byte_size(Jid) - N - S - 8})};
binary_to_jid2(Jid, <<A, B, C, D, E, F, G, "/", _/binary>>, N, S)
  when A =/= $@, B =/= $@, C =/= $@, D =/= $@, E =/= $@, F =/= $@, G =/= $@ ->
    {erlang:binary_part(Jid, {0, N}),
     erlang:binary_part(Jid, {N + 1, S + 7}),
     erlang:binary_part(Jid, {N + S + 9, byte_size(Jid) - N - S - 9})};
binary_to_jid2(Jid, <<A, B, C, D, E, F, G, H, J/binary>>, N, S)
  when A =/= $@, B =/= $@, C =/= $@, D =/= $@, E =/= $@, F =/= $@, G =/= $@, H =/= $@ ->
    binary_to_jid2(Jid, J, N, S + 8);
binary_to_jid2(Jid, <<_C, J/binary>>, N, S) ->
    binary_to_jid2(Jid, J, N, S + 1).
