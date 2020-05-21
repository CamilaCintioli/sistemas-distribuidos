-module(client_tests).

-export([when_two_clients_write_to_an_entry_the_last_value_to_be_commited_is_saved/0]).

when_two_clients_write_to_an_entry_the_last_value_to_be_commited_is_saved() ->
    Server = server:start(10),
    Client1 = client:start(Server),
    Client2 = client:start(Server),
    Position = 1,
    Client1Value = client1wrote,
    Client2Value = client2wrote,
    Client1 ! {write, Position, Client1Value},
    Client2 ! {write, Position, Client2Value},
    Client1 ! commit,
    Client2 ! commit,
    timer:sleep(1),
    Client2Value = read(Position, Server).

read(Position, Server) ->
    Handler = server:open(Server),
    Ref = make_ref(),
    Handler ! {read, Ref, Position},
    receive {Ref, Value} -> Value end.
