
for(F, From, To) ->
    for(F, From, To, []).

for(_F, From, To, Acc) when From > To ->
    lists:reverse(Acc);

for(F, From, To, Acc) ->
    for(F, From + 1, To, [F(From) | Acc]).
