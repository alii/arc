-module(arc_ordered_entries_ffi).
-export([insert/3, next_from/2]).

insert({ordered_entries, Entries, Order, NextSeq}, K, V) ->
    case Entries of
        #{K := {Seq, _}} -> {ordered_entries, Entries#{K := {Seq, V}}, Order, NextSeq};
        _ -> {ordered_entries, Entries#{K => {NextSeq, V}}, Order#{NextSeq => K}, NextSeq + 1}
    end.

%% the first live entry at or after Cursor, with the cursor past it
next_from({ordered_entries, _, _, NextSeq}, Cursor) when Cursor >= NextSeq -> none;
next_from({ordered_entries, Entries, Order, _} = Table, Cursor) ->
    case Order of
        #{Cursor := K} ->
            #{K := {_, V}} = Entries,
            {some, {Cursor + 1, K, V}};
        _ -> next_from(Table, Cursor + 1)
    end.
