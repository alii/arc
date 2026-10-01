-module(arc_ordered_entries_ffi).
-export([next_from/2]).

%% the first live entry at or after Cursor, with the cursor past it
next_from({ordered_entries, _, _, NextSeq}, Cursor) when Cursor >= NextSeq -> none;
next_from({ordered_entries, Entries, Order, _} = Table, Cursor) ->
    case Order of
        #{Cursor := K} ->
            #{K := {_, V}} = Entries,
            {some, {Cursor + 1, K, V}};
        _ -> next_from(Table, Cursor + 1)
    end.
