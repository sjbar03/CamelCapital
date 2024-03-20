type t
(** An alias for the type [Column]. An algebraic data type consisting of
    IntColumn, StringColumn, and FloatColumn.*)

exception WrongFunction
(** [WrongFunction] is raised when a column function is used on a column and/or
    an entry that does not agree with the type of the entries stored in that
    column.*)

val empty_int_col : t
(** [empty_int_col] is the empty int column. Its title is the empty string.*)

val empty_string_col : t
(** [empty_string_col] is the empty string column. Its title is the empty
    string.*)

val empty_float_col : t
(** [empty_float_col] is the empty float column. Its title is the empty string.*)

val title : t -> string
(** [title col] is the string title assigned to column [col]*)

val get_entry_at_index : t -> int -> string
(** [get_entry_at_index col index] is the string representation of the entry at
    [index] in [col]. *)

val col_length : t -> int
(** [col_length col] is the number of entries in [col]*)

val create_column_from_list : string list -> t
(** [create_column_from_list data] creates a column using the first entry of
    [data] as the title and the tail of [data] as the body. If the first entry
    is the string representation of an int, an IntColumn is made, if the first
    entry is the string representation of a float, a FloatColumn is made, and a
    StringColumn is made otherwise. *)

val map_int_col : (int -> int) -> t -> t
(** [map_int_col func col] is the column created by applying function [func] to
    every entry in [col]. Requires: [col] is an IntColumn *)

val map_float_col : (float -> float) -> t -> t
(** [map_float_col func col] is the column created by applying [func] to every
    entry in [col]. Requires: [col] is a FloatColumn. *)

val map_string_col : (string -> string) -> t -> t
(** [map_string_col func col] is the column created by applying [func] to every
    entry in [col]. Requires: [col] is a StringColumn. *)

val equals : t -> t -> bool
(** [equals col1 col2] is true if [col1] and [col2] have structurally equivalent
    titles and bodies *)

val remove_entry : t -> int -> t
(** [remove_entry col index] is the column that is created when the entry at
    [index] in [col] is removed.*)

val filter_indices_int :
  t -> ?acc:int list -> ?index:int -> (int option -> bool) -> int list
(** [filter_indices_int col crit_func] is a sorted list of indicies
    corresponding to the entries in [col] that are false under the criteria
    function [crit_func]. Requires: [col] is an IntColumn. Raises:
    [WrongFunction] if [col] is not an IntColumn. *)

val filter_indices_string :
  t -> ?acc:int list -> ?index:int -> (string option -> bool) -> int list
(** [filter_indices_string col crit_func] is a sorted list of indicies
    corresponding to the entries in [col] that are false under the criteria
    function [crit_func]. Requires: [col] is a StringColumn. Raises:
    [WrongFunction] if [col] is not a StringColumn.*)

val filter_indices_float :
  t -> ?acc:int list -> ?index:int -> (float option -> bool) -> int list
(** [filter_indices_float col crit_func] is a sorted list of indicies
    corresponding to the entries in [col] that are false under the criteria
    function [crit_func]. Requires: [col] is a FloatColumn. Raises:
    [WrongFunction] if [col] is not a FloatColumn.*)

val batch_remove_entries : t -> int list -> t
(** [batch_remove_entries col indices] is the column created by removing the
    entries corresponding to the index integers in [indices]. Requires:
    [indices] is sorted in ascending order.*)

val reduce_int_col : t -> ('a -> int option -> 'a) -> 'a -> 'a
(** [reduce_int_col col func init] is the result of folding left over the body
    of [col] with [func] and [init]. Requires: [col] is an IntColumn. Raises:
    [WrongFunction] if [col] is not an IntColumn. *)

val reduce_float_col : t -> ('a -> float option -> 'a) -> 'a -> 'a
(** [reduce_float_col col func init] is the result of folding left over the body
    of [col] with [func] and [init]. Requires: [col] is a FloatColumn. Raises:
    [WrongFunction] if [col] is not a FloatColumn. *)

val reduce_string_col : t -> ('a -> string option -> 'a) -> 'a -> 'a
(** [reduce_string_col col func init] is the result of folding left over the
    body of [col] with [func] and [init]. Requires: [col] is a StringColumn.
    Raises: [WrongFunction] if [col] is not a StringColumn. *)
