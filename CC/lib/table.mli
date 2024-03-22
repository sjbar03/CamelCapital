type t
(** An alias for the type [Table] *)

exception NoColumnFound
(** [exception NoColumnFound] is raised when a column with a given title is not
    present in a table. *)

val empty_table : t
(** The empty table has no columns. *)

val add_column : t -> Column.t -> t
(** [add_column tbl col] is a copy of table [tbl] with column [col] appended to
    the right-hand side of the table.*)

val remove_column : t -> string -> t
(** [remove_column tbl col_name] is a copy of table [tbl] with the column titled
    [col_name] removed. If no column with that title exists, [tbl] is returned.*)

val get_column_by_name : t -> string -> Column.t
(** [get_column_by_name tbl name] is the column in table [tbl] with the title
    [name]. Raises NoColumnFound if no column by the given title exists in
    [tbl].*)

val length : t -> int
(** [length tbl] is the number of columns in [tbl].*)

val print_table : ?index:int -> ?num_rows:int -> t -> unit
(** [print_table tbl num_rows] prints the titles of each column and then the
    first [num_rows] rows of [tbl].*)

val create_table_from_csv : string -> t
(** [create_table_from_csv filename] is a table with contents retrieved from the
    file at data/"filename".csv. Do not include the ".csv" Requires: the
    provided CSV table is rectangular and all columns are uniform in type. *)

val equals : t -> t -> bool
(** [equals t1 t2] is true if tables t1 and t2 have structurally equivalent
    (under [Column.equals])) columns in the same order.*)

val filter_by_string_column : t -> (string option -> bool) -> string -> t
(** [filter_by_string_column tbl crit_func col_name] is the table created by
    removing all rows of [tbl] in which the entries in the column titled
    [col_name] are false under the criteria function [crit_func]. Requires: the
    column titled [col_name] has string entries. Raises: [Column.WrongFunction]
    if the column titled [col_name] does not have string entries.*)