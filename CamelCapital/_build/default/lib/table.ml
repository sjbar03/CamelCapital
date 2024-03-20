type t = Column.t list

exception NoColumnFound

let empty_table : t = []
let add_column (tbl : t) col : t = List.append tbl [ col ]

let remove_column (tbl : t) col_name : t =
  List.filter (fun col -> Column.title col <> col_name) tbl

let rec get_column_index tbl ?(i = 0) col_name =
  match tbl with
  | [] -> raise NoColumnFound
  | h :: t ->
      if Column.title h = col_name then i
      else get_column_index t col_name ~i:(i + 1)

let rec insert_col_at_index (tbl : t) (col : Column.t) (i : int)
    (acc : Column.t list) =
  match tbl with
  | [] -> List.rev acc @ [ col ]
  | h :: t ->
      if i = 0 then List.rev acc @ (col :: h :: t)
      else insert_col_at_index t col (i - 1) (h :: acc)

let rec get_column_by_name tbl name : Column.t =
  match tbl with
  | [] -> raise NoColumnFound
  | h :: t -> if Column.title h = name then h else get_column_by_name t name

let length tbl =
  if tbl = empty_table then 0 else Column.col_length (List.nth tbl 0)

(********************Print Functions************************)
let truncate str len =
  if String.length str > len then String.sub str 0 len ^ "..." else str

let rec print_column_titles tbl =
  match tbl with
  | [] -> print_endline ""
  | h :: b ->
      Printf.printf "%-15s" (truncate (Column.title h) 10);
      print_column_titles b

let rec print_row_at_index tbl index =
  match tbl with
  | [] -> print_endline ""
  | h :: t ->
      Printf.printf "%-15s" (truncate (Column.get_entry_at_index h index) 10);
      print_row_at_index t index

let rec print_table ?(index = 0) ?(num_rows = 50) tbl =
  if index < length tbl && index != 0 && index < num_rows then (
    print_row_at_index tbl index;
    print_table ~index:(index + 1) tbl ~num_rows)
  else if index = 0 && length tbl <> 0 then (
    print_column_titles tbl;
    print_row_at_index tbl index;
    print_table ~index:(index + 1) tbl ~num_rows)
  else print_endline ""

(****************CSV LOADING********************)

let load_csv filename : string list list =
  Csv.square (Csv.load ("data/" ^ filename ^ ".csv"))

let rec create_column_body (data : string list list) index acc =
  match data with
  | [] -> List.rev acc
  | h :: t -> create_column_body t index (List.nth h index :: acc)

let rec create_table_list (data : string list list) acc index =
  match index with
  | i ->
      if i >= List.length (List.hd data) then List.rev acc
      else create_table_list data (create_column_body data i [] :: acc) (i + 1)

let rec create_table_from_list data_list tbl =
  match data_list with
  | [] -> tbl
  | h :: t ->
      create_table_from_list t
        (add_column tbl (Column.create_column_from_list h))

let create_table_from_csv filename =
  let data = create_table_list (load_csv filename) [] 0 in
  create_table_from_list data empty_table

(*****************HIGH ORDER OPERATIONS************************)
let map_int_column tbl col_name (func : int -> int) : t =
  let col = get_column_by_name tbl col_name in
  let new_col = Column.map_int_col func col in
  let index = get_column_index tbl col_name in
  let table_wo_col = remove_column tbl col_name in
  insert_col_at_index table_wo_col new_col index []

let map_float_column tbl col_name (func : float -> float) : t =
  let col = get_column_by_name tbl col_name in
  let new_col = Column.map_float_col func col in
  let index = get_column_index tbl col_name in
  let table_wo_col = remove_column tbl col_name in
  insert_col_at_index table_wo_col new_col index []

let map_string_column tbl col_name (func : string -> string) : t =
  let col = get_column_by_name tbl col_name in
  let new_col = Column.map_string_col func col in
  let index = get_column_index tbl col_name in
  let table_wo_col = remove_column tbl col_name in
  insert_col_at_index table_wo_col new_col index []

let equals t1 t2 = List.equal Column.equals t1 t2

(***************************FILTER FUNCTIONS********************************)

let rec batch_row_remove tbl ?(acc = empty_table) row_indices =
  match tbl with
  | [] -> List.rev acc
  | h :: t ->
      batch_row_remove t row_indices
        ~acc:(Column.batch_remove_entries h row_indices :: acc)

let filter_by_int_column tbl crit_func col_name =
  let indices =
    Column.filter_indices_int (get_column_by_name tbl col_name) crit_func
  in
  batch_row_remove tbl indices

let filter_by_string_column tbl crit_func col_name =
  let indices =
    Column.filter_indices_string (get_column_by_name tbl col_name) crit_func
  in
  batch_row_remove tbl indices

let filter_by_float_column tbl crit_func col_name =
  let indices =
    Column.filter_indices_float (get_column_by_name tbl col_name) crit_func
  in
  batch_row_remove tbl indices

(*************************REDUCE FUNCTIONS******************************)
let reduce_int_col tbl col_name func init =
  let col = get_column_by_name tbl col_name in
  Column.reduce_int_col col func init

let reduce_float_col tbl col_name func init =
  let col = get_column_by_name tbl col_name in
  Column.reduce_float_col col func init

let reduce_string_col tbl col_name func init =
  let col = get_column_by_name tbl col_name in
  Column.reduce_string_col col func init
