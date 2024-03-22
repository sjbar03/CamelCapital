type t = Column.t list

exception NoColumnFound

let empty_table : t = []
let add_column (tbl : t) col : t = List.append tbl [ col ]

let remove_column (tbl : t) col_name : t =
  List.filter (fun col -> Column.title col <> col_name) tbl
  
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

let equals t1 t2 = List.equal Column.equals t1 t2

(***************************FILTER FUNCTIONS********************************)

let rec batch_row_remove tbl ?(acc = empty_table) row_indices =
  match tbl with
  | [] -> List.rev acc
  | h :: t ->
      batch_row_remove t row_indices
        ~acc:(Column.batch_remove_entries h row_indices :: acc)

let filter_by_string_column tbl crit_func col_name =
  let indices =
    Column.filter_indices_string (get_column_by_name tbl col_name) crit_func
  in
  batch_row_remove tbl indices