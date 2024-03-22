type t =
  | IntColumn of string * int option list
  | StringColumn of string * string option list
  | FloatColumn of string * float option list

exception WrongFunction

let empty_int_col = IntColumn ("", [])
let empty_string_col = StringColumn ("", [])
let empty_float_col = FloatColumn ("", [])

let title (col : t) =
  match col with
  | IntColumn (t, _) -> t
  | StringColumn (t, _) -> t
  | FloatColumn (t, _) -> t

let get_string_body (col : t) =
  match col with
  | StringColumn (_, b) -> b
  | _ -> raise WrongFunction

let string_of_int_opt i = if i = None then "" else string_of_int (Option.get i)

let string_of_float_opt flt =
  if flt = None then "" else string_of_float (Option.get flt)

let string_of_string_opt str = if str = None then "" else Option.get str

let get_entry_at_index (col : t) (index : int) =
  match col with
  | IntColumn (_, b) -> string_of_int_opt (List.nth b index)
  | FloatColumn (_, b) -> string_of_float_opt (List.nth b index)
  | StringColumn (_, b) -> string_of_string_opt (List.nth b index)

let col_length col =
  match col with
  | IntColumn (_, b) -> List.length b
  | FloatColumn (_, b) -> List.length b
  | StringColumn (_, b) -> List.length b

type data =
  | String of string option list
  | Float of float option list
  | Int of int option list

let find_data_type data =
  if int_of_string_opt (List.nth data 0) <> None then
    Int (List.map int_of_string_opt data)
  else if float_of_string_opt (List.nth data 0) <> None then
    Float (List.map float_of_string_opt data)
  else String (List.map Option.some data)

let create_column_from_list data =
  let body = find_data_type (List.tl data) in
  match body with
  | String b -> StringColumn (List.hd data, b)
  | Float b -> FloatColumn (List.hd data, b)
  | Int b -> IntColumn (List.hd data, b)

let equals col1 col2 =
  match (col1, col2) with
  | IntColumn (t, b), IntColumn (t', b') -> t = t' && b = b'
  | StringColumn (t, b), StringColumn (t', b') -> t = t' && b = b'
  | FloatColumn (t, b), FloatColumn (t', b') -> t = t' && b = b'
  | _ -> false

let rec remove lst index acc =
  match lst with
  | [] -> failwith "Invalid index out of bounds."
  | h :: t ->
      if index = 0 then List.rev acc @ t else remove t (index - 1) (h :: acc)

let remove_entry col index =
  match col with
  | IntColumn (t, b) -> IntColumn (t, remove b index [])
  | FloatColumn (t, b) -> FloatColumn (t, remove b index [])
  | StringColumn (t, b) -> StringColumn (t, remove b index [])

let rec filter_indices_string col ?(acc = []) ?(index = 0) crit_func =
  match get_string_body col with
  | [] -> List.sort Int.compare acc
  | h :: t ->
      if crit_func h then
        filter_indices_string
          (StringColumn (title col, t))
          crit_func ~acc ~index:(index + 1)
      else
        filter_indices_string
          (StringColumn (title col, t))
          crit_func ~acc:(index :: acc) ~index:(index + 1)

let rec batch_remove_list lst ?(acc = []) ?(index = 0) indices =
  match indices with
  | [] -> List.rev acc @ lst
  | h :: t ->
      if index = h then batch_remove_list (List.tl lst) t ~acc ~index:(index + 1)
      else
        batch_remove_list (List.tl lst) indices ~acc:(List.hd lst :: acc)
          ~index:(index + 1)

let batch_remove_entries col indices =
  match col with
  | IntColumn (t, b) -> IntColumn (t, batch_remove_list b indices)
  | StringColumn (t, b) -> StringColumn (t, batch_remove_list b indices)
  | FloatColumn (t, b) -> FloatColumn (t, batch_remove_list b indices)
