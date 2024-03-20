(** @author Jephthah Kwame Mensah (jkm255) *)

open A4.MakeSet

module CaseSensitive = struct
  type t = string

  let compare t1 t2 = String.(compare t1 t2)
  let to_string t : string = t
end

module CaseInsensitive = struct
  type t = string

  let compare s1 s2 = String.(compare (lowercase_ascii s1) (lowercase_ascii s2))
  let to_string t : string = t
end

module CaseSensitiveStringSet = Make (CaseSensitive)
module CaseInsensitiveStringSet = Make (CaseInsensitive)

let argv : string list = Array.to_list Sys.argv

(** [concat_dir path file] is the concatenation of [path] and [file], forming a
    full file path. *)
let concat_dir path file = path ^ file

let matrix lst to_string =
  let f acc elt = to_string elt :: acc in
  [ List.rev (List.fold_left f [] lst) ]

let mat =
  if BatList.at argv 1 = "-i" then
    let file1 =
      BatList.of_enum
        (BatFile.lines_of (concat_dir "data/" (BatList.at argv 2)))
    in
    let file2 =
      BatList.of_enum
        (BatFile.lines_of (concat_dir "data/" (BatList.at argv 3)))
    in
    let set1 = CaseInsensitiveStringSet.of_list file1 in
    let set2 = CaseInsensitiveStringSet.of_list file2 in
    let set3 = CaseInsensitiveStringSet.inter set1 set2 in
    let mat =
      matrix (CaseInsensitiveStringSet.to_list set3) CaseInsensitive.to_string
    in
    mat
  else
    let file1 =
      BatList.of_enum
        (BatFile.lines_of (concat_dir "data/" (BatList.at argv 1)))
    in
    let file2 =
      BatList.of_enum
        (BatFile.lines_of (concat_dir "data/" (BatList.at argv 2)))
    in
    let set1 = CaseSensitiveStringSet.of_list file1 in
    let set2 = CaseSensitiveStringSet.of_list file2 in
    let set3 = CaseSensitiveStringSet.inter set1 set2 in
    let mat =
      matrix (CaseSensitiveStringSet.to_list set3) CaseSensitive.to_string
    in
    mat

let () = Csv.(square mat |> transpose |> print_readable)
