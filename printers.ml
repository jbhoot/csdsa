let rec build res =
  match res with
  | [] -> ""
  | hd :: [] -> Printf.sprintf "%d" hd
  | hd :: tl -> Printf.sprintf "%d; " hd ^ build tl

let list_to_string res = Printf.sprintf "[%s]" (build res)
