let rec build_string_of_list list =
  match list with
  | [] -> ""
  | hd :: [] -> Printf.sprintf "%d" hd
  | hd :: tl -> Printf.sprintf "%d; " hd ^ build_string_of_list tl

let list_to_string res = Printf.sprintf "[ %s ]" (build_string_of_list res)

let rec build_string_of_array ?(pos=0) arr =
  if pos >= Array.length arr then
    ""
  else if pos == Array.length arr - 1 then
    Printf.sprintf "%d" arr.(pos)
   else
    Printf.sprintf "%d; " arr.(pos) ^ (build_string_of_array ~pos:(pos+1) arr)

let array_to_string arr = Printf.sprintf "[| %s |]" (build_string_of_array arr)
