let rec double_array ?(pos = 0) arr =
  if pos >= Array.length arr then arr
  else
    (arr.(pos) <- arr.(pos) * 2;
     double_array ~pos:(pos + 1))
      arr

let rec double_list list =
  match list with [] -> [] | hd :: tl -> (hd * 2) :: double_list tl

let rec count_x ?(pos = 0) s =
  if pos >= String.length s then 0
  else (if String.get s pos == 'x' then 1 else 0) + count_x ~pos:(pos + 1) s

let () =
  print_endline (Printers.list_to_string (double_list [ 1; 2; 3; 4; 5 ]));
  print_endline (Printers.array_to_string (double_array [| 1; 2; 3; 4; 5 |]));
  print_endline (string_of_int (count_x "xbxcxd"))
