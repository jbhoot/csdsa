module Ex1 = struct
  let list_to_hashtbl list =
    let ht = Hashtbl.create (List.length list) in
    List.iter (fun i -> Hashtbl.add ht i true) list;
    ht

  let intersection a b =
    let larger_list, smaller_list =
      if List.length a > List.length b then (a, b) else (b, a)
    in
    let hashtbl = list_to_hashtbl larger_list in
    List.fold_left
      (fun accum curr ->
        match Hashtbl.find_opt hashtbl curr with
        | Some _ -> curr :: accum
        | None -> accum)
      [] smaller_list
end

module Ex2 = struct
  let rec _find_duplicate hashtbl list =
    match list with
    | [] -> None
    | hd :: tl -> (
        match Hashtbl.find_opt hashtbl hd with
        | Some _ -> Some hd
        | None ->
            Hashtbl.add hashtbl hd 1;
            _find_duplicate hashtbl tl)

  let find_duplicate list =
    let ht = Hashtbl.create (List.length list) in
    _find_duplicate ht list
end

module Ex3 = struct
  let string_to_hashtbl s =
    let hashtbl = Hashtbl.create (String.length s) in
    String.iter (fun c -> Hashtbl.add hashtbl c true) s;
    hashtbl

  let find_missing_letter s =
    let hashtbl = string_to_hashtbl s in
    let alphabet_set =
      [
        'a';
        'b';
        'c';
        'd';
        'e';
        'f';
        'g';
        'h';
        'i';
        'j';
        'k';
        'l';
        'm';
        'n';
        'o';
        'p';
        'q';
        'r';
        's';
        't';
        'u';
        'v';
        'w';
        'x';
        'y';
        'z';
      ]
    in
    List.find_opt
      (fun c ->
        match Hashtbl.find_opt hashtbl c with Some _ -> false | None -> true)
      alphabet_set
end

module Ex4 = struct
  let string_to_hashtbl s =
    let hashtbl = Hashtbl.create (String.length s) in
    String.iter
      (fun c ->
        match Hashtbl.find_opt hashtbl c with
        | Some v -> Hashtbl.add hashtbl c (v + 1)
        | None -> Hashtbl.add hashtbl c 1)
      s;
    hashtbl

  let rec _find_first_non_duplicate_char hashtbl s =
    match s with
    | [] -> None
    | hd :: tl -> (
        match Hashtbl.find_opt hashtbl hd with
        | Some v ->
            if v == 1 then Some hd
            else _find_first_non_duplicate_char hashtbl tl
        | None -> _find_first_non_duplicate_char hashtbl tl)

  let find_first_non_duplicate_char s =
    let hashtbl = string_to_hashtbl s in
    _find_first_non_duplicate_char hashtbl (s |> String.to_seq |> List.of_seq)
end

let () =
  print_endline
    ("Ex1 (intersection): "
    ^ Printers.list_to_string (Ex1.intersection [ 3; 5; 4; 2 ] [ 4; 5; 3; 6 ]));

  print_endline
    ("Ex2 (find duplicate str in list): "
    ^
    match Ex2.find_duplicate [ "a"; "b"; "c"; "d"; "c"; "e"; "f" ] with
    | Some v -> v
    | None -> "no duplicate found");

  print_endline
    ("Ex3 (find missing alphabet): "
    ^
    match
      Ex3.find_missing_letter "the quick brown box jumps over a lazy dog"
    with
    | Some v -> String.make 1 v
    | None -> "all letters are present.");

  print_endline
    ("Ex4 (find non-duplicate char): "
    ^
    match
      Ex4.find_first_non_duplicate_char "minimum"
    with
    | Some v -> String.make 1 v
    | None -> "No unique letter present.")
