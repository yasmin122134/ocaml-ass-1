let rec insert_at member index lst =
  match index, lst with
  | 0, _ -> member :: lst
  | _, [] -> lst
  | i, hd :: tl -> hd :: insert_at member (i - 1) tl



let insert_none_at index lst =
  insert_at "None" index lst