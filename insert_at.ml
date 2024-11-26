let rec insert_at index member lst = 
    match lst, index with
    | [], 0 -> [member]
    | [], _ when index > 0 -> lst
    | hd :: tl, 0 -> member :: lst
    | hd :: tl, _ when index > 0 -> hd :: insert_at (index - 1) member tl
    | lst, _ -> lst
