
let double_char t = 
  let char_of_string s = List.init (String.length s) (String.get s) in
  let list_of_chars = char_of_string t in
  let join l = List.fold_left ( ^ ) "" l in
    List.init (List.length list_of_chars) (fun i -> String.make 2 (List.nth list_of_chars i)) |> join