open Double_char.Main

let is_even n = (n mod 2) = 0

let text_length_is_doubled = 
  QCheck.Test.make ~count: 100 
  ~name: "text input length is doubled"
  QCheck.(string_printable)
  (fun t ->  String.length (double_char t) = 2 * (String.length t))

let text_length_is_even =
   QCheck.Test.make ~count:100
   ~name: "length is even"
   QCheck.(string_printable)
   (fun t -> is_even (String.length @@ double_char t))

let two_first_char_are_the_same =
  QCheck.Test.make ~count:1000
  ~name: "2 first chars are the same"
  QCheck.(string_printable_of_size (Gen.int_range 1 60))
  (fun t -> (double_char t).[0] = (double_char t).[1])

let _ =
  let open OUnit2 in
  run_test_tt_main
  ("double char invariant" >:::
    List.map QCheck_ounit.to_ounit2_test [text_length_is_doubled; text_length_is_even; two_first_char_are_the_same])