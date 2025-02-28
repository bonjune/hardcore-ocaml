let opt = ref false
let input = ref ""
let options = [ ("-opt", Arg.Set opt, "Optimized version") ]
let long_list n = List.init n (fun _ -> Random.int 10000000)
let to_string_unopt lst = List.fold_left (fun s n -> s ^ string_of_int n) "" lst
let to_string_opt lst =
  let rec iter lst acc =
    match lst with
    | [] -> acc
    | hd :: tl -> iter tl (string_of_int hd :: acc)
  in

  iter lst []
  |> String.concat ""


let main () =
  Arg.parse options (fun x -> input := x) "";
  Memtrace.trace_if_requested ();
  let n = int_of_string !input in
  let init_list = long_list n in
  let to_string = if !opt then to_string_opt else to_string_unopt in
  let to_string_list = to_string init_list in
  String.length to_string_list |> Printf.printf "%d\n"

let _ = main ()
