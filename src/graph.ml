(* let print_item print v = print v; print_string " "
 * let print_labelled_item print i v = print_int i; print_endline ":"; print v; print_newline ()
 * let print_array print = Array.iter (print_item print)
 * let print_list print = List.iter (print_item print)
 * let print_graphed = Array.iteri (print_labelled_item (print_list print_int))
 * let print_components = List.iteri (print_labelled_item (print_list print_int))
 *
 * let dump_state name lowlink number stack components i =
 *   print_endline name;
 *   print_array print_int lowlink; print_endline "lowlink";
 *   print_array print_int number; print_endline "number";
 *   print_list print_int stack; print_endline "stack";
 *   print_endline "components: ";
 *   print_components components;
 *   print_string "i = "; print_int i; print_newline() *)

let rec fold_cnt f acc xs cnt = match xs with
  | [] -> cnt acc
  | x :: rest -> f acc x (fun acc' -> fold_cnt f acc' rest cnt)

let sccs nodes =
  let count = Array.length nodes in
  let rec connect (lowlink, number, stack, components, i) v cnt =
    if number.(v) >= 0
    then cnt (lowlink, number, stack, components, i)
    else
      let loop_cnt v cnt (lowlink, number, stack, components, i) =
        if i < count
        then connect (lowlink, number, stack, components, i) (v + 1) cnt
        else cnt (lowlink, number, stack, components, i) in
      let end_cnt v cnt (lowlink, number, stack, components, i) =
        if lowlink.(v) = number.(v)
        then
          let rec iter stack component = match stack with
            | w :: rest when number.(w) >= number.(v) -> iter rest (w :: component)
            | _ -> stack, component in
          let stack', component = iter stack [] in
          loop_cnt v cnt (lowlink, number, stack', component :: components, i)
        else
          loop_cnt v cnt (lowlink, number, stack, components, i) in
      let tree_cnt w cnt (lowlink, number, stack, components, i) =
        lowlink.(v) <- min lowlink.(v) lowlink.(w);
        cnt (lowlink, number, stack, components, i) in
      let iter_cnt (lowlink, number, stack, components, i) w cnt =
        if lowlink.(w) = -1
        then connect (lowlink, number, stack, components, i) w (tree_cnt w cnt)
        else if number.(w) < number.(v) && List.mem w stack
        then (lowlink.(v) <- min lowlink.(v) lowlink.(w);
              cnt (lowlink, number, stack, components, i))
        else cnt (lowlink, number, stack, components, i) in
      lowlink.(v) <- i;
      number.(v) <- i;
      fold_cnt iter_cnt (lowlink, number, v :: stack, components, i + 1) nodes.(v) (end_cnt v cnt) in
  let lowlink = Array.init count (fun _ -> -1) in
  let number = Array.init count (fun _ -> -1) in
  if count > 0 then
    connect (lowlink, number, [], [], 0) 0 (fun (_, _, _, components, _) ->
        components |> List.map Array.of_list |> List.rev |> Array.of_list)
  else
    [||]
