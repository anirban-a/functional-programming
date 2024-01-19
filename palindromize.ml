open String
let f (ar:int list) (n:int):int=
  let rec find left right l r =
    if l<r && (List.length left>1) && (List.length right >1) then
      let left_fst = List.hd left in
      let left_rest = List.tl left in
      let right_fst = List.hd right in
      let right_rest = List.tl right in

      if left_fst>right_fst then
        let diff = left_fst - right_fst in
        let left' = diff::left_rest in
        1+ (find left' right_rest l (r-1))
      else if left_fst<right_fst then
        let diff = right_fst - left_fst in
        let right' = diff:: right_rest in
        1+ (find left_rest right' (l+1) r)
      else
        find left_rest right_rest (l+1) (r-1)
    else
      0 in
  find ar (List.rev ar) 0 (n-1)
    

let () =
  let rec solve(t:int) =
      if t>0 then
        let n = read_line()|>int_of_string in
        let line = read_line()|>String.split_on_char(' ')|>List.map int_of_string in
        let ans = f line n in
        let ()= Printf.printf "%d\n" ans in
        solve (t-1)
      in
  let t=read_line()|>int_of_string in
  solve(t)
