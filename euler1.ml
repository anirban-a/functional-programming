let read_int() = read_line()|>int_of_string

let multiple_3 (x:int) = x mod 3 == 0
let multiple_5 (x:int) = x mod 5 == 0

let rec calculate_sum (lim:int)=
  if lim>=3 then
    if (multiple_3 lim) || (multiple_5 lim) then  lim+calculate_sum (lim-1)
    else calculate_sum (lim-1)
  else
    0
let ()=
    Printf.printf "%d\n" (calculate_sum 999)