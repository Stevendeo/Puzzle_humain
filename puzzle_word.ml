(* assumes n = List.length l *)

let rec get_list x l =
  if x = 0
  then (List.hd l, List.tl l)
  else
    let el,l_res = get_list (x-1) (List.tl l)
    in el,(List.hd l)::l_res
                         
let rec randomize_list n l =
  match l with
    [] -> []
  | tt::[] -> l
  | _ ->
     begin
       assert (n>1);
       let x=Random.int (n-1) in
       let x,l_res = get_list x l
       in
       x::(randomize_list (n-1) l_res)
     end

type card =
  { w:string;
    mutable devant: int option;
    mutable derriere: int option;
    mutable gauche:int option;
    mutable droite:int option
  }

let make_smpl_card w = {w = w;devant=None;derriere=None;gauche=None;droite=None}

let print_option fmt x =
  match x with
    None -> Format.fprintf fmt "X "
  | Some x -> 
    
    Format.fprintf fmt "%3d" x

let print_card fmt x = if x.w = "" then () else
  Format.fprintf fmt 
    "___________________________\n\
     |           %a            |\n\
     |                         |\n\
     |                         |\n\
     |                         |\n\
     |%a%12s        %a|\n\
     |                         |\n\
     |                         |\n\
     |                         |\n\
     |           %a           |\n\
     |_________________________|\n\n\n"

 print_option (x.devant) print_option x.gauche x.w print_option x.droite print_option ( x.derriere)
  
                              
let read_file ch_in : string array array =

  let list_line = ref  [] in
  begin
  try
    while true do
      list_line := (input_line ch_in) :: !list_line      
    done
  with
    End_of_file -> ()
  end;
                   
  let nb_line = List.length !list_line in

  let array_res = Array.make nb_line [||] in
      Array.iteri
        (fun i _ ->
          let s = List.hd !list_line in
          list_line := List.tl !list_line;
          let t = Array.of_list (Str.split (Str.regexp " ") s) 
          in array_res.(nb_line-i-1) <- t
        )
        array_res; 
      array_res
let _ =
  (* création du tableau *)
  Format.printf "Starting the generation\n%!";
  let ch_in =
  if (Array.length Sys.argv >= 2)
  then
    open_in Sys.argv.(1)
  else
    Pervasives.stdin
  in
  
  let tab_lu = read_file ch_in in
  let nb_line = Array.length tab_lu in
  Format.printf "nb_line=%d@.%!" nb_line;
  let nb_col = Array.fold_left (fun res t -> max res (Array.length t)) 0 tab_lu  in

  let mat = Array.make_matrix nb_line nb_col "" in
  Array.iteri
    (fun i t ->
        Array.iteri
          (fun j c ->
            mat.(i).(j) <- c
          )
          t
    )
    tab_lu;
   
  let x = ref 0 and l = ref [] in
  (* generate the unique random numbers, l stores the numbers already taken *)

  let gen () : int =
    x := Random.int 400;
    while (List.mem !x !l) do
      x := Random.int 400
    done;
    l:= !x::!l;
    !x
  in

  let mat =
    Array.map
    (fun t ->
      Array.map
        (fun x ->
           make_smpl_card x
        )
        t
    )
    mat
  in

  
    (* generation of the list of cards *)
  let complete_cards i j : unit =
    let card = mat.(i).(j) in if card.w = "" then () else
    let _ = try 
        match card.gauche, mat.(i).(j-1).w with 
          Some _,_ | _,"" -> () 
        | None,_ -> 
          let left_c = mat.(i).(j-1) in
          let i = gen () in
          card.gauche <- Some i;
          left_c.droite <- Some i;
      with Invalid_argument _ -> card.gauche <- None in
    let _ = try  
        match card.droite, mat.(i).(j+1).w with 
          Some _,_ | _,"" -> () 
        | None,_ -> 
          
        let right_c = mat.(i).(j+1) in
        let i = gen () in
        card.droite <- Some i;
        right_c.gauche <- Some i;

      with Invalid_argument _ -> card.droite <- None in


    let _ = try 
        match card.devant, mat.(i-1).(j).w with 
          (Some _),_ | _,""-> () 
        | None,_ -> 
        let up_c = mat.(i-1).(j) in
        let i = gen () in
        card.devant <- Some i;
        up_c.derriere <- Some i;
      with Invalid_argument _ -> card.devant <- None in

    let _ = try 
        match card.derriere, mat.(i+1).(j).w with 
          Some _,_ | _,"" -> () 
        | None,_ -> 
        let down_c = mat.(i+1).(j) in
        let i = gen () in
        card.derriere <- Some i;
        down_c.devant <- Some i;
      with Invalid_argument _ -> card.derriere <- None  in
    () 
  in
  for i=0 to nb_line-1 do
    for j= 0 to nb_col-1 do
      (complete_cards i j);
      Format.fprintf ( Format.formatter_of_out_channel stdout) "%a" print_card mat.(i).(j)
    done
  done;
    
    (* printing *)
  let ch_out =
    if (Array.length Sys.argv = 2)
    then
      open_out (Sys.argv.(1)^".out")
    else
      Pervasives.stdout
  in

  let fmt = Format.formatter_of_out_channel ch_out in

  Array.iter (Array.iter (print_card fmt)) mat;
