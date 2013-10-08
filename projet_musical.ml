
type objet_musical =
    | Note of int * int * int
    | Silence of int
    | Parallele of objet_musical list
    | Compose of objet_musical list ;;

let s1 = Compose((Note(60,1000,65))::(Note (64,500,65))::(Note (62,500,65))::(Silence(1000))::(Note (66,1000,65))::[]);;
let s2 = Compose((Note(53,2000,65))::(Note (57,1000,65))::(Note (57,1000,65))::[]);;
let exemple = Parallele([s1;s2]);;

let max e1 e2 = if e1 > e2 then e1 else e2 ;;

let rec duration obj =
    match obj with
        | Note(_, d, _) -> d
        | Silence d     -> d
        | Compose l     -> List.fold_left (+) 0 (List.map duration l)
        | Parallele l   -> List.fold_left max  0 (List.map duration l) ;;

duration exemple ;;

let rec copy obj =
    match obj with
        | Note(a, b, c) -> Note(a, b, c)
        | Silence d     -> Silence(d)
        | Compose l     -> Compose(List.map copy l)
        | Parallele l   -> Parallele(List.map copy l) ;;

let exemple2 = copy exemple ;;

let rec note_count obj =
    match obj with
        | Note _ | Silence _ -> 1
        | Compose l          -> List.fold_left (+) 0 (List.map note_count l)
        | Parallele l        -> List.fold_left (+) 0 (List.map note_count l) ;;

note_count s1 ;;

note_count exemple ;;

let rec stretch = function f -> function obj ->
    match obj with
        | Note(a, b, c) -> Note(a, (int_of_float ((float_of_int b) *. f)), c)
        | Silence d     -> Silence((int_of_float ((float_of_int d) *. f)))
        | Compose l     -> Compose(List.map (stretch f) l)
        | Parallele l   -> Parallele(List.map (stretch f) l) ;;

let exemple3 = stretch 2.5 exemple ;;

let rec transpose obj =
    match obj with
        | Note(a, b, c) -> Note(a + 12, b, c)
        | Silence d     -> Silence(d)
        | Compose l     -> Compose(List.map transpose l)
        | Parallele l   -> Parallele(List.map transpose l) ;;

let rec retrograde obj =
    match obj with
        | Note(a, b, c)    -> Note(a, b, c)
        | Silence d        -> Silence(d)
        | Compose l        -> Compose(List.rev(List.map (retrograde) l))
        | Parallele l      -> Parallele(List.rev (List.map (retrograde) l)) ;;

retrograde exemple ;;

let rec mirror obj =
    match obj with
        | Note(a, b, c)   -> Note(a - 2 * (a - 60), b, c)
        | Silence d       -> Silence(d)
        | Compose l       -> Compose(List.map (mirror) l)
        | Parallele l     -> Parallele(List.map (mirror) l) ;;

let rec repeat = function obj -> function nbr ->
    if (nbr = 0 || nbr = 1) 
    then obj
    else Compose(obj::(repeat obj (nbr - 1))::[]) ;;

let rec canon = function decalage -> function obj -> 
    Parallele(obj::Compose(Silence(decalage)::obj::[])::[]) ;; 

let concat_mo = function mo1 -> function mo2 ->
    Compose(mo1::mo2::[]) ;;

(* Partie grphique : Piano-roll *)

let init_graph = 
    Graphics.open_graph " 1000x300" ;;
    (* 2 pixels pour 1 hauteur , pour la longeur : 1 pixel pour 25 ms *)

let random_color () = 
    match (Random.int 7) with
    | 0 -> Graphics.black
    | 1 -> Graphics.red
    | 2 -> Graphics.green
    | 3 -> Graphics.blue
    | 4 -> Graphics.yellow
    | 5 -> Graphics.cyan
    | _ -> Graphics.magenta ;;


let draw_note = function hauteur -> function duree -> function debut -> 
    Graphics.set_color (random_color ()) ;
    Graphics.fill_rect (debut / 25 ) (hauteur * 2) (duree / 25) 2 ;
    debut + duree ;;

let rec draw_om_list = function debut -> function
    | []   -> debut 
    | a::q -> (draw_om_list (draw_om debut a) q)
and
    draw_om = function debut -> function
    | Note(h, d, v)                     -> draw_note h d debut
    | Silence d                         -> debut + d
    | Compose []                        -> debut
    | Parallele []                      -> debut
    | Compose(a::q)                     -> (draw_om_list (draw_om debut a) q) 
    | Parallele l                       -> (List.fold_left (max) 0 (List.map (draw_om debut) l)) ;;

let continue () =
    let ev = Graphics.wait_next_event [Graphics.Key_pressed] in (if ev.Graphics.key = 'q' then false else true) ;;


let main =
    Random.init (int_of_float (Sys.time ())) ;
    init_graph ;
    

    draw_om 0 (repeat exemple 5) ;

    while continue () do 
        ()
    done ;
    Graphics.close_graph () 

(*
    let data = (960, 
                  [((0, 0, ControlChange (60,65)); 
                    (0, 0, ControlChange (60, 65)); 
                  )]) ;;

    (Midi.write data Sys.argv.(1)) ;

*)

  ;;

