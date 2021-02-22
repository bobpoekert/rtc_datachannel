
type 'a t = 
    | End
    | Cell of ('a * 'a t Lwt.t)
    | Begin of 'a t Lwt.t

type 'a register = ('a t * 'a t Lwt.u) ref

let add r v = 
    let init_read, init_write = Lwt.task () in 
    let nxt = match v with
    | None -> End
    | Some v -> Cell (v, init_read) in 
    Lwt.wakeup_later (snd !r) nxt;
    r := (nxt, init_write)

let create () = 
    let init_read, init_write = Lwt.task () in ref (
        (Begin init_read),
        init_write
    )

let deref r = fst !r

let rec map m s = 
    match s with 
    | End -> End
    | Begin v -> Begin (Lwt.map (map m) v)
    | Cell (v_in, tail) -> 
            let v_out = m v_in in
            Cell (v_out, Lwt.map (map m) tail)

let chain f v =
    Lwt.bind 
        v
        (fun v -> Lwt.map f v)

let rec map_lwt m s =
    match s with 
    | End -> End
    | Begin v -> Begin (chain (map_lwt m) v)
    | Cell (v_in, tail) ->
        Cell (
            (m v_in),
            (chain (map_lwt m) v))

let rec take_while m s =
    match s with 
    | End -> End
    | Begin v -> Begin (Lwt.map (take_while m) v)
    | Cell (v_in, tail) ->
            if m v_in then
                Cell (v_in, Lwt.map (take_while m) tail)
            else
                End

let rec filter m s = 
    match s with 
    | End -> End
    | Begin v -> Begin (Lwt.map (filter m) v)
    | Cell (v_in, tail) -> 
            if m v_in then
                Cell (v_in, Lwt.map (filter m) tail)
            else
                Begin (Lwt.map (filter m) tail)

let rec append a b =
    match a with 
    | End -> b
    | Begin v -> Begin (Lwt.map (fun v -> append v b) v)
    | Cell (v, tail) -> Cell (v, Lwt.map (fun v -> append v b) tail)

let rec filter_map m s = 
    match s with 
    | End -> End
    | Begin v -> Begin (Lwt.map (filter_map m) v)
    | Cell (v, tail) ->
            let tail = Lwt.map (filter_map m) tail in 
            match m v with
            | None -> Begin tail 
            | Some v -> Cell (v, tail)

let fold_left f acc s = 
    let reader, writer = Lwt.task () in 
    let acc = ref acc in 
    let rec it v = 
        match v with 
        | End -> Lwt.wakeup_later writer !acc
        | Begin v -> Lwt.on_success v it
        | Cell (vv, tail) -> 
                acc := f !acc vv;
                Lwt.on_success tail it
    in
    it s;
    reader

let await f s = 
    let reader, writer = Lwt.task () in
    let rec it s =
        match s with
        | End -> Lwt.wakeup_later writer None
        | Begin v -> Lwt.on_success v it
        | Cell (vv, tail) -> 
                match f vv with
                | None -> 
                    Lwt.on_success tail it
                | Some res -> 
                    Lwt.wakeup_later writer (Some res)
    in
    it s;
    reader

let iter f s = 
    Lwt.map ignore
        (await 
            (fun v -> if f v then (Some ()) else None)
            s)

let rec tl v = 
    match v with
    | End -> End
    | Begin v -> Begin (Lwt.map tl v)
    | Cell (_, tail) -> Begin tail

let rec hd v =
    match v with 
    | End -> Lwt.return None
    | Begin v -> Lwt.bind v hd
    | Cell (vv, _) -> Lwt.return (Some vv)

let rec of_list l tail =
    match l with 
    | [] -> tail
    | h :: t -> Cell (h, Lwt.return (of_list t tail))

let rec interleave_pending (vs, pending) = 
    let vs, pending = List.fold_left
        (fun (vs, vs_pending) v ->
            match v with
            | End -> (vs, vs_pending)
            | Begin v -> (vs, v :: vs_pending)
            | Cell (v, tail) -> (v :: vs, tail :: vs_pending))
        ([], pending) vs in 
    if (vs, pending) = ([], []) then
        End
    else
        let await = Lwt.nchoose_split pending in 
        let await = Lwt.map interleave_pending await in 
        of_list vs (Begin await)

let interleave inp = 
    interleave_pending (inp, [])
