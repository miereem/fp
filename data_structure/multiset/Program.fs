module Multiset

open System

type MultiSet<'T when 'T : equality> =
     {
        Table : (int * 'T * int) option array  // (хэш, элемент, количество)
        Capacity : int
        Size : int
    }

let empty (capacity : int) : MultiSet<'T> =
    {
        Table = Array.create capacity None
        Capacity = capacity
        Size = 0
    }


let private findSlot (hash : int) (capacity : int) (i : int) : int =
    (hash + i) % capacity


let add (element : 'T) (count : int) (set : MultiSet<'T>) : MultiSet<'T> =
    if count <= 0 then set
    else
        let hash = abs (element.GetHashCode())
        let rec loop i =
            let slot = findSlot hash set.Capacity i
            match set.Table.[slot] with
            | None ->
                // Если слот пустой, добавляем элемент
                set.Table.[slot] <- Some (hash, element, count)
                { set with Size = set.Size + 1 }
            | Some (_, e, c) when e = element ->
                // Если элемент уже есть, увеличиваем его количество
                set.Table.[slot] <- Some (hash, element, c + count)
                set
            | _ -> loop (i + 1)
        loop 0


let count (element : 'T) (set : MultiSet<'T>) : int =
    let hash = abs (element.GetHashCode())
    let rec loop i =
        let slot = findSlot hash set.Capacity i
        match set.Table.[slot] with
        | None -> 0
        | Some (_, e, c) when e = element -> c
        | _ -> loop (i + 1)
    loop 0


let isEmpty (set : MultiSet<'T>) : bool =
    set.Size = 0


let union (set1 : MultiSet<'T>) (set2 : MultiSet<'T>) : MultiSet<'T> =
    let merged = Array.copy set1.Table
    set2.Table
    |> Array.iter (function
        | Some (_, e, c) -> add e c { set1 with Table = merged } |> ignore
        | None -> ())
    { set1 with Table = merged; Size = set1.Size + set2.Size }


let zero<'T when 'T : equality> : MultiSet<'T> = empty 16

