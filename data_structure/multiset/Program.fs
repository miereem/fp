module Multiset

open System

[<CustomEquality; NoComparison>]
type MultiSet<'T when 'T : equality> =
    {
        Table : (int * 'T * int) option array  // (hash, element, count)
        Capacity : int
        Size : int
    }
    override this.Equals(other) =
        match other with
        | :? MultiSet<'T> as otherSet ->
            this.Size = otherSet.Size &&
            Array.forall2 (fun x y ->
                match x, y with
                | Some (_, e1, c1), Some (_, e2, c2) -> e1 = e2 && c1 = c2
                | None, None -> true
                | _ -> false) this.Table otherSet.Table
        | _ -> false

    override this.GetHashCode() =
        hash (this.Table, this.Capacity, this.Size)

let empty (capacity : int) : MultiSet<'T> =
    {
        Table = Array.create capacity None
        Capacity = capacity
        Size = 0
    }

// using linear probing
let private findSlot (hash : int) (capacity : int) (i : int) : int =
    (hash + i) % capacity


let add (element : 'T) (count : int) (set : MultiSet<'T>) : MultiSet<'T> =
    if count <= 0 then set
    else
        let hash = abs (element.GetHashCode())
        let rec loop i (set: MultiSet<'T>) =
            let slot = findSlot hash set.Capacity i
            match set.Table.[slot] with
            | None ->
                let newTable = Array.mapi (fun idx value -> if idx = slot then Some (hash, element, count) else value) set.Table
                { set with Table = newTable; Size = set.Size + 1 }
            | Some (_, e, c) when e = element ->
                let newTable = Array.mapi (fun idx value -> if idx = slot then Some (hash, element, c + count) else value) set.Table
                { set with Table = newTable }
            | _ -> loop (i + 1) set
        loop 0 set

let count (element : 'T) (set : MultiSet<'T>) : int =
    let hash = abs (element.GetHashCode())
    let rec loop i =
        let slot = findSlot hash set.Capacity i
        match set.Table.[slot] with
        | None -> 0
        | Some (_, e, c) when e = element -> c
        | _ -> loop (i + 1)
    loop 0

let delete (element : 'T) (set : MultiSet<'T>) : MultiSet<'T> =
    let hash = abs (element.GetHashCode())
    let rec loop i (set: MultiSet<'T>) =
        let slot = findSlot hash set.Capacity i
        match set.Table.[slot] with
        | None -> set 
        | Some (_, e, c) when e = element ->
            if c > 1 then
                let newTable = Array.copy set.Table
                newTable.[slot] <- Some (hash, element, c - 1)
                { set with Table = newTable }
            else
                let newTable = Array.copy set.Table
                newTable.[slot] <- None
                { set with Table = newTable; Size = set.Size - 1 }
        | _ -> loop (i + 1) set
    loop 0 set

let isEmpty (set : MultiSet<'T>) : bool =
    set.Size = 0

let union (set1 : MultiSet<'T>) (set2 : MultiSet<'T>) : MultiSet<'T> =
    if isEmpty set1 then set2
    elif isEmpty set2 then set1
    else
        let combinedCapacity = set1.Capacity + set2.Capacity
        let result = empty combinedCapacity

        let resultWithSet1 =
            set1.Table
            |> Array.fold (fun acc entry ->
                match entry with
                | Some (_, e, c) -> add e c acc
                | None -> acc) result

        set2.Table
        |> Array.fold (fun acc entry ->
            match entry with
            | Some (_, e, c) -> add e c acc
            | None -> acc) resultWithSet1

let filter (predicate : 'T -> bool) (set : MultiSet<'T>) : MultiSet<'T> =
    let rec processTable i filteredSet =
        if i >= set.Capacity then
            filteredSet
        else
            match set.Table.[i] with
            | Some (_, e, c) when predicate e ->
                let updatedSet = add e c filteredSet
                processTable (i + 1) updatedSet
            | _ -> processTable (i + 1) filteredSet

    let emptySet = { Table = Array.create set.Capacity None; Capacity = set.Capacity; Size = 0 }
    processTable 0 emptySet

// использую свою add 

let foldLeft (folder : 'State -> 'T -> int -> 'State) (state : 'State) (set : MultiSet<'T>) : 'State =
    set.Table
    |> Array.fold (fun acc entry ->
        match entry with
        | Some (_, e, c) -> folder acc e c
        | None -> acc) state

let foldRight (folder : 'T -> int -> 'State -> 'State) (state : 'State) (set : MultiSet<'T>) : 'State =
    set.Table
    |> Array.choose (fun entry ->
        match entry with
        | Some (_, e, c) -> Some (e, c)
        | None -> None)
    |> Array.rev
    |> Array.fold (fun acc (e, c) -> folder e c acc) state

let zero<'T when 'T : equality> : MultiSet<'T> = empty 0