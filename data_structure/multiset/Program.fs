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

// Create an empty MultiSet with the given capacity
let empty (capacity : int) : MultiSet<'T> =
    {
        Table = Array.create capacity None
        Capacity = capacity
        Size = 0
    }

// Helper function to find the next slot in the table using linear probing
let private findSlot (hash : int) (capacity : int) (i : int) : int =
    (hash + i) % capacity

// Add an element to the MultiSet with the specified count
let add (element : 'T) (count : int) (set : MultiSet<'T>) : MultiSet<'T> =
    if count <= 0 then set
    else
        let hash = abs (element.GetHashCode())
        let rec loop i (set: MultiSet<'T>) =
            let slot = findSlot hash set.Capacity i
            match set.Table.[slot] with
            | None ->
                // If the slot is empty, add the element
                let newTable = Array.copy set.Table
                newTable.[slot] <- Some (hash, element, count)
                { set with Table = newTable; Size = set.Size + 1 }
            | Some (_, e, c) when e = element ->
                // If the element already exists, increment its count
                let newTable = Array.copy set.Table
                newTable.[slot] <- Some (hash, element, c + count)
                { set with Table = newTable }
            | _ -> loop (i + 1) set
        loop 0 set

// Get the count of a specific element in the MultiSet
let count (element : 'T) (set : MultiSet<'T>) : int =
    let hash = abs (element.GetHashCode())
    let rec loop i =
        let slot = findSlot hash set.Capacity i
        match set.Table.[slot] with
        | None -> 0
        | Some (_, e, c) when e = element -> c
        | _ -> loop (i + 1)
    loop 0



// Check if the MultiSet is empty
let isEmpty (set : MultiSet<'T>) : bool =
    set.Size = 0

let union (set1 : MultiSet<'T>) (set2 : MultiSet<'T>) : MultiSet<'T> =
    if isEmpty set1 then set2
    elif isEmpty set2 then set1
    else
        let combinedCapacity = set1.Capacity + set2.Capacity
        let result = empty combinedCapacity

        // Add elements from set1 and then set2 to the result
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


// The zero element for the MultiSet monoid
let zero<'T when 'T : equality> : MultiSet<'T> = empty 0
