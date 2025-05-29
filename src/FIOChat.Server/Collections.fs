namespace FIOChat.Server

open System.Collections.Immutable

module Collections =

    // TODO: Make this an in memory cache.
    type CircularBuffer<'T>(capacity: int) =
        let mutable queue = ImmutableQueue.Empty
        let mutable count = 0

        member _.Add(item: 'T) =
            if count >= capacity then
                queue <- queue.Dequeue()
            else
                count <- count + 1
            queue <- queue.Enqueue item

        member _.ToList() =
            queue |> Seq.toList
