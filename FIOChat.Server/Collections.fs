namespace FIOChat.Server

open System.Collections.Immutable

module Collections =

    type CircularBuffer<'T>(capacity: int) =
        let mutable queue = ImmutableQueue.Empty
        let mutable count = 0

        member this.Add(item: 'T) =
            if count >= capacity then
                queue <- queue.Dequeue()
            else
                count <- count + 1
            queue <- queue.Enqueue(item)

        member this.ToList() =
            queue |> Seq.toList
