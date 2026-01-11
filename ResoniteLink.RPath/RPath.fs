namespace ResoniteLink.RPath

open System
open System.Linq
open System.Threading.Tasks
open ResoniteLink

type ErrorInfo = string
type RPath<'T> = ILinkInterface -> ValueTask<'T seq>

type ResoniteLinkException(errorMessage) =
    inherit Exception(errorMessage)

    static member Raise(message) =
        raise (ResoniteLinkException(message))
        Seq.empty

module Operators =
    let inline (>>=) ([<InlineIfLambda>] m: RPath<'T>) ([<InlineIfLambda>] binder: 'T -> RPath<'U>) : RPath<'U> =
        fun link ->
            task {
                let! boundValue = m link

                let! secondaryBoundValues =
                    boundValue
                    |> Seq.map (fun result -> binder result link |> _.AsTask())
                    |> Task.WhenAll

                return secondaryBoundValues |> Seq.collect id
            }
            |> ValueTask<'U seq>

    let inline (>=>)
        ([<InlineIfLambda>] left: 'In -> RPath<'Inner>)
        ([<InlineIfLambda>] right: 'Inner -> RPath<'Out>)
        : 'In -> RPath<'Out> =
        fun input -> left input >>= right

module RPath =
    open Operators

    let illegalApiResponse response =
        raise (Exception $"Illegal ResoniteLink API response type: {response.GetType().FullName}")

    let (|FailedResponse|SlotResponse|ComponentResponse|AssetResponse|) (response: Response) =
        match response with
        | r when not r.Success -> FailedResponse r.ErrorInfo
        | :? SlotData as s -> SlotResponse s.Data
        | :? ComponentData as c -> ComponentResponse c.Data
        | :? AssetData as a -> AssetResponse a.AssetURL
        | otherwise ->
            invalidArg
                (nameof response)
                $"The provided message is not a supported subtype: %s{otherwise.GetType().FullName}"

    let getSlot slotID depth includeComponentData (link: ILinkInterface) =
        task {
            let! result =
                link.GetSlotData(GetSlot(SlotID = slotID, Depth = depth, IncludeComponentData = includeComponentData))

            if result.Success then
                return result.Data
            else
                return raise (ResoniteLinkException result.ErrorInfo)
        }

    let inline wrap value : RPath<'T> =
        fun _ -> value |> Seq.singleton |> ValueTask<_>

    let empty: RPath<_> = fun _ -> Seq.empty |> ValueTask<_>

    let inline andThen ([<InlineIfLambda>] binder: 'T -> RPath<'U>) ([<InlineIfLambda>] m: RPath<'T>) : RPath<'U> =
        m >>= binder

    let inline pipeTo
        ([<InlineIfLambda>] right: 'Inner -> RPath<'Out>)
        ([<InlineIfLambda>] left: 'In -> RPath<'Inner>)
        : 'In -> RPath<'Out> =
        left >=> right

    let inline children (deep: bool) (slot: Slot) : RPath<Slot> =
        fun link ->
            task {
                let! slot = getSlot slot.ID 1 deep link
                return slot.Children.AsEnumerable()
            }
            |> ValueTask<Slot seq>

    let inline childrenShallow slot : RPath<Slot> = children false slot
    let inline childrenDeep slot : RPath<Slot> = children true slot

    let rec collectChildrenRecursive (slot: Slot) =
        seq {
            if not (isNull slot.Children) then
                yield! slot.Children
                yield! slot.Children |> Seq.collect collectChildrenRecursive
        }

    let inline descendants (deep: bool) (slot: Slot) : RPath<Slot> =
        fun link ->
            task {
                let! slot = getSlot slot.ID (-1) deep link
                return collectChildrenRecursive slot
            }
            |> ValueTask<Slot seq>

    let inline descendantsShallow slot : RPath<Slot> = descendants false slot
    let inline descendantsDeep slot : RPath<Slot> = descendants true slot


    let inline parent deep (slot: Slot) : RPath<Slot> =
        if isNull slot.Parent.TargetID then
            fun _ -> ValueTask<Slot seq>(Seq.empty)
        else
            fun link ->
                task {
                    let! slot = getSlot slot.Parent.TargetID 0 deep link
                    return Seq.singleton slot
                }
                |> ValueTask<Slot seq>

    let inline parentShallow (slot: Slot) : RPath<Slot> = parent false slot
    let inline parentDeep (slot: Slot) : RPath<Slot> = parent true slot

    let inline ancestors deep (slot: Slot) : RPath<Slot> =
        fun link ->
            task {
                let mutable ancestorList: Slot list = []
                let mutable currentSlot: Slot = slot

                if currentSlot.IsReferenceOnly then
                    let! slot = getSlot slot.ID 0 deep link
                    currentSlot <- slot

                while not <| isNull currentSlot.Parent.TargetID do
                    let! slot = getSlot currentSlot.Parent.TargetID 0 deep link
                    currentSlot <- slot
                    ancestorList <- currentSlot :: ancestorList

                return ancestorList |> List.toSeq
            }
            |> ValueTask<Slot seq>

    let inline ancestorsShallow slot = ancestors false slot
    let inline ancestorsDeep slot = ancestors true slot

    let inline filter ([<InlineIfLambda>] predicate) ([<InlineIfLambda>] previousQuery: RPath<'T>) : RPath<'T> =
        // same as
        // previousQuery >>= (fun item -> if predicate item then wrap item else empty)
        fun link ->
            task {
                let! results = previousQuery link
                return results |> Seq.filter predicate
            }
            |> ValueTask<'T seq>

    let inline map ([<InlineIfLambda>] projection) ([<InlineIfLambda>] previousQuery: RPath<'T>) : RPath<'U> =
        // same as
        // previousQuery >>= (mapper >> wrap)
        fun link ->
            task {
                let! results = previousQuery link
                return results |> Seq.map projection
            }
            |> ValueTask<'U seq>

    let inline mapAll ([<InlineIfLambda>] projection) ([<InlineIfLambda>] previousQuery: RPath<'T>) : RPath<'U> =
        fun link ->
            task {
                let! results = previousQuery link
                return projection results
            }
            |> ValueTask<'U seq>

    let inline flatmap ([<InlineIfLambda>] mapper: 'I -> RPath<'T>) ([<InlineIfLambda>] previousQuery) : RPath<'T> =
        previousQuery >>= mapper

    let inline components (slot: Slot) : RPath<Component> =
        if
            not (
                slot.IsReferenceOnly
                || isNull slot.Components
                || (slot.Components.Count > 0 && slot.Components[0].IsReferenceOnly)
            )
        then
            fun _ -> ValueTask<Component seq>(slot.Components.AsEnumerable())
        else
            fun link ->
                task {
                    let! slot = getSlot slot.ID 0 true link
                    return slot.Components.AsEnumerable()
                }
                |> ValueTask<Component seq>

    let inline ofType (componentType: string) ([<InlineIfLambda>] components: RPath<Component>) : RPath<Component> =
        fun link ->
            task {
                let! results = components link
                return results |> Seq.filter (fun c -> c.ComponentType = componentType)
            }
            |> ValueTask<Component seq>

    let inline getMember<'T when 'T :> ResoniteLink.Member>
        (memberName: string)
        ([<InlineIfLambda>] previousQuery: RPath<Component>)
        : RPath<'T> =
        fun link ->
            task {
                let! results = previousQuery link

                return
                    results
                    |> Seq.map (fun c -> c.Members.TryGetValue memberName)
                    |> Seq.filter fst
                    |> Seq.map snd
                    |> Seq.choose (fun memberValue ->
                        match memberValue with
                        | :? 'T as value -> Some value
                        | _ -> None)
            }
            |> ValueTask<'T seq>

    let inline itemAt (index: int) ([<InlineIfLambda>] previousQuery: RPath<'T>) : RPath<'T> =
        fun link ->
            task {
                let! result = previousQuery link
                let normalizedIndex = if index < 0 then (Seq.length result) + index else index

                return
                    match Seq.tryItem normalizedIndex result with
                    | None -> Seq.empty
                    | Some value -> Seq.singleton value
            }
            |> ValueTask<'T seq>

    let inline take (count: int) ([<InlineIfLambda>] previousQuery: RPath<'T>) : RPath<'T> =
        fun link ->
            task {
                let! result = previousQuery link
                return Seq.take count result
            }
            |> ValueTask<'T seq>

    let inline skip (count: int) ([<InlineIfLambda>] previousQuery: RPath<'T>) : RPath<'T> =
        fun link ->
            task {
                let! result = previousQuery link
                return Seq.skip count result
            }
            |> ValueTask<'T seq>

    let inline slice (start: int, stop: int) ([<InlineIfLambda>] previousQuery: RPath<'T>) : RPath<'T> =
        fun link ->
            task {
                let! result = previousQuery link

                let normalizeIndex index =
                    if index < 0 then (Seq.length result) + index else index

                let normalizedStart = normalizeIndex start
                let normalizedStop = normalizeIndex stop

                return
                    result
                    |> Seq.skip normalizedStart
                    |> Seq.take (normalizedStop - normalizedStart)
            }
            |> ValueTask<'T seq>

    let inline dereference
        (dereferenceFunc:
            ILinkInterface -> string -> Task<'T>
                when 'T: (member Success: bool) and 'T: (member ErrorInfo: string) and 'T: (member Data: 'U))
        (referenceValue: Reference)
        : RPath<'U> =
        fun link ->
            if isNull referenceValue.TargetID then
                ValueTask<'U seq>(Seq.empty)
            else
                task {
                    let! dereferencedValue = dereferenceFunc link referenceValue.TargetID

                    if dereferencedValue.Success then
                        return Seq.singleton dereferencedValue.Data
                    else
                        return raise (ResoniteLinkException dereferencedValue.ErrorInfo)
                }
                |> ValueTask<'U seq>

    let inline dereferenceSlot deep (referenceValue: Reference) : RPath<Slot> =
        let getSlot (link: ILinkInterface) slotID =
            link.GetSlotData(GetSlot(SlotID = slotID, Depth = 0, IncludeComponentData = deep))

        dereference getSlot referenceValue

    let inline dereferenceSlotShallow referenceValue = dereferenceSlot false referenceValue
    let inline dereferenceSlotDeep referenceValue = dereferenceSlot true referenceValue

    let inline dereferenceComponent (referenceValue: Reference) : RPath<Component> =
        let getComponent (link: ILinkInterface) componentID =
            link.GetComponentData(GetComponent(ComponentID = componentID))

        dereference getComponent referenceValue

    let inline CastQuery<'T> ([<InlineIfLambda>] previousQuery: RPath<obj>) : RPath<'T> =
        fun link ->
            task {
                match! previousQuery link with
                | :? System.Collections.Generic.IEnumerable<'T> as properType -> return properType
                | _ -> return Seq.empty
            }
            |> ValueTask<'T seq>

    let inline toResultAsync (link: ILinkInterface) ([<InlineIfLambda>] query: RPath<'T>) =
        task {
            try
                let! result = query link
                return Result.Ok result
            with :? ResoniteLinkException as e ->
                return Result.Error e
        }
        |> ValueTask<Result<'T seq, ResoniteLinkException>>

    let inline toSeqAsync (link: ILinkInterface) ([<InlineIfLambda>] query: RPath<'T>) = query link

    let inline toArrayAsync (link: ILinkInterface) (query: RPath<'T>) =
        task {
            let! items = query link
            return (Seq.toArray items)
        }
        |> ValueTask<'T[]>

    let inline toResizeArray (link: ILinkInterface) (query: RPath<'T>) =
        task {
            let! items = query link
            return (ResizeArray items)
        }
        |> ValueTask<ResizeArray<'T>>

    let root: RPath<Slot> =
        fun link ->
            task {
                let! rootSlot = getSlot "Root" 0 false link
                return Seq.singleton rootSlot
            }
            |> ValueTask<Slot seq>
