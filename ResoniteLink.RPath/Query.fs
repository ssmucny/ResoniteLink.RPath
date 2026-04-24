namespace ResoniteLink.RPath

open System
open System.Threading.Tasks
open ResoniteLink

/// <summary>
/// The core query type representing a composable, lazy query over the ResoniteLink data model.
/// </summary>
/// <typeparam name="T">The type of elements produced by this query.</typeparam>
/// <remarks>
/// Query values are composable using monadic bind operations and can be executed lazily.
/// Query is a monad stack that includes asynchronous operations (ValueTask) and sequences (seq).
/// </remarks>
[<Struct>]
type Query<'T> =
    { RunQuery: LinkInterface -> ValueTask<'T seq> }


/// <summary>
/// Exception thrown when a ResoniteLink operation fails.
/// </summary>
type ResoniteLinkException(errorMessage) =
    inherit Exception(errorMessage)

/// <summary>
/// Core module containing functions for building and composing RPath queries.
/// </summary>
/// <remarks>
/// <para>RPath provides a LINQ-like query EDSL for ResoniteLink, enabling ergonomic exploration of the data model hierarchy.</para>
/// <para>Queries are lazy and composable, and they communicate with ResoniteLink on demand as data is needed.</para>
/// </remarks>
module Query =

    let inline private q ([<InlineIfLambda>] run) : Query<'T> = { RunQuery = run }

    let inline private nonNullValues (items: seq<'T>) = items |> Seq.choose Option.ofObj

    let private trySlotId (slot: Slot) =
        slot
        |> Option.ofObj
        |> Option.map (fun nonNullSlot ->
            assert (not <| isNull nonNullSlot.ID)
            nonNullSlot.ID)

    let private slotIDsOf (slots: seq<Slot>) = slots |> Seq.choose trySlotId

    let private tryReferenceTargetId (reference: Reference) =
        reference
        |> Option.ofObj
        |> Option.bind (fun nonNullReference -> nonNullReference.TargetID |> Option.ofObj)

    let private mapById (slots: seq<Slot>) =
        slots
        |> Seq.choose (fun slot ->
            slot
            |> Option.ofObj
            |> Option.map (fun nonNullSlot ->
                assert (not <| isNull nonNullSlot.ID)
                nonNullSlot.ID, nonNullSlot))
        |> Map.ofSeq

    // Null for root (Parent.TargetID = null signals no parent); Slot.Parent itself is never null from API.
    let private tryParentId (slot: Slot) =
        if isNull slot then
            None
        else
            assert (not <| isNull slot.Parent)
            slot.Parent.TargetID |> Option.ofObj

    // Children is null for leaf slots and reference-only slots
    let private slotChildrenOrEmpty (slot: Slot) : seq<Slot> =
        if isNull slot || isNull slot.Children then
            Seq.empty
        else
            slot.Children :> seq<Slot>

    // Components is null on reference-only stubs that fail hydration
    let private slotComponentsOrEmpty (slot: Slot) : seq<Component> =
        if isNull slot || isNull slot.Components then
            Seq.empty
        else
            slot.Components :> seq<Component>

    /// <summary>Internal helper to raise an exception for unexpected API response types.</summary>
    let illegalApiResponse (response: Response) =
        let responseType =
            if isNull response then
                "<null>"
            else
                response.GetType().FullName

        raise (Exception $"Illegal ResoniteLink API response type: {responseType}")

    /// <summary>Active pattern for matching ResoniteLink response types.</summary>
    let (|FailedResponse|SlotResponse|ComponentResponse|AssetResponse|) (response: Response) =
        match response with
        // A null response entry is treated as a protocol/API contract violation.
        | null -> invalidArg (nameof response) "The provided message is null."
        | r when not r.Success -> FailedResponse r.ErrorInfo
        | :? SlotData as s -> SlotResponse s.Data
        | :? ComponentData as c -> ComponentResponse c.Data
        | :? AssetData as a -> AssetResponse a.AssetURL
        | otherwise ->
            invalidArg
                (nameof response)
                $"The provided message is not a supported subtype: %s{otherwise.GetType().FullName}"

    /// <summary>
    /// Retrieves a slot from the data model with the specified options.
    /// </summary>
    let private getSlot slotID depth includeComponentData (link: LinkInterface) =
        task {
            let! result =
                link.GetSlotData(GetSlot(SlotID = slotID, Depth = depth, IncludeComponentData = includeComponentData))

            if result.Success then
                return result.Data
            else
                return raise (ResoniteLinkException result.ErrorInfo)
        }

    /// <summary>
    /// Wraps a single value into a Query that always returns that value.
    /// </summary>
    /// <remarks>This is the pure/return function for the Query monad</remarks>
    let inline wrap value : Query<'T> =
        q (fun _ -> value |> Seq.singleton |> ValueTask<_>)

    /// <summary>
    /// An empty Query that returns no results.
    /// </summary>
    let inline empty<'T> : Query<'T> = q (fun _ -> Seq.empty |> ValueTask<_>)

    /// <summary>
    /// Applies a transformation function to all results of a query as a single sequence.
    /// </summary>
    /// <remarks>This operation only maps over the ValueTask part of the Query</remarks>
    let inline mapAll ([<InlineIfLambda>] projection) (query: Query<'T>) : Query<'U> =
        q (fun link ->
            task {
                let! results = query.RunQuery link
                return projection results
            }
            |> ValueTask<'U seq>)

    /// <summary>
    /// Monadic bind operation. For each result of the source query, applies a function that returns a new query,
    /// then flattens all results.
    /// </summary>
    /// <remarks>This operation may trigger additional requests to the ResoniteLink data model.</remarks>
    let inline bind ([<InlineIfLambda>] binder: 'T -> Query<'U>) (query: Query<'T>) : Query<'U> =
        q (fun link ->
            task {
                let! boundValue = query.RunQuery link

                let! secondaryBoundValues =
                    boundValue
                    |> Seq.map (fun result -> ((binder result).RunQuery link).AsTask())
                    |> Task.WhenAll

                return secondaryBoundValues |> Seq.collect id
            }
            |> ValueTask<'U seq>)

    /// <summary>
    /// Applies a continuation function to the complete result sequence of a query.
    /// </summary>
    /// <remarks>This operation lifts async functions on lists to Query. The restriction to list functions is for efficiency. Similar to bind, but just operates over the async monad dimension.</remarks>
    let inline andThen ([<InlineIfLambda>] continuation: 'T seq -> ValueTask<'U seq>) (query: Query<'T>) : Query<'U> =
        q (fun link ->
            task {
                let! result = query.RunQuery link
                return! continuation result
            }
            |> ValueTask<'U seq>)

    /// <summary>
    /// Maps each element to a sequence and flattens the results (SelectMany/flatMap operation).
    /// </summary>
    /// <remarks>This operation lifts list/flattening functions to Query. If you want a version that accepts a mapper that returns a task then use andThen or bind.
    /// This operation is strictly non-async: operates over only the list monad dimension.</remarks>
    let inline flatMap ([<InlineIfLambda>] mapper: 'T -> seq<'U>) (query: Query<'T>) : Query<'U> =
        query |> mapAll (Seq.collect mapper)

    /// <summary>
    /// Composes two query-producing functions (Kleisli composition).
    /// </summary>
    let inline pipeTo
        ([<InlineIfLambda>] right: 'Inner -> Query<'Out>)
        ([<InlineIfLambda>] left: 'In -> Query<'Inner>)
        : 'In -> Query<'Out> =
        fun input -> bind right (left input)

    /// <summary>
    /// Applies a projection function to each element in the query results.
    /// </summary>
    let inline map ([<InlineIfLambda>] projection) (query: Query<'T>) : Query<'U> = query |> mapAll (Seq.map projection)

    /// <summary>
    /// Filters query results to only include elements that satisfy the predicate.
    /// </summary>
    let inline filter ([<InlineIfLambda>] predicate) (query: Query<'T>) : Query<'T> =
        query |> mapAll (Seq.filter predicate)

    /// Internal helper to collect descendants from a fully loaded slot tree.
    let rec private collectChildrenRecursive (slot: Slot) =
        seq {
            // Null slot values here are only expected from user-supplied query streams, not API responses from Resonite
            if not (isNull slot) then
                let children = slotChildrenOrEmpty slot
                yield! children
                yield! children |> Seq.collect collectChildrenRecursive
        }

    /// Internal helper for batched GetSlot requests.
    let private runGetSlotBatch
        (includeComponents: bool)
        (depth: int)
        (slotIDs: string array)
        (link: LinkInterface)
        : ValueTask<Slot array> =
        task {
            if Array.isEmpty slotIDs then
                return [||]
            else
                let batchRequest =
                    ResizeArray(
                        seq {
                            for slotID in slotIDs do
                                GetSlot(SlotID = slotID, Depth = depth, IncludeComponentData = includeComponents)
                                :> DataModelOperation
                        }
                    )

                let! batchResponse = link.RunDataModelOperationBatch batchRequest

                if not batchResponse.Success then
                    return raise (ResoniteLinkException batchResponse.ErrorInfo)
                else
                    return
                        [| for response in batchResponse.Responses do
                               match response with
                               | SlotResponse slot -> yield slot
                               | FailedResponse errorInfo -> raise (ResoniteLinkException errorInfo)
                               | _ -> illegalApiResponse response |]
        }
        |> ValueTask<Slot[]>

    /// Internal helper for batched GetComponent requests.
    let private runGetComponentBatch (componentIDs: string[]) (link: LinkInterface) =
        task {
            if Array.isEmpty componentIDs then
                return [||]
            else
                let batchRequest =
                    ResizeArray(
                        seq {
                            for componentID in componentIDs do
                                GetComponent(ComponentID = componentID) :> DataModelOperation
                        }
                    )

                let! batchResponse = link.RunDataModelOperationBatch batchRequest

                if not batchResponse.Success then
                    return raise (ResoniteLinkException batchResponse.ErrorInfo)
                else
                    return
                        [| for response in batchResponse.Responses do
                               match response with
                               | ComponentResponse foundComponent -> yield foundComponent
                               | FailedResponse errorInfo -> raise (ResoniteLinkException errorInfo)
                               | _ -> illegalApiResponse response |]
        }
        |> ValueTask<Component[]>

    /// Internal helper to identify slots that need hydration before components are available.
    let private requiresComponentHydration (slot: Slot) =
        slot.IsReferenceOnly
        // Components can be null on reference-only stubs until hydrated.
        || isNull slot.Components
        || (slot.Components
            |> Seq.tryHead
            |> Option.map _.IsReferenceOnly
            |> Option.defaultValue false)

    /// Internal helper to hydrate reference-only slots so parent pointers are available.
    let private hydrateReferenceOnlySlots (slots: Slot[]) (link: LinkInterface) : Task<Slot[]> =
        task {
            let referenceOnlyIDs =
                slots |> Array.filter _.IsReferenceOnly |> Array.map _.ID |> Array.distinct

            let! hydratedReferenceSlots = runGetSlotBatch false 0 referenceOnlyIDs link
            let hydratedByID = hydratedReferenceSlots |> mapById

            return
                slots
                |> Array.map (fun slot ->
                    if slot.IsReferenceOnly then
                        hydratedByID |> Map.tryFind slot.ID |> Option.defaultValue slot
                    else
                        slot)
        }

    /// Internal helper to walk up the hierarchy and collect all ancestors.
    let private walkAncestors
        (includeComponents: bool)
        (sourceSlots: Slot array)
        (link: LinkInterface)
        : Task<Slot array> =
        task {
            let ancestors = ResizeArray<Slot>()
            let mutable visitedAncestorIDs = Set.empty<string>
            let mutable frontier = sourceSlots |> Array.choose tryParentId |> Array.distinct

            while not (Array.isEmpty frontier) do
                let parentIDs =
                    frontier
                    |> Array.filter (fun parentId -> not (visitedAncestorIDs.Contains parentId))

                visitedAncestorIDs <- Set.union visitedAncestorIDs (Set.ofArray parentIDs)

                let! parentSlots = runGetSlotBatch includeComponents 0 parentIDs link
                ancestors.AddRange parentSlots
                frontier <- parentSlots |> Array.choose tryParentId |> Array.distinct

            return ancestors.ToArray()
        }

    /// <summary>
    /// Gets the direct children returned from a query of slots.
    /// </summary>
    /// <remarks>Slots with <c>null</c> child collections are treated as having no children.</remarks>
    let children (includeComponents: bool) (slotQuery: Query<Slot>) : Query<Slot> =
        q (fun link ->
            task {
                let! parentSlots = slotQuery.RunQuery link
let parentSlotIDs =
    // Protect against null slots in user code.
    parentSlots |> slotIDsOf |> Seq.toArray
                let! hydratedParents = runGetSlotBatch includeComponents 1 parentSlotIDs link

                return
                    seq {
                        for slot in hydratedParents do
                            yield! slotChildrenOrEmpty slot
                    }
            }
            |> ValueTask<Slot seq>)

    /// <summary>
    /// Gets direct children returned from a query of slots that satisfy a predicate.
    /// </summary>
    let inline child
        ([<InlineIfLambda>] childPredicate)
        (includeComponents: bool)
        (slotQuery: Query<Slot>)
        : Query<Slot> =
        slotQuery |> children includeComponents |> filter childPredicate

    /// <summary>
    /// Gets the direct children returned from a query of slots without component data.
    /// </summary>
    let childrenLite (slotQuery: Query<Slot>) : Query<Slot> = children false slotQuery

    /// <summary>
    /// Gets the direct children returned from a query of slots with full component data.
    /// </summary>
    let childrenFull (slotQuery: Query<Slot>) : Query<Slot> = children true slotQuery

    /// <summary>
    /// Gets all descendants from each slot returned by a slot query.
    /// </summary>
    let descendants (includeComponents: bool) (slotQuery: Query<Slot>) : Query<Slot> =
        q (fun link ->
            task {
                let! sourceSlots = slotQuery.RunQuery link
                // Null slots here are defensive handling for user-constructed query streams.
                let sourceSlotIDs = sourceSlots |> slotIDsOf |> Seq.toArray
                let! hydratedTrees = runGetSlotBatch includeComponents -1 sourceSlotIDs link
                return hydratedTrees |> Seq.collect collectChildrenRecursive
            }
            |> ValueTask<Slot seq>)

    /// <summary>
    /// Gets all descendants from each slot in a query without component data.
    /// </summary>
    let descendantsLite (slotQuery: Query<Slot>) : Query<Slot> = descendants false slotQuery

    /// <summary>
    /// Gets all descendants from each slot in a query with full component data.
    /// </summary>
    let descendantsFull (slotQuery: Query<Slot>) : Query<Slot> = descendants true slotQuery

    /// <summary>
    /// Gets each source slot and all of its descendants from a slot query.
    /// </summary>
    let descendantsAndSelf (includeComponents: bool) (slotQuery: Query<Slot>) : Query<Slot> =
        q (fun link ->
            task {
                let! sourceSlots = slotQuery.RunQuery link
                // Null slots here are defensive handling for user-constructed query streams.
                let sourceSlotIDs = sourceSlots |> slotIDsOf |> Seq.toArray
                let! hydratedTrees = runGetSlotBatch includeComponents -1 sourceSlotIDs link

                return
                    seq {
                        for root in hydratedTrees do
                            yield root
                            yield! collectChildrenRecursive root
                    }
            }
            |> ValueTask<Slot seq>)

    /// <summary>
    /// Gets each source slot and all of its descendants without component data.
    /// </summary>
    let descendantsAndSelfLite (slotQuery: Query<Slot>) : Query<Slot> = descendantsAndSelf false slotQuery

    /// <summary>
    /// Gets each source slot and all of its descendants with full component data.
    /// </summary>
    let descendantsAndSelfFull (slotQuery: Query<Slot>) : Query<Slot> = descendantsAndSelf true slotQuery

    /// <summary>
    /// Gets the parent for each slot returned by a slot query.
    /// </summary>
    let parent (includeComponents: bool) (slotQuery: Query<Slot>) : Query<Slot> =
        q (fun link ->
            task {
                let! sourceSlots = slotQuery.RunQuery link
                // Null slots here are defensive handling for user-constructed query streams.
                let sourceSlotsArray = sourceSlots |> nonNullValues |> Seq.toArray
                let! hydratedSources = hydrateReferenceOnlySlots sourceSlotsArray link

                let parentIDsInOrder = hydratedSources |> Array.choose tryParentId

                let uniqueParentIDs = parentIDsInOrder |> Array.distinct
                let! parentSlots = runGetSlotBatch includeComponents 0 uniqueParentIDs link
                let parentByID = parentSlots |> mapById

                return
                    seq {
                        for parentID in parentIDsInOrder do
                            match parentByID |> Map.tryFind parentID with
                            | Some parentSlot -> yield parentSlot
                            | None -> ()
                    }
            }
            |> ValueTask<Slot seq>)

    /// <summary>
    /// Gets the parent for each slot in a query without component data.
    /// </summary>
    let parentLite (slotQuery: Query<Slot>) : Query<Slot> = parent false slotQuery

    /// <summary>
    /// Gets the parent for each slot in a query with full component data.
    /// </summary>
    let parentFull (slotQuery: Query<Slot>) : Query<Slot> = parent true slotQuery

    /// <summary>
    /// Gets all ancestors for each slot returned by a slot query.
    /// </summary>
    let ancestors (includeComponents: bool) (slotQuery: Query<Slot>) : Query<Slot> =
        q (fun link ->
            task {
                let! sourceSlots = slotQuery.RunQuery link
                // Null slots here are defensive handling for user-constructed query streams.
                let sourceSlotsArray = sourceSlots |> nonNullValues |> Seq.toArray
                let! hydratedSources = hydrateReferenceOnlySlots sourceSlotsArray link

                let! ancestorSlots = walkAncestors includeComponents hydratedSources link
                return ancestorSlots :> seq<Slot>
            }
            |> ValueTask<Slot seq>)

    /// <summary>
    /// Gets all ancestors for each slot in a query without component data.
    /// </summary>
    let ancestorsLite (slotQuery: Query<Slot>) = ancestors false slotQuery

    /// <summary>
    /// Gets all ancestors for each slot in a query with full component data.
    /// </summary>
    let ancestorsFull (slotQuery: Query<Slot>) = ancestors true slotQuery

    /// <summary>
    /// Gets each source slot and all of its ancestors from a slot query.
    /// </summary>
    let ancestorsAndSelf (includeComponents: bool) (slotQuery: Query<Slot>) : Query<Slot> =
        q (fun link ->
            task {
                let! sourceSlots = slotQuery.RunQuery link
                // Null slots here are defensive handling for user-constructed query streams.
                let sourceSlotsArray = sourceSlots |> nonNullValues |> Seq.toArray
                let! hydratedSources = hydrateReferenceOnlySlots sourceSlotsArray link
                let! ancestorSlots = walkAncestors includeComponents hydratedSources link

                return
                    seq {
                        yield! sourceSlotsArray
                        yield! ancestorSlots
                    }
            }
            |> ValueTask<Slot seq>)

    /// <summary>
    /// Gets each source slot and all of its ancestors without component data.
    /// </summary>
    let ancestorsAndSelfLite (slotQuery: Query<Slot>) : Query<Slot> = ancestorsAndSelf false slotQuery

    /// <summary>
    /// Gets each source slot and all of its ancestors with full component data.
    /// </summary>
    let ancestorsAndSelfFull (slotQuery: Query<Slot>) : Query<Slot> = ancestorsAndSelf true slotQuery

    /// <summary>
    /// Gets the components attached to each slot in a query.
    /// </summary>
    /// <remarks>
    /// If component data is not already loaded, this batches hydration requests by slot ID.
    /// Slots with <c>null</c> component collections are treated as having no components.
    /// </remarks>
    let components (slotQuery: Query<Slot>) : Query<Component> =
        q (fun link ->
            task {
                let! sourceSlots = slotQuery.RunQuery link
                // Null slots here are defensive handling for user-constructed query streams.
                let sourceSlotsArray = sourceSlots |> nonNullValues |> Seq.toArray

                let slotsToHydrateIDs =
                    sourceSlotsArray
                    |> Array.filter requiresComponentHydration
                    |> Array.map _.ID
                    |> Array.distinct

                let! hydratedSlots = runGetSlotBatch true 0 slotsToHydrateIDs link
                let hydratedByID = hydratedSlots |> mapById

                return
                    seq {
                        for slot in sourceSlotsArray do
                            let resolvedSlot =
                                if requiresComponentHydration slot then
                                    hydratedByID |> Map.tryFind slot.ID |> Option.defaultValue slot
                                else
                                    slot

                            yield! slotComponentsOrEmpty resolvedSlot
                    }
            }
            |> ValueTask<Component seq>)

    /// <summary>
    /// Filters components to only include those of the specified type.
    /// </summary>
    let inline ofType (componentType: string) (components: Query<Component>) : Query<Component> =
        components |> mapAll (Seq.filter (fun c -> c.ComponentType = componentType))

    /// <summary>
    /// Gets a member from each component by name and casts it to the specified type.
    /// </summary>
    /// <remarks>Components without a matching member or with a member of incompatible type are excluded from results.</remarks>
    let inline getMember<'T when 'T :> ResoniteLink.Member> (memberName: string) (query: Query<Component>) : Query<'T> =
        query
        |> mapAll (
            Seq.choose (fun c ->
                // Components in query streams can be null from user-provided values; Members can be null on partial payloads.
                if isNull c || isNull c.Members then
                    None
                else
                    match c.Members.TryGetValue memberName with
                    | true, value -> Some value
                    | false, _ -> None)
            >> Seq.choose (function
                | :? 'T as value -> Some value
                | _ -> None)
        )

    /// <summary>
    /// Gets a single element at the specified index from the query results.
    /// </summary>
    let inline itemAt (index: int) (query: Query<'T>) : Query<'T> =
        query
        |> mapAll (fun result ->
            match Seq.tryItem index result with
            | None -> Seq.empty
            | Some value -> Seq.singleton value)

    /// <summary>
    /// Takes the first <paramref name="count"/> elements from the query results.
    /// </summary>
    /// <param name="count">The number of elements to take from the start of the result sequence.</param>
    /// <param name="query">The source query.</param>
    /// <returns>A query yielding the first <paramref name="count"/> results.</returns>
    /// <remarks>
    /// Uses <see cref="M:Microsoft.FSharp.Collections.SeqModule.Take"/> semantics.
    /// If fewer than <paramref name="count"/> results are available, an exception is raised during execution.
    /// </remarks>
    let inline take (count: int) (query: Query<'T>) : Query<'T> = query |> mapAll (Seq.take count)

    /// <summary>
    /// Skips the first <paramref name="count"/> elements from the query results.
    /// </summary>
    /// <param name="count">The number of elements to skip from the start of the result sequence.</param>
    /// <param name="query">The source query.</param>
    /// <returns>A query yielding the remaining elements after the skip.</returns>
    /// <remarks>
    /// Uses <see cref="M:Microsoft.FSharp.Collections.SeqModule.Skip"/> semantics.
    /// If fewer than <paramref name="count"/> results are available, an exception is raised during execution.
    /// </remarks>
    let inline skip (count: int) (query: Query<'T>) : Query<'T> = query |> mapAll (Seq.skip count)

    /// <summary>
    /// Gets a slice of elements from <paramref name="start"/> (inclusive) to <paramref name="stop"/> (exclusive).
    /// </summary>
    /// <param name="start">Zero-based inclusive start index.</param>
    /// <param name="stop">Zero-based exclusive stop index.</param>
    /// <param name="query">The source query.</param>
    /// <returns>A query yielding elements in the requested index range.</returns>
    /// <remarks>
    /// This is equivalent to applying <c>skip start</c> followed by <c>take (stop - start)</c>.
    /// </remarks>
    let inline slice (start: int, stop: int) (query: Query<'T>) : Query<'T> =
        query
        |> mapAll (fun result -> result |> Seq.skip start |> Seq.take (stop - start))

    /// <summary>
    /// Dereferences slot references to get target slots.
    /// </summary>
    let dereferenceSlot includeComponents (referenceQuery: Query<Reference>) : Query<Slot> =
        q (fun link ->
            task {
                let! references = referenceQuery.RunQuery link

                let uniqueSlotsToGet =
                    references
                    // Reference values can be null in user-provided query streams; TargetID null means unresolved reference.
                    |> Seq.choose tryReferenceTargetId
                    |> Seq.toArray
                    |> Array.distinct

                let! targetSlots = runGetSlotBatch includeComponents 0 uniqueSlotsToGet link

                return targetSlots :> seq<Slot>
            }
            |> ValueTask<Slot seq>)

    /// <summary>
    /// Dereferences slot references without component data.
    /// </summary>
    let dereferenceSlotLite (referenceQuery: Query<Reference>) = dereferenceSlot false referenceQuery

    /// <summary>
    /// Dereferences slot references with full component data.
    /// </summary>
    let dereferenceSlotFull (referenceQuery: Query<Reference>) = dereferenceSlot true referenceQuery

    /// <summary>
    /// Dereferences component references to get target components.
    /// </summary>
    let dereferenceComponent (referenceQuery: Query<Reference>) : Query<Component> =
        q (fun link ->
            task {
                let! references = referenceQuery.RunQuery link

                let uniqueComponentsToGet =
                    references
                    // Reference values can be null in user-provided query streams; TargetID null means unresolved reference.
                    |> Seq.choose tryReferenceTargetId
                    |> Seq.toArray
                    |> Array.distinct

                let! targetComponents = runGetComponentBatch uniqueComponentsToGet link

                return targetComponents :> seq<Component>
            }
            |> ValueTask<Component seq>)

    /// <summary>
    /// Tries to cast each element in an object query to a specific type, dropping non-matching values.
    /// </summary>
    let inline tryCast<'T> (query: Query<obj>) : Query<'T> =
        query
        |> mapAll (
            Seq.choose (function
                | :? 'T as properType -> Some properType
                | _ -> None)
        )

    /// <summary>
    /// Casts each element in an object query to a specific type.
    /// </summary>
    /// <exception cref="InvalidCastException">Thrown when any element cannot be cast to the requested type.</exception>
    let inline cast<'T> (query: Query<obj>) : Query<'T> = query |> mapAll Seq.cast<'T>

    /// <summary>
    /// Executes a query and returns at most the first result only if it exists.
    /// </summary>
    /// <param name="query">The query to execute.</param>
    /// <param name="link">The active ResoniteLink interface used to execute the query.</param>
    /// <returns>The first result wrapped in <c>Some</c>, or <c>None</c> when the query is empty.</returns>
    /// <exception cref="ResoniteLinkException">Thrown when a ResoniteLink operation fails.</exception>
    let inline first (query: Query<'T>) =
        q (fun link ->
            task {
                let! items = query.RunQuery link
                return Seq.truncate 1 items
            }
            |> ValueTask<'T seq>)

    /// <summary>
    /// Executes a query and returns the first result, or <paramref name="defaultValue"/> when no values are produced.
    /// </summary>
    /// <param name="defaultValue">The fallback value returned when the query is empty.</param>
    /// <param name="query">The query to execute.</param>
    /// <param name="link">The active ResoniteLink interface used to execute the query.</param>
    /// <returns>The first query result when present; otherwise <paramref name="defaultValue"/>.</returns>
    /// <exception cref="ResoniteLinkException">Thrown when a ResoniteLink operation fails.</exception>
    let inline firstOr (defaultValue: 'T) (query: Query<'T>) =
        q (fun link ->
            task {
                let! items = query.RunQuery link

                match Seq.tryHead items with
                | Some firstItem -> return Seq.singleton firstItem
                | None -> return Seq.singleton defaultValue
            }
            |> ValueTask<'T seq>)

    /// <summary>
    /// Executes a query and returns the result as a <c>Result</c>, capturing <see cref="T:ResoniteLink.RPath.ResoniteLinkException"/> failures.
    /// </summary>
    /// <remarks>
    /// Exceptions thrown by user-provided projection/filter code are not wrapped and continue to propagate.
    /// </remarks>
    let inline toResult (query: Query<'T>) (link: LinkInterface) =
        task {
            try
                let! result = query.RunQuery link
                return Result.Ok result
            with :? ResoniteLinkException as e ->
                return Result.Error e
        }
        |> ValueTask<Result<'T seq, ResoniteLinkException>>

    /// <summary>
    /// Executes a query and returns the results as a sequence.
    /// </summary>
    /// <exception cref="ResoniteLinkException">Thrown when a ResoniteLink operation fails.</exception>
    let inline toSeq (query: Query<'T>) (link: LinkInterface) = query.RunQuery link

    /// <summary>
    /// Executes a query and returns the results as an array.
    /// </summary>
    /// <exception cref="ResoniteLinkException">Thrown when a ResoniteLink operation fails.</exception>
    let inline toArray (query: Query<'T>) (link: LinkInterface) =
        task {
            let! items = query.RunQuery link
            return (Seq.toArray items)
        }
        |> ValueTask<'T[]>

    /// <summary>
    /// Executes a query and returns the results as a ResizeArray (List&lt;T&gt;).
    /// </summary>
    /// <exception cref="ResoniteLinkException">Thrown when a ResoniteLink operation fails.</exception>
    let inline toResizeArray (query: Query<'T>) (link: LinkInterface) =
        task {
            let! items = query.RunQuery link
            return (ResizeArray items)
        }
        |> ValueTask<ResizeArray<'T>>

    /// <summary>
    /// Executes a query and returns the single result when exactly one value is produced, or raises an exception otherwise.
    /// </summary>
    /// <param name="query"></param>
    /// <param name="link"></param>
    let inline exactlyOne (query: Query<'T>) (link: LinkInterface) =
        task {
            let! items = query.RunQuery link
            return Seq.exactlyOne items
        }
        |> ValueTask<'T>

    /// <summary>
    /// Executes a query and returns the single result wrapped in <c>Some</c> when exactly one value is found, <c>None</c> when no values are produced or there is more than one.
    /// </summary>
    /// <param name="query"></param>
    /// <param name="link"></param>
    let inline tryExactlyOne (query: Query<'T>) (link: LinkInterface) =
        task {
            let! items = query.RunQuery link
            return Seq.tryExactlyOne items
        }
        |> ValueTask<'T option>

    /// <summary>
    /// A query that returns the root slot from the data model. Usable as an entry point from the base of the hierarchy.
    /// </summary>
    [<CompiledName "Root">] // The compiled name attribute allows this to be accessed as Query.Root in C# for ergonomic chaining.
    let root: Query<Slot> =
        q (fun link ->
            task {
                let! rootSlot = getSlot "Root" 0 false link
                return Seq.singleton rootSlot
            }
            |> ValueTask<Slot seq>)

    /// <summary>
    /// A query that returns the slot with the specified ID.
    /// </summary>
    /// <param name="slotID">The Resonite slot ID to retrieve.</param>
    /// <exception cref="ResoniteLinkException">Thrown when the slot cannot be resolved by ResoniteLink.</exception>
    [<CompiledName "FindSlotByID">]
    let findSlotByID (slotID: string) : Query<Slot> =
        q (fun link ->
            task {
                let! slot = getSlot slotID 0 false link
                return Seq.singleton slot
            }
            |> ValueTask<Slot seq>)

    /// <summary>
    /// A query that returns the component with the specified ID.
    /// </summary>
    /// <param name="componentID">The Resonite component ID to retrieve.</param>
    /// <exception cref="ResoniteLinkException">Thrown when the component cannot be resolved by ResoniteLink.</exception>
    [<CompiledName "FindComponentByID">]
    let findComponentByID (componentID: string) : Query<Component> =
        q (fun link ->
            task {
                let! comp = link.GetComponentData(GetComponent(ComponentID = componentID))

                if comp.Success then
                    return Seq.singleton comp.Data
                else
                    return raise (ResoniteLinkException comp.ErrorInfo)
            }
            |> ValueTask<Component seq>)

    /// <summary>
    /// Executes a query with a LinkInterface. Returns the result as a ValueTask.
    /// </summary>
    /// <remarks>This simply flips the arguments of the query function for ergonomic chaining with piping</remarks>
    let inline runAsync (link: LinkInterface) (query: Query<'T>) = query.RunQuery link

    /// <summary>
    /// Executes a query with a LinkInterface. Blocks until the query completes.
    /// </summary>
    /// <remarks>This simply flips the arguments of the query function for ergonomic chaining with piping</remarks>
    let inline run (link: LinkInterface) (query: Query<'T>) =
        query.RunQuery link |> _.AsTask() |> Async.AwaitTask |> Async.RunSynchronously

/// <summary>
/// Custom operators for composing RPath queries in F# using symbolic operators.
/// </summary>
/// <remarks>
/// <para>The <c>&gt;&gt;=</c> operator is the monadic bind operator that indicates where new data is fetched from the data model.</para>
/// <para>The <c>&gt;=&gt;</c> operator is the Kleisli composition operator for composing query functions.</para>
/// </remarks>
module Operators =
    /// <summary>
    /// Monadic bind operator. Sequences a query with a function that produces a new query for each result.
    /// </summary>
    /// <param name="m">The source query.</param>
    /// <param name="binder">A function that takes each result and returns a new query.</param>
    /// <returns>A new query that flattens the results of all bound queries.</returns>
    /// <remarks>Use this operator to indicate where new data is fetched from the ResoniteLink data model.</remarks>
    let inline (>>=) (m: Query<'T>) ([<InlineIfLambda>] binder: 'T -> Query<'U>) : Query<'U> = Query.bind binder m

    /// <summary>
    /// Kleisli composition operator. Composes two query-producing functions into a single function.
    /// </summary>
    /// <param name="left">The first query function to apply.</param>
    /// <param name="right">The second query function to apply to the results of the first.</param>
    /// <returns>A composed function that applies both query functions in sequence.</returns>
    let inline (>=>)
        ([<InlineIfLambda>] left: 'In -> Query<'Inner>)
        ([<InlineIfLambda>] right: 'Inner -> Query<'Out>)
        : 'In -> Query<'Out> =
        Query.pipeTo right left
