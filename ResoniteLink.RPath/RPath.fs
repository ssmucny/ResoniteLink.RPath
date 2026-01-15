namespace ResoniteLink.RPath

open System
open System.Linq
open System.Threading.Tasks
open ResoniteLink

/// <summary>Error information returned from failed ResoniteLink operations.</summary>
type ErrorInfo = string

/// <summary>
/// The core query type representing a composable, lazy query over the ResoniteLink data model.
/// </summary>
/// <typeparam name="T">The type of elements produced by this query.</typeparam>
/// <remarks>
/// RPath queries are functions that take an ILinkInterface and return a sequence of results.
/// They are composable using monadic bind operations and can be executed lazily.
/// RPath is a monad stack that includes asynchronous operations (ValueTask) and sequences (seq).
/// </remarks>
type RPath<'T> = ILinkInterface -> ValueTask<'T seq>

/// <summary>
/// Exception thrown when a ResoniteLink operation fails.
/// </summary>
type ResoniteLinkException(errorMessage) =
    inherit Exception(errorMessage)

    /// <summary>
    /// Raises a ResoniteLinkException with the specified message.
    /// </summary>
    /// <param name="message">The error message.</param>
    /// <returns>An empty sequence (never returns, always throws).</returns>
    static member Raise(message) =
        raise (ResoniteLinkException(message))
        Seq.empty

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

    /// <summary>
    /// Kleisli composition operator. Composes two query-producing functions into a single function.
    /// </summary>
    /// <param name="left">The first query function to apply.</param>
    /// <param name="right">The second query function to apply to the results of the first.</param>
    /// <returns>A composed function that applies both query functions in sequence.</returns>
    let inline (>=>)
        ([<InlineIfLambda>] left: 'In -> RPath<'Inner>)
        ([<InlineIfLambda>] right: 'Inner -> RPath<'Out>)
        : 'In -> RPath<'Out> =
        fun input -> left input >>= right

/// <summary>
/// Core module containing functions for building and composing RPath queries.
/// </summary>
/// <remarks>
/// <para>RPath provides a LINQ-like query EDSL for ResoniteLink, enabling ergonomic exploration of the data model hierarchy.</para>
/// <para>Queries are lazy and composable, and they communicate with ResoniteLink on demand as data is needed.</para>
/// </remarks>
module RPath =
    open Operators

    /// <summary>Internal helper to raise an exception for unexpected API response types.</summary>
    let illegalApiResponse response =
        raise (Exception $"Illegal ResoniteLink API response type: {response.GetType().FullName}")

    /// <summary>Active pattern for matching ResoniteLink response types.</summary>
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

    /// <summary>
    /// Retrieves a slot from the data model with the specified options.
    /// </summary>
    /// <param name="slotID">The ID of the slot to retrieve.</param>
    /// <param name="depth">The depth of child slots to include (-1 for all descendants, 0 for none, 1 for direct children, etc.).</param>
    /// <param name="includeComponentData">Whether to include full component data in the response.</param>
    /// <param name="link">The link interface to use for the request.</param>
    /// <returns>A task containing the retrieved slot.</returns>
    let getSlot slotID depth includeComponentData (link: ILinkInterface) =
        task {
            let! result =
                link.GetSlotData(GetSlot(SlotID = slotID, Depth = depth, IncludeComponentData = includeComponentData))

            if result.Success then
                return result.Data
            else
                return raise (ResoniteLinkException result.ErrorInfo)
        }

    /// <summary>
    /// Wraps a single value into an RPath query that always returns that value.
    /// </summary>
    /// <param name="value">The value to wrap.</param>
    /// <returns>An RPath query that returns the wrapped value.</returns>
    /// <remarks>This is the pure/return function for the RPath monad</remarks>
    let inline wrap value : RPath<'T> =
        fun _ -> value |> Seq.singleton |> ValueTask<_>

    /// <summary>
    /// An empty RPath query that returns no results.
    /// </summary>
    let empty: RPath<_> = fun _ -> Seq.empty |> ValueTask<_>

    /// <summary>
    /// Applies a transformation function to all results of a query as a single sequence.
    /// </summary>
    /// <param name="projection">The function to apply to the result sequence.</param>
    /// <param name="query">The source query.</param>
    /// <returns>A new query with the transformed results.</returns>
    /// <remarks>This operation only maps over the ValueTask part of the RPath</remarks>
    let inline mapAll ([<InlineIfLambda>] projection) ([<InlineIfLambda>] query: RPath<'T>) : RPath<'U> =
        fun link ->
            task {
                let! results = query link
                return projection results
            }
            |> ValueTask<'U seq>

    /// <summary>
    /// Monadic bind operation. For each result of the source query, applies a function that returns a new query,
    /// then flattens all results.
    /// </summary>
    /// <param name="binder">A function that takes each result and returns a new query.</param>
    /// <param name="query">The source query.</param>
    /// <returns>A new query with flattened results from all bound queries.</returns>
    /// <remarks>This operation may trigger additional requests to the ResoniteLink data model.</remarks>
    let inline bind ([<InlineIfLambda>] binder: 'T -> RPath<'U>) ([<InlineIfLambda>] query: RPath<'T>) : RPath<'U> =
        query >>= binder

    /// <summary>
    /// Applies a continuation function to the complete result sequence of a query.
    /// </summary>
    /// <param name="continuation">A function that takes the result sequence and returns a new async sequence.</param>
    /// <param name="query">The source query.</param>
    /// <returns>A new query with results from the continuation.</returns>
    /// <remarks>This operation lifts async functions on lists to RPath. The restriction to list functions is for efficiency</remarks>
    let inline andThen
        ([<InlineIfLambda>] continuation: 'T seq -> ValueTask<'U seq>)
        ([<InlineIfLambda>] query: RPath<'T>)
        : RPath<'U> =
        fun link ->
            task {
                let! result = query link
                return! continuation result
            }
            |> ValueTask<'U seq>

    /// <summary>
    /// Maps each element to a sequence and flattens the results (SelectMany/flatMap operation).
    /// </summary>
    /// <param name="mapper">A function that maps each element to a sequence.</param>
    /// <param name="query">The source query.</param>
    /// <returns>A new query with flattened results.</returns>
    /// <remarks>This operation lifts list/flattening functions to RPath</remarks>
    let inline flatmap ([<InlineIfLambda>] mapper: 'T -> seq<'U>) ([<InlineIfLambda>] query) : RPath<'U> =
        query >>= (fun item -> fun _ -> item |> Seq.collect mapper |> ValueTask<_>)

    /// <summary>
    /// Composes two query-producing functions (Kleisli composition).
    /// </summary>
    /// <param name="right">The second query function to apply.</param>
    /// <param name="left">The first query function to apply.</param>
    /// <returns>A composed function that applies both query functions in sequence.</returns>
    let inline pipeTo
        ([<InlineIfLambda>] right: 'Inner -> RPath<'Out>)
        ([<InlineIfLambda>] left: 'In -> RPath<'Inner>)
        : 'In -> RPath<'Out> =
        left >=> right

    /// <summary>
    /// Applies a projection function to each element in the query results.
    /// </summary>
    /// <param name="projection">The function to apply to each element.</param>
    /// <param name="query">The source query.</param>
    /// <returns>A new query with transformed elements.</returns>
    let inline map ([<InlineIfLambda>] projection) ([<InlineIfLambda>] query: RPath<'T>) : RPath<'U> =
        query |> mapAll (Seq.map projection)

    /// <summary>
    /// Filters query results to only include elements that satisfy the predicate.
    /// </summary>
    /// <param name="predicate">The function to test each element.</param>
    /// <param name="query">The source query.</param>
    /// <returns>A new query containing only elements that satisfy the predicate.</returns>
    let inline filter ([<InlineIfLambda>] predicate) ([<InlineIfLambda>] query: RPath<'T>) : RPath<'T> =
        query |> mapAll (Seq.filter predicate)

    /// <summary>
    /// Gets the direct children of a slot.
    /// </summary>
    /// <param name="deep">If true, includes full component data for each child slot.</param>
    /// <param name="slot">The parent slot.</param>
    /// <returns>A query returning the direct child slots.</returns>
    let children (deep: bool) (slot: Slot) : RPath<Slot> =
        fun link ->
            task {
                let! slot = getSlot slot.ID 1 deep link
                return slot.Children.AsEnumerable()
            }
            |> ValueTask<Slot seq>

    /// <summary>
    /// Gets the direct children of a slot without component data.
    /// </summary>
    /// <param name="slot">The parent slot.</param>
    /// <returns>A query returning the direct child slots (reference only, no component data).</returns>
    let childrenShallow slot : RPath<Slot> = children false slot

    /// <summary>
    /// Gets the direct children of a slot with full component data.
    /// </summary>
    /// <param name="slot">The parent slot.</param>
    /// <returns>A query returning the direct child slots with component data.</returns>
    let childrenDeep slot : RPath<Slot> = children true slot

    /// <summary>
    /// Recursively collects all children from a slot hierarchy (in-memory operation).
    /// </summary>
    /// <param name="slot">The root slot to collect children from.</param>
    /// <returns>A sequence of all descendant slots.</returns>
    let rec collectChildrenRecursive (slot: Slot) =
        seq {
            if not (isNull slot.Children) then
                yield! slot.Children
                yield! slot.Children |> Seq.collect collectChildrenRecursive
        }

    /// <summary>
    /// Gets all descendants of a slot (children, grandchildren, etc.).
    /// </summary>
    /// <param name="deep">If true, includes full component data for each descendant slot.</param>
    /// <param name="slot">The ancestor slot.</param>
    /// <returns>A query returning all descendant slots.</returns>
    let descendants (deep: bool) (slot: Slot) : RPath<Slot> =
        fun link ->
            task {
                let! slot = getSlot slot.ID (-1) deep link
                return collectChildrenRecursive slot
            }
            |> ValueTask<Slot seq>

    /// <summary>
    /// Gets all descendants of a slot without component data.
    /// </summary>
    /// <param name="slot">The ancestor slot.</param>
    /// <returns>A query returning all descendant slots (reference only, no component data).</returns>
    let descendantsShallow slot : RPath<Slot> = descendants false slot

    /// <summary>
    /// Gets all descendants of a slot with full component data.
    /// </summary>
    /// <param name="slot">The ancestor slot.</param>
    /// <returns>A query returning all descendant slots with component data.</returns>
    let descendantsDeep slot : RPath<Slot> = descendants true slot


    /// <summary>
    /// Gets the parent of a slot.
    /// </summary>
    /// <param name="deep">If true, includes full component data for the parent slot.</param>
    /// <param name="slot">The child slot.</param>
    /// <returns>A query returning the parent slot, or empty if at root.</returns>
    let parent deep (slot: Slot) : RPath<Slot> =
        if isNull slot.Parent.TargetID then
            fun _ -> ValueTask<Slot seq>(Seq.empty)
        else
            fun link ->
                task {
                    let! slot = getSlot slot.Parent.TargetID 0 deep link
                    return Seq.singleton slot
                }
                |> ValueTask<Slot seq>

    /// <summary>
    /// Gets the parent of a slot without component data.
    /// </summary>
    /// <param name="slot">The child slot.</param>
    /// <returns>A query returning the parent slot (reference only), or empty if at root.</returns>
    let parentShallow (slot: Slot) : RPath<Slot> = parent false slot

    /// <summary>
    /// Gets the parent of a slot with full component data.
    /// </summary>
    /// <param name="slot">The child slot.</param>
    /// <returns>A query returning the parent slot with component data, or empty if at root.</returns>
    let parentDeep (slot: Slot) : RPath<Slot> = parent true slot

    /// <summary>
    /// Gets all ancestors of a slot (parent, grandparent, etc.) up to the root.
    /// </summary>
    /// <param name="deep">If true, includes full component data for each ancestor slot.</param>
    /// <param name="slot">The descendant slot.</param>
    /// <returns>A query returning all ancestor slots from root to parent.</returns>
    let ancestors deep (slot: Slot) : RPath<Slot> =
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

    /// <summary>
    /// Gets all ancestors of a slot without component data.
    /// </summary>
    /// <param name="slot">The descendant slot.</param>
    /// <returns>A query returning all ancestor slots (reference only, no component data).</returns>
    let ancestorsShallow slot = ancestors false slot

    /// <summary>
    /// Gets all ancestors of a slot with full component data.
    /// </summary>
    /// <param name="slot">The descendant slot.</param>
    /// <returns>A query returning all ancestor slots with component data.</returns>
    let ancestorsDeep slot = ancestors true slot

    /// <summary>
    /// Gets the components attached to a slot.
    /// </summary>
    /// <param name="slot">The slot to get components from.</param>
    /// <returns>A query returning all components on the slot.</returns>
    /// <remarks>If component data is not already loaded, this will fetch it from the data model.</remarks>
    let components (slot: Slot) : RPath<Component> =
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

    /// <summary>
    /// Filters components to only include those of the specified type.
    /// </summary>
    /// <param name="componentType">The fully qualified type name of the component (e.g., "FrooxEngine.ReferenceField&lt;FrooxEngine.Slot&gt;").</param>
    /// <param name="components">The source component query.</param>
    /// <returns>A query returning only components matching the specified type.</returns>
    let inline ofType (componentType: string) ([<InlineIfLambda>] components: RPath<Component>) : RPath<Component> =
        components |> mapAll (Seq.filter (fun c -> c.ComponentType = componentType))

    /// <summary>
    /// Gets a member from each component by name and casts it to the specified type.
    /// </summary>
    /// <typeparam name="T">The expected member type (must inherit from ResoniteLink.Member).</typeparam>
    /// <param name="memberName">The name of the member to retrieve.</param>
    /// <param name="query">The source component query.</param>
    /// <returns>A query returning the typed members.</returns>
    /// <remarks>Components without a matching member or with a member of incompatible type are excluded from results.</remarks>
    let inline getMember<'T when 'T :> ResoniteLink.Member>
        (memberName: string)
        ([<InlineIfLambda>] query: RPath<Component>)
        : RPath<'T> =
        query
        |> mapAll (
            Seq.map (fun c -> c.Members.TryGetValue memberName)
            >> Seq.filter fst
            >> Seq.map snd
            >> Seq.choose (function
                | :? 'T as value -> Some value
                | _ -> None)
        )

    /// <summary>
    /// Gets a single element at the specified index from the query results.
    /// </summary>
    /// <param name="index">The index of the element (supports negative indexing from the end).</param>
    /// <param name="query">The source query.</param>
    /// <returns>A query returning the element at the specified index, or empty if out of bounds.</returns>
    let inline itemAt (index: int) ([<InlineIfLambda>] query: RPath<'T>) : RPath<'T> =
        query
        |> mapAll (fun result ->
            let normalizedIndex = if index < 0 then (Seq.length result) + index else index

            match Seq.tryItem normalizedIndex result with
            | None -> Seq.empty
            | Some value -> Seq.singleton value)

    /// <summary>
    /// Takes the first N elements from the query results.
    /// </summary>
    /// <param name="count">The number of elements to take.</param>
    /// <param name="query">The source query.</param>
    /// <returns>A query returning at most the specified number of elements.</returns>
    let inline take (count: int) ([<InlineIfLambda>] query: RPath<'T>) : RPath<'T> =
        query |> mapAll (Seq.take count)

    /// <summary>
    /// Skips the first N elements from the query results.
    /// </summary>
    /// <param name="count">The number of elements to skip.</param>
    /// <param name="query">The source query.</param>
    /// <returns>A query returning all elements after the skipped ones.</returns>
    let inline skip (count: int) ([<InlineIfLambda>] query: RPath<'T>) : RPath<'T> =
        query |> mapAll (Seq.skip count)

    /// <summary>
    /// Gets a slice of elements from start to stop index.
    /// </summary>
    /// <param name="start">The start index (inclusive, supports negative indexing).</param>
    /// <param name="stop">The stop index (exclusive, supports negative indexing).</param>
    /// <param name="query">The source query.</param>
    /// <returns>A query returning elements in the specified range.</returns>
    let inline slice (start: int, stop: int) ([<InlineIfLambda>] query: RPath<'T>) : RPath<'T> =
        query
        |> mapAll (fun result ->
            let normalizeIndex index =
                if index < 0 then (Seq.length result) + index else index

            let normalizedStart = normalizeIndex start
            let normalizedStop = normalizeIndex stop

            result
            |> Seq.skip normalizedStart
            |> Seq.take (normalizedStop - normalizedStart))

    /// <summary>
    /// Internal helper to dereference a reference value using a provided dereferencing function.
    /// </summary>
    /// <param name="dereferenceFunc">The function to use for dereferencing.</param>
    /// <param name="referenceValue">The reference to dereference.</param>
    /// <returns>A query returning the dereferenced value, or empty if the reference is null.</returns>
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

    /// <summary>
    /// Dereferences a slot reference to get the target slot.
    /// </summary>
    /// <param name="deep">If true, includes full component data for the target slot.</param>
    /// <param name="referenceValue">The reference to dereference.</param>
    /// <returns>A query returning the target slot, or empty if the reference is null.</returns>
    let dereferenceSlot deep (referenceValue: Reference) : RPath<Slot> =
        let getSlot (link: ILinkInterface) slotID =
            link.GetSlotData(GetSlot(SlotID = slotID, Depth = 0, IncludeComponentData = deep))

        dereference getSlot referenceValue

    /// <summary>
    /// Dereferences a slot reference without component data.
    /// </summary>
    /// <param name="referenceValue">The reference to dereference.</param>
    /// <returns>A query returning the target slot (reference only), or empty if null.</returns>
    let dereferenceSlotShallow referenceValue = dereferenceSlot false referenceValue

    /// <summary>
    /// Dereferences a slot reference with full component data.
    /// </summary>
    /// <param name="referenceValue">The reference to dereference.</param>
    /// <returns>A query returning the target slot with component data, or empty if null.</returns>
    let dereferenceSlotDeep referenceValue = dereferenceSlot true referenceValue

    /// <summary>
    /// Dereferences a component reference to get the target component.
    /// </summary>
    /// <param name="referenceValue">The reference to dereference.</param>
    /// <returns>A query returning the target component, or empty if the reference is null.</returns>
    let inline dereferenceComponent (referenceValue: Reference) : RPath<Component> =
        let getComponent (link: ILinkInterface) componentID =
            link.GetComponentData(GetComponent(ComponentID = componentID))

        dereference getComponent referenceValue

    /// <summary>
    /// Casts an object query to a typed query.
    /// </summary>
    /// <typeparam name="T">The target type.</typeparam>
    /// <param name="query">The source object query.</param>
    /// <returns>A query returning properly typed elements, or empty if the cast fails.</returns>
    let inline CastQuery<'T> ([<InlineIfLambda>] query: RPath<obj>) : RPath<'T> =
        query
        |> mapAll (function
            | :? System.Collections.Generic.IEnumerable<'T> as properType -> properType
            | _ -> Seq.empty)

    /// <summary>
    /// Executes a query and returns the result as a Result type, capturing any errors.
    /// </summary>
    /// <param name="link">The link interface to use for execution.</param>
    /// <param name="query">The query to execute.</param>
    /// <returns>A ValueTask containing Ok with the results, or Error with the exception.</returns>
    let inline toResultAsync (link: ILinkInterface) ([<InlineIfLambda>] query: RPath<'T>) =
        task {
            try
                let! result = query link
                return Result.Ok result
            with :? ResoniteLinkException as e ->
                return Result.Error e
        }
        |> ValueTask<Result<'T seq, ResoniteLinkException>>

    /// <summary>
    /// Executes a query and returns the results as a sequence.
    /// </summary>
    /// <param name="link">The link interface to use for execution.</param>
    /// <param name="query">The query to execute.</param>
    /// <returns>A ValueTask containing the result sequence.</returns>
    /// <exception cref="ResoniteLinkException">Thrown when a ResoniteLink operation fails.</exception>
    let inline toSeqAsync (link: ILinkInterface) ([<InlineIfLambda>] query: RPath<'T>) = query link

    /// <summary>
    /// Executes a query and returns the results as an array.
    /// </summary>
    /// <param name="link">The link interface to use for execution.</param>
    /// <param name="query">The query to execute.</param>
    /// <returns>A ValueTask containing the result array.</returns>
    /// <exception cref="ResoniteLinkException">Thrown when a ResoniteLink operation fails.</exception>
    let inline toArrayAsync (link: ILinkInterface) (query: RPath<'T>) =
        task {
            let! items = query link
            return (Seq.toArray items)
        }
        |> ValueTask<'T[]>

    /// <summary>
    /// Executes a query and returns the results as a ResizeArray (List&lt;T&gt;).
    /// </summary>
    /// <param name="link">The link interface to use for execution.</param>
    /// <param name="query">The query to execute.</param>
    /// <returns>A ValueTask containing the result as a mutable list.</returns>
    /// <exception cref="ResoniteLinkException">Thrown when a ResoniteLink operation fails.</exception>
    let inline toResizeArray (link: ILinkInterface) (query: RPath<'T>) =
        task {
            let! items = query link
            return (ResizeArray items)
        }
        |> ValueTask<ResizeArray<'T>>

    /// <summary>
    /// A query that returns the root slot of the data model.
    /// </summary>
    /// <returns>A query returning the root slot.</returns>
    let root: RPath<Slot> =
        fun link ->
            task {
                let! rootSlot = getSlot "Root" 0 false link
                return Seq.singleton rootSlot
            }
            |> ValueTask<Slot seq>
