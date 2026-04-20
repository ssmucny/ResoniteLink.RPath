namespace ResoniteLink.RPath

open System.Runtime.CompilerServices
open System.Threading.Tasks
open ResoniteLink

// F# friendly extensions on Query<T> and Slot for general transformations and slot hierarchy navigation, using F# function types.

/// <summary>
/// General transformation and execution extension methods on Query&lt;'T&gt; using F# function types.
/// </summary>
[<Extension>]
type QueryExtensions =

    // --- Transformations ---

    /// <summary>Projects each element to a new form.</summary>
    [<Extension>]
    static member inline Map(query: Query<'T>, projection: 'T -> 'U) : Query<'U> = Query.map projection query

    /// <summary>Transforms the entire result sequence at once.</summary>
    [<Extension>]
    static member inline MapAll(query: Query<'T>, projection: 'T seq -> 'U seq) : Query<'U> =
        Query.mapAll projection query

    /// <summary>Filters results to only those satisfying the predicate.</summary>
    [<Extension>]
    static member inline Filter(query: Query<'T>, predicate: 'T -> bool) : Query<'T> = Query.filter predicate query

    /// <summary>Monadic bind: for each result, applies a function returning a new query and flattens all results.</summary>
    /// <remarks>May trigger additional requests to the ResoniteLink data model.</remarks>
    [<Extension>]
    static member inline Bind(query: Query<'T>, binder: 'T -> Query<'U>) : Query<'U> = Query.bind binder query

    /// <summary>Maps each element to a sequence and flattens the results.</summary>
    [<Extension>]
    static member inline FlatMap(query: Query<'T>, mapper: 'T -> 'U seq) : Query<'U> = Query.flatMap mapper query

    /// <summary>Applies an async continuation to the complete result sequence.</summary>
    [<Extension>]
    static member inline AndThen(query: Query<'T>, continuation: 'T seq -> ValueTask<'U seq>) : Query<'U> =
        Query.andThen continuation query

    /// <summary>Takes the first N elements.</summary>
    [<Extension>]
    static member inline Take(query: Query<'T>, count: int) : Query<'T> = Query.take count query

    /// <summary>Skips the first N elements.</summary>
    [<Extension>]
    static member inline Skip(query: Query<'T>, count: int) : Query<'T> = Query.skip count query

    /// <summary>Gets a slice of elements between start (inclusive) and stop (exclusive) indices.</summary>
    [<Extension>]
    static member inline Slice(query: Query<'T>, start: int, stop: int) : Query<'T> = Query.slice (start, stop) query

    /// <summary>Gets the element at the specified index.</summary>
    [<Extension>]
    static member inline At(query: Query<'T>, index: int) : Query<'T> = Query.itemAt index query

    // --- Execution ---

    /// <summary>Executes the query synchronously and returns the results as a sequence.</summary>
    /// <exception cref="ResoniteLinkException">Thrown when a ResoniteLink operation fails.</exception>
    [<Extension>]
    static member inline Run(query: Query<'T>, link: LinkInterface) : 'T seq = Query.run link query

    /// <summary>Executes the query asynchronously and returns a ValueTask of the result sequence.</summary>
    /// <exception cref="ResoniteLinkException">Thrown when a ResoniteLink operation fails.</exception>
    [<Extension>]
    static member inline RunAsync(query: Query<'T>, link: LinkInterface) : ValueTask<'T seq> = Query.runAsync link query

    /// <summary>Executes the query and packages the outcome as a Result, capturing any ResoniteLinkException.</summary>
    [<Extension>]
    static member inline ToResult(query: Query<'T>, link: LinkInterface) = Query.toResult query link

    /// <summary>Executes the query and returns the results as an array.</summary>
    /// <exception cref="ResoniteLinkException">Thrown when a ResoniteLink operation fails.</exception>
    [<Extension>]
    static member inline ToArray(query: Query<'T>, link: LinkInterface) : ValueTask<'T[]> = Query.toArray query link

    /// <summary>Executes the query and returns the results as a ResizeArray (List&lt;T&gt;).</summary>
    /// <exception cref="ResoniteLinkException">Thrown when a ResoniteLink operation fails.</exception>
    [<Extension>]
    static member inline ToList
        (query: Query<'T>, link: LinkInterface)
        : ValueTask<System.Collections.Generic.List<'T>> =
        Query.toResizeArray query link

    /// <summary>Executes the query and returns the first result, or None if the sequence is empty.</summary>
    [<Extension>]
    static member inline First(query: Query<'T>, link: LinkInterface) : Query<'T> = Query.first query

    /// <summary>Executes the query and returns the first result, or <paramref name="defaultValue"/> if empty.</summary>
    [<Extension>]
    static member inline FirstOr(query: Query<'T>, defaultValue: 'T, link: LinkInterface) : Query<'T> =
        Query.firstOr defaultValue query link

    /// <summary>Executes the query and returns the single result, or throws if there are zero or multiple results.</summary>
    [<Extension>]
    static member inline Single(query: Query<'T>, link: LinkInterface) : ValueTask<'T> = Query.exactlyOne query link

    /// <summary>Executes the query and returns the single result, or <paramref name="defaultValue"/> if there are zero results or more than 1</summary>
    [<Extension>]
    static member inline SingleOr(query: Query<'T>, defaultValue: 'T, link: LinkInterface) : ValueTask<'T> =
        task {
            let! result = Query.tryExactlyOne query link
            return result |> Option.defaultValue defaultValue
        }
        |> ValueTask<'T>

/// <summary>
/// Extension methods on Query&lt;Slot&gt; for navigating the slot hierarchy, using F# function types.
/// </summary>
/// <remarks>
/// All traversal methods default to <c>includeComponents = true</c>.
/// Pass <c>false</c> to skip fetching component data for better performance when components are not needed.
/// </remarks>
[<Extension>]
type SlotQueryExtensions =

    /// <summary>Gets the direct children of each slot, including component data.</summary>
    [<Extension>]
    static member inline Children(query: Query<Slot>) : Query<Slot> = Query.children true query

    /// <summary>Gets the direct children of each slot.</summary>
    /// <param name="includeComponents">When <c>false</c>, omits component data for better performance.</param>
    [<Extension>]
    static member inline Children(query: Query<Slot>, includeComponents: bool) : Query<Slot> =
        Query.children includeComponents query

    /// <summary>Gets direct children matching the predicate, including component data.</summary>
    [<Extension>]
    static member inline Child(query: Query<Slot>, predicate: Slot -> bool) : Query<Slot> =
        Query.child predicate true query

    /// <summary>Gets direct children matching the predicate.</summary>
    /// <param name="includeComponents">When <c>false</c>, omits component data for better performance.</param>
    [<Extension>]
    static member inline Child(query: Query<Slot>, predicate: Slot -> bool, includeComponents: bool) : Query<Slot> =
        Query.child predicate includeComponents query

    /// <summary>Gets the parent of each slot, including component data.</summary>
    [<Extension>]
    static member inline Parent(query: Query<Slot>) : Query<Slot> = Query.parent true query

    /// <summary>Gets the parent of each slot.</summary>
    /// <param name="includeComponents">When <c>false</c>, omits component data for better performance.</param>
    [<Extension>]
    static member inline Parent(query: Query<Slot>, includeComponents: bool) : Query<Slot> =
        Query.parent includeComponents query

    /// <summary>Gets all ancestors (parent, grandparent, etc.) of each slot, including component data.</summary>
    [<Extension>]
    static member inline Ancestors(query: Query<Slot>) : Query<Slot> = Query.ancestors true query

    /// <summary>Gets all ancestors (parent, grandparent, etc.) of each slot.</summary>
    /// <param name="includeComponents">When <c>false</c>, omits component data for better performance.</param>
    [<Extension>]
    static member inline Ancestors(query: Query<Slot>, includeComponents: bool) : Query<Slot> =
        Query.ancestors includeComponents query

    /// <summary>Gets each slot and all of its ancestors, including component data.</summary>
    [<Extension>]
    static member inline AncestorsAndSelf(query: Query<Slot>) : Query<Slot> = Query.ancestorsAndSelf true query

    /// <summary>Gets each slot and all of its ancestors.</summary>
    /// <param name="includeComponents">When <c>false</c>, omits component data for better performance.</param>
    [<Extension>]
    static member inline AncestorsAndSelf(query: Query<Slot>, includeComponents: bool) : Query<Slot> =
        Query.ancestorsAndSelf includeComponents query

    /// <summary>Gets all descendants (children, grandchildren, etc.) of each slot, including component data.</summary>
    [<Extension>]
    static member inline Descendants(query: Query<Slot>) : Query<Slot> = Query.descendants true query

    /// <summary>Gets all descendants (children, grandchildren, etc.) of each slot.</summary>
    /// <param name="includeComponents">When <c>false</c>, omits component data for better performance.</param>
    [<Extension>]
    static member inline Descendants(query: Query<Slot>, includeComponents: bool) : Query<Slot> =
        Query.descendants includeComponents query

    /// <summary>Gets each slot and all of its descendants, including component data.</summary>
    [<Extension>]
    static member inline DescendantsAndSelf(query: Query<Slot>) : Query<Slot> = Query.descendantsAndSelf true query

    /// <summary>Gets each slot and all of its descendants.</summary>
    /// <param name="includeComponents">When <c>false</c>, omits component data for better performance.</param>
    [<Extension>]
    static member inline DescendantsAndSelf(query: Query<Slot>, includeComponents: bool) : Query<Slot> =
        Query.descendantsAndSelf includeComponents query

    /// <summary>Gets the components attached to each slot, fetching from the model if not already loaded.</summary>
    [<Extension>]
    static member inline Components(query: Query<Slot>) : Query<Component> = Query.components query


/// <summary>
/// Extension methods on Slot for navigating the slot hierarchy, using F# function types.
/// </summary>
/// <remarks>
/// All traversal methods default to <c>includeComponents = true</c>.
/// Pass <c>false</c> to skip fetching component data for better performance when components are not needed.
/// </remarks>
[<Extension>]
type SlotExtensions =

    /// <summary>Gets the direct children of the slot, including component data.</summary>
    [<Extension>]
    static member inline GetChildren(slot: Slot) : Query<Slot> = Query.wrap slot |> Query.children true

    /// <summary>Gets the direct children of the slot.</summary>
    /// <param name="includeComponents">When <c>false</c>, omits component data for better performance.</param>
    [<Extension>]
    static member inline GetChildren(slot: Slot, includeComponents: bool) : Query<Slot> =
        Query.wrap slot |> Query.children includeComponents

    /// <summary>Gets direct children matching the predicate, including component data.</summary>
    [<Extension>]
    static member inline Child(slot: Slot, predicate: Slot -> bool) : Query<Slot> =
        Query.wrap slot |> Query.child predicate true

    /// <summary>Gets direct children matching the predicate.</summary>
    /// <param name="includeComponents">When <c>false</c>, omits component data for better performance.</param>
    [<Extension>]
    static member inline Child(slot: Slot, predicate: Slot -> bool, includeComponents: bool) : Query<Slot> =
        Query.wrap slot |> Query.child predicate includeComponents

    /// <summary>Gets the parent of the slot, including component data.</summary>
    [<Extension>]
    static member inline Parent(slot: Slot) : Query<Slot> = Query.wrap slot |> Query.parent true

    /// <summary>Gets the parent of the slot.</summary>
    /// <param name="includeComponents">When <c>false</c>, omits component data for better performance.</param>
    [<Extension>]
    static member inline Parent(slot: Slot, includeComponents: bool) : Query<Slot> =
        Query.wrap slot |> Query.parent includeComponents

    /// <summary>Gets all ancestors (parent, grandparent, etc.) of the slot, including component data.</summary>
    [<Extension>]
    static member inline Ancestors(slot: Slot) : Query<Slot> = Query.wrap slot |> Query.ancestors true

    /// <summary>Gets all ancestors (parent, grandparent, etc.) of the slot.</summary>
    /// <param name="includeComponents">When <c>false</c>, omits component data for better performance.</param>
    [<Extension>]
    static member inline Ancestors(slot: Slot, includeComponents: bool) : Query<Slot> =
        Query.wrap slot |> Query.ancestors includeComponents

    /// <summary>Gets the slot and all of its ancestors, including component data.</summary>
    [<Extension>]
    static member inline AncestorsAndSelf(slot: Slot) : Query<Slot> =
        Query.wrap slot |> Query.ancestorsAndSelf true

    /// <summary>Gets the slot and all of its ancestors.</summary>
    /// <param name="includeComponents">When <c>false</c>, omits component data for better performance.</param>
    [<Extension>]
    static member inline AncestorsAndSelf(slot: Slot, includeComponents: bool) : Query<Slot> =
        Query.wrap slot |> Query.ancestorsAndSelf includeComponents

    /// <summary>Gets all descendants (children, grandchildren, etc.) of the slot, including component data.</summary>
    [<Extension>]
    static member inline Descendants(slot: Slot) : Query<Slot> =
        Query.wrap slot |> Query.descendants true

    /// <summary>Gets all descendants (children, grandchildren, etc.) of the slot.</summary>
    /// <param name="includeComponents">When <c>false</c>, omits component data for better performance.</param>
    [<Extension>]
    static member inline Descendants(slot: Slot, includeComponents: bool) : Query<Slot> =
        Query.wrap slot |> Query.descendants includeComponents

    /// <summary>Gets the slot and all of its descendants, including component data.</summary>
    [<Extension>]
    static member inline DescendantsAndSelf(slot: Slot) : Query<Slot> =
        Query.wrap slot |> Query.descendantsAndSelf true

    /// <summary>Gets the slot and all of its descendants.</summary>
    /// <param name="includeComponents">When <c>false</c>, omits component data for better performance.</param>
    [<Extension>]
    static member inline DescendantsAndSelf(slot: Slot, includeComponents: bool) : Query<Slot> =
        Query.wrap slot |> Query.descendantsAndSelf includeComponents

    /// <summary>Gets the components attached to the slot, fetching from the model if not already loaded.</summary>
    [<Extension>]
    static member inline GetComponents(slot: Slot) : Query<Component> = Query.wrap slot |> Query.components


/// <summary>
/// Extension methods on Query&lt;Component&gt; for filtering components and accessing their members.
/// </summary>
[<Extension>]
type ComponentQueryExtensions =

    /// <summary>Filters components to only those matching the fully-qualified type name.</summary>
    /// <param name="typeName">e.g. <c>"FrooxEngine.ReferenceField&lt;FrooxEngine.Slot&gt;"</c></param>
    [<Extension>]
    static member inline OfType(query: Query<Component>, typeName: string) : Query<Component> =
        Query.ofType typeName query

    /// <summary>Gets a member by name from each component, filtered to the specified member type.</summary>
    /// <remarks>Components without a matching member or with an incompatible type are excluded from results.</remarks>
    [<Extension>]
    static member inline Member<'T when 'T :> ResoniteLink.Member>
        (query: Query<Component>, memberName: string)
        : Query<'T> =
        Query.getMember<'T> memberName query


/// <summary>
/// Extension methods on Query&lt;Reference&gt; for dereferencing references to their target data model objects.
/// </summary>
[<Extension>]
type ReferenceQueryExtensions =

    /// <summary>Dereferences slot references to the target slots, including component data.</summary>
    [<Extension>]
    static member inline DereferenceSlot(query: Query<Reference>) : Query<Slot> = Query.dereferenceSlot true query

    /// <summary>Dereferences slot references to the target slots.</summary>
    /// <param name="includeComponents">When <c>false</c>, omits component data for better performance.</param>
    [<Extension>]
    static member inline DereferenceSlot(query: Query<Reference>, includeComponents: bool) : Query<Slot> =
        Query.dereferenceSlot includeComponents query

    /// <summary>Dereferences component references to the target components.</summary>
    [<Extension>]
    static member inline DereferenceComponent(query: Query<Reference>) : Query<Component> =
        Query.dereferenceComponent query


/// <summary>
/// Extension methods on Reference for dereferencing to target data model objects.
/// </summary>
[<Extension>]
type ReferenceExtensions =

    /// <summary>Dereferences a slot reference to the target slot, including component data.</summary>
    [<Extension>]
    static member inline DereferenceSlot(referenceValue: Reference) : Query<Slot> =
        Query.wrap referenceValue |> Query.dereferenceSlot true

    /// <summary>Dereferences a slot reference to the target slot.</summary>
    /// <param name="includeComponents">When <c>false</c>, omits component data for better performance.</param>
    [<Extension>]
    static member inline DereferenceSlot(referenceValue: Reference, includeComponents: bool) : Query<Slot> =
        Query.wrap referenceValue |> Query.dereferenceSlot includeComponents

    /// <summary>Dereferences a component reference to the target component.</summary>
    [<Extension>]
    static member inline DereferenceComponent(referenceValue: Reference) : Query<Component> =
        Query.wrap referenceValue |> Query.dereferenceComponent


// Recommended C# imports:
//   using ResoniteLink.RPath;        // Query<T> type + zero-arg extensions + execution
//   using ResoniteLink.RPath.CSharp; // Func<>-accepting overloads (Select, Where, etc.)

namespace ResoniteLink.RPath.CSharp

open System
open System.Runtime.CompilerServices
open System.Threading.Tasks
open ResoniteLink
open ResoniteLink.RPath


/// <summary>
/// C# extension methods on Query&lt;T&gt; for general transformations, using <see cref="Func{T,TResult}"/> delegate types.
/// </summary>
/// <remarks>
/// Zero-arg operations (<c>Take</c>, <c>Skip</c>, <c>At</c>, <c>Slice</c>) and all execution methods
/// (<c>Run</c>, <c>ToList</c>, <c>ToArray</c>, etc.) are available via <c>using ResoniteLink.RPath</c>.
/// </remarks>
[<Extension>]
type QueryCSharpExtensions =

    /// <summary>Projects each element to a new form.</summary>
    [<Extension>]
    static member inline Select(query: Query<'T>, projection: Func<'T, 'U>) : Query<'U> =
        Query.map projection.Invoke query

    /// <summary>Transforms the entire result sequence at once.</summary>
    [<Extension>]
    static member inline SelectAll(query: Query<'T>, projection: Func<'T seq, 'U seq>) : Query<'U> =
        Query.mapAll projection.Invoke query

    /// <summary>Filters results to only those satisfying the predicate.</summary>
    [<Extension>]
    static member inline Where(query: Query<'T>, predicate: Func<'T, bool>) : Query<'T> =
        Query.filter predicate.Invoke query

    /// <summary>Monadic bind: for each result, applies a function returning a new query and flattens all results.</summary>
    /// <remarks>May trigger additional requests to the ResoniteLink data model.</remarks>
    [<Extension>]
    static member inline Bind(query: Query<'T>, binder: Func<'T, Query<'U>>) : Query<'U> =
        Query.bind binder.Invoke query

    /// <summary>Projects each element to a sequence and flattens the results (equivalent to LINQ SelectMany).</summary>
    [<Extension>]
    static member inline SelectMany(query: Query<'T>, collector: Func<'T, 'U seq>) : Query<'U> =
        Query.flatMap collector.Invoke query

    /// <summary>Applies an async continuation to the complete result sequence.</summary>
    [<Extension>]
    static member inline AndThen(query: Query<'T>, continuation: Func<'T seq, ValueTask<'U seq>>) : Query<'U> =
        Query.andThen continuation.Invoke query


/// <summary>
/// C# extension methods on Query&lt;Slot&gt; for callback-accepting slot operations, using <see cref="Func{T,TResult}"/> types.
/// </summary>
/// <remarks>
/// Zero-arg traversal methods (<c>Children</c>, <c>Parent</c>, <c>Descendants</c>, etc.) and their
/// <c>bool includeComponents</c> overloads are available via <c>using ResoniteLink.RPath</c>.
/// </remarks>
[<Extension>]
type SlotQueryCSharpExtensions =

    /// <summary>Gets direct children matching the predicate, including component data.</summary>
    [<Extension>]
    static member inline Child(query: Query<Slot>, predicate: Func<Slot, bool>) : Query<Slot> =
        Query.child predicate.Invoke true query

    /// <summary>Gets direct children matching the predicate.</summary>
    /// <param name="includeComponents">When <c>false</c>, omits component data for better performance.</param>
    [<Extension>]
    static member inline Child(query: Query<Slot>, predicate: Func<Slot, bool>, includeComponents: bool) : Query<Slot> =
        Query.child predicate.Invoke includeComponents query


/// <summary>
/// C# extension methods on Slot for callback-accepting slot operations, using <see cref="Func{T,TResult}"/> types.
/// </summary>
[<Extension>]
type SlotCSharpExtensions =

    /// <summary>Gets direct children matching the predicate, including component data.</summary>
    [<Extension>]
    static member inline Child(slot: Slot, predicate: Func<Slot, bool>) : Query<Slot> =
        Query.wrap slot |> Query.child predicate.Invoke true

    /// <summary>Gets direct children matching the predicate.</summary>
    /// <param name="includeComponents">When <c>false</c>, omits component data for better performance.</param>
    [<Extension>]
    static member inline Child(slot: Slot, predicate: Func<Slot, bool>, includeComponents: bool) : Query<Slot> =
        Query.wrap slot |> Query.child predicate.Invoke includeComponents

    [<Extension>]
    static member inline Patch(slot: Slot, link: LinkInterface) =
        slot |> Slot.patch |> link.RunDataModelOperationBatch

    [<Extension>]
    static member inline Patch(slots: #seq<Slot>, link: LinkInterface) =
        slots |> Slot.patchSlots |> link.RunDataModelOperationBatch
