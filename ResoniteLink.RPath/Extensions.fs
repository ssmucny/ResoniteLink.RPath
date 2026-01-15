namespace ResoniteLink.RPath

open System
open System.Runtime.CompilerServices
open System.Threading.Tasks
open ResoniteLink
open ResoniteLink.RPath

/// <summary>
/// A builder type that holds a link interface and an RPath query, enabling fluent LINQ-style method chaining.
/// </summary>
/// <typeparam name="T">The type of elements produced by the query.</typeparam>
/// <remarks>
/// Use the extension method <c>RPath()</c> on a LinkInterface to create an RPathBuilder starting at the root slot.
/// Chain methods like <c>Children()</c>, <c>Where()</c>, <c>Select()</c>, etc. to build queries.
/// Execute with <c>ToListAsync()</c>, <c>ToArrayAsync()</c>, or <c>ToResultAsync()</c>.
/// </remarks>
[<Struct>]
type RPathBuilder<'T> =
    { /// <summary>The link interface used to execute the query.</summary>
      Link: ILinkInterface
      /// <summary>The underlying RPath query function.</summary>
      RunWith: RPath<'T> }

/// <summary>
/// Extension methods for starting RPath queries from a LinkInterface.
/// </summary>
[<Extension>]
type LinkInterfaceExtensions =
    /// <summary>
    /// Creates an RPathBuilder starting at the root slot.
    /// </summary>
    /// <param name="linkInterface">The link interface to use for query execution.</param>
    /// <returns>An RPathBuilder positioned at the root slot.</returns>
    [<Extension>]
    static member inline RPath(linkInterface: ILinkInterface) =
        { Link = linkInterface
          RunWith = RPath.root }

    /// <summary>
    /// Creates an RPathBuilder starting at the root slot.
    /// </summary>
    /// <param name="linkInterface">The ResoniteLink LinkInterface to use for query execution.</param>
    /// <returns>An RPathBuilder positioned at the root slot.</returns>
    [<Extension>]
    static member inline RPath(linkInterface: LinkInterface) =
        { Link = linkInterface.ToInterface()
          RunWith = RPath.root }

    /// <summary>
    /// Creates an RPathBuilder starting with a specific initial value.
    /// </summary>
    /// <param name="linkInterface">The link interface to use for query execution.</param>
    /// <param name="initialValue">The initial value to start the query with.</param>
    /// <returns>An RPathBuilder containing the initial value.</returns>
    [<Extension>]
    static member inline RPath(linkInterface: ILinkInterface, initialValue) =
        { Link = linkInterface
          RunWith = RPath.wrap initialValue }

    /// <summary>
    /// Creates an RPathBuilder starting with a specific initial value.
    /// </summary>
    /// <param name="linkInterface">The ResoniteLink LinkInterface to use for query execution.</param>
    /// <param name="initialValue">The initial value to start the query with.</param>
    /// <returns>An RPathBuilder containing the initial value.</returns>
    [<Extension>]
    static member inline RPath(linkInterface: LinkInterface, initialValue) =
        { Link = linkInterface.ToInterface()
          RunWith = RPath.wrap initialValue }

    /// <summary>
    /// Converts a ResoniteLink LinkInterface to an ILinkInterface.
    /// </summary>
    /// <param name="link">The LinkInterface to convert.</param>
    /// <returns>An ILinkInterface wrapper around the LinkInterface.</returns>
    [<Extension>]
    static member ToInterface(link: LinkInterface) =
        ILinkInterface.ofResoniteLinkInterface link


type RPathBuilder<'T> with
    /// <summary>
    /// Projects each element of the query to a new form.
    /// </summary>
    /// <param name="projection">A function to apply to each element.</param>
    /// <returns>A new RPathBuilder with the projected elements.</returns>
    member inline this.Select(projection: Func<'T, 'U>) =
        { Link = this.Link
          RunWith = RPath.map projection.Invoke this.RunWith }

    /// <summary>
    /// Applies a transformation to the entire result sequence.
    /// </summary>
    /// <param name="projection">A function to apply to the result sequence.</param>
    /// <returns>A new RPathBuilder with the transformed results.</returns>
    member inline this.SelectAll(projection: Func<'T seq, 'U seq>) =
        { Link = this.Link
          RunWith = RPath.mapAll projection.Invoke this.RunWith }

    /// <summary>
    /// Filters the query results based on a predicate.
    /// </summary>
    /// <param name="predicate">A function to test each element.</param>
    /// <returns>A new RPathBuilder containing only elements that satisfy the predicate.</returns>
    member inline this.Where(predicate: Func<'T, bool>) =
        { Link = this.Link
          RunWith = RPath.filter predicate.Invoke this.RunWith }

    /// <summary>
    /// Applies a continuation function to the complete result sequence.
    /// </summary>
    /// <param name="continuation">A function that takes the result sequence and returns a new async sequence.</param>
    /// <returns>A new RPathBuilder with results from the continuation.</returns>
    member inline this.AndThen(continuation: Func<'T seq, ValueTask<'U seq>>) =
        { Link = this.Link
          RunWith = RPath.andThen continuation.Invoke this.RunWith }

    /// <summary>
    /// Projects each element to a new query and flattens the results.
    /// </summary>
    /// <param name="collector">A function that returns a query for each element.</param>
    /// <returns>A new RPathBuilder with flattened results.</returns>
    /// <remarks>This operation may trigger additional requests to the ResoniteLink data model.</remarks>
    member inline this.SelectMany(collector: Func<'T, RPath<'U>>) =
        { Link = this.Link
          RunWith = RPath.bind collector.Invoke this.RunWith }

    /// <summary>
    /// Takes the first N elements from the query results.
    /// </summary>
    /// <param name="count">The number of elements to take.</param>
    /// <returns>A new RPathBuilder with at most the specified number of elements.</returns>
    member inline this.Take(count: int) =
        { Link = this.Link
          RunWith = RPath.take count this.RunWith }

    /// <summary>
    /// Skips the first N elements from the query results.
    /// </summary>
    /// <param name="count">The number of elements to skip.</param>
    /// <returns>A new RPathBuilder with elements after the skipped ones.</returns>
    member inline this.Skip(count: int) =
        { Link = this.Link
          RunWith = RPath.skip count this.RunWith }

    /// <summary>
    /// Gets a slice of elements from start to stop index.
    /// </summary>
    /// <param name="start">The start index (inclusive).</param>
    /// <param name="stop">The stop index (exclusive).</param>
    /// <returns>A new RPathBuilder with elements in the specified range.</returns>
    member inline this.Slice(start: int, stop: int) =
        { Link = this.Link
          RunWith = RPath.slice (start, stop) this.RunWith }

    /// <summary>
    /// Executes the query and returns a Result containing either the results or an error.
    /// </summary>
    /// <returns>A ValueTask containing Ok with the result sequence, or Error with the exception.</returns>
    member inline this.ToResultAsync() =
        RPath.toResultAsync this.Link this.RunWith

    /// <summary>
    /// Executes the query and returns the results as an array.
    /// </summary>
    /// <returns>A ValueTask containing the result array.</returns>
    /// <exception cref="ResoniteLinkException">Thrown when a ResoniteLink operation fails.</exception>
    member inline this.ToArrayAsync() =
        RPath.toArrayAsync this.Link this.RunWith

    /// <summary>
    /// Executes the query and returns the results as a sequence.
    /// </summary>
    /// <returns>A ValueTask containing the result sequence.</returns>
    /// <exception cref="ResoniteLinkException">Thrown when a ResoniteLink operation fails.</exception>
    member inline this.AsEnumerableAsync() = RPath.toSeqAsync this.Link this.RunWith

    /// <summary>
    /// Executes the query and returns the results as a List&lt;T&gt;.
    /// </summary>
    /// <returns>A ValueTask containing the results as a mutable list.</returns>
    /// <exception cref="ResoniteLinkException">Thrown when a ResoniteLink operation fails.</exception>
    member inline this.ToListAsync() =
        RPath.toResizeArray this.Link this.RunWith

/// <summary>
/// Extension methods for dereferencing Reference values in RPath queries.
/// </summary>
[<Extension>]
type ReferenceBuilderExtensions =
    /// <summary>
    /// Dereferences slot references to get the target slots with full component data.
    /// </summary>
    /// <param name="referenceQuery">The query containing Reference values.</param>
    /// <returns>An RPathBuilder containing the dereferenced slots with component data.</returns>
    [<Extension>]
    static member inline DereferenceSlot(referenceQuery: RPathBuilder<Reference>) =
        { Link = referenceQuery.Link
          RunWith = RPath.bind RPath.dereferenceSlotDeep referenceQuery.RunWith }

    /// <summary>
    /// Dereferences slot references to get the target slots without component data.
    /// </summary>
    /// <param name="referenceQuery">The query containing Reference values.</param>
    /// <returns>An RPathBuilder containing the dereferenced slots (reference only).</returns>
    [<Extension>]
    static member inline DereferenceSlotShallow(referenceQuery: RPathBuilder<Reference>) =
        { Link = referenceQuery.Link
          RunWith = RPath.bind RPath.dereferenceSlotShallow referenceQuery.RunWith }

    /// <summary>
    /// Dereferences component references to get the target components.
    /// </summary>
    /// <param name="referenceQuery">The query containing Reference values.</param>
    /// <returns>An RPathBuilder containing the dereferenced components.</returns>
    [<Extension>]
    static member inline DereferenceComponent(referenceQuery: RPathBuilder<Reference>) =
        { Link = referenceQuery.Link
          RunWith = RPath.bind RPath.dereferenceComponent referenceQuery.RunWith }

/// <summary>
/// Extension methods for navigating slot hierarchies in RPath queries.
/// </summary>
[<Extension>]
type SlotQueryBuilderExtensions =
    /// <summary>
    /// Gets the direct children of each slot with full component data.
    /// </summary>
    /// <param name="slotQuery">The query containing Slot values.</param>
    /// <returns>An RPathBuilder containing the child slots with component data.</returns>
    [<Extension>]
    static member inline Children(slotQuery: RPathBuilder<Slot>) =
        { Link = slotQuery.Link
          RunWith = RPath.bind RPath.childrenDeep slotQuery.RunWith }

    /// <summary>
    /// Gets the parent of each slot with full component data.
    /// </summary>
    /// <param name="slotQuery">The query containing Slot values.</param>
    /// <returns>An RPathBuilder containing the parent slots with component data.</returns>
    [<Extension>]
    static member inline Parent(slotQuery: RPathBuilder<Slot>) =
        { Link = slotQuery.Link
          RunWith = RPath.bind RPath.parentDeep slotQuery.RunWith }

    /// <summary>
    /// Gets all ancestors of each slot (parent, grandparent, etc.) with full component data.
    /// </summary>
    /// <param name="slotQuery">The query containing Slot values.</param>
    /// <returns>An RPathBuilder containing all ancestor slots with component data.</returns>
    [<Extension>]
    static member inline Ancestors(slotQuery: RPathBuilder<Slot>) =
        { Link = slotQuery.Link
          RunWith = RPath.bind RPath.ancestorsDeep slotQuery.RunWith }

    /// <summary>
    /// Gets all descendants of each slot (children, grandchildren, etc.) with full component data.
    /// </summary>
    /// <param name="slotQuery">The query containing Slot values.</param>
    /// <returns>An RPathBuilder containing all descendant slots with component data.</returns>
    [<Extension>]
    static member inline Descendants(slotQuery: RPathBuilder<Slot>) =
        { Link = slotQuery.Link
          RunWith = RPath.bind RPath.descendantsDeep slotQuery.RunWith }

    /// <summary>
    /// Gets the direct children of each slot without component data.
    /// </summary>
    /// <param name="slotQuery">The query containing Slot values.</param>
    /// <returns>An RPathBuilder containing the child slots (reference only).</returns>
    [<Extension>]
    static member inline ChildrenShallow(slotQuery: RPathBuilder<Slot>) =
        { Link = slotQuery.Link
          RunWith = RPath.bind RPath.childrenShallow slotQuery.RunWith }

    /// <summary>
    /// Gets the parent of each slot without component data.
    /// </summary>
    /// <param name="slotQuery">The query containing Slot values.</param>
    /// <returns>An RPathBuilder containing the parent slots (reference only).</returns>
    [<Extension>]
    static member inline ParentShallow(slotQuery: RPathBuilder<Slot>) =
        { Link = slotQuery.Link
          RunWith = RPath.bind RPath.parentShallow slotQuery.RunWith }

    /// <summary>
    /// Gets all ancestors of each slot without component data.
    /// </summary>
    /// <param name="slotQuery">The query containing Slot values.</param>
    /// <returns>An RPathBuilder containing all ancestor slots (reference only).</returns>
    [<Extension>]
    static member inline AncestorsShallow(slotQuery: RPathBuilder<Slot>) =
        { Link = slotQuery.Link
          RunWith = RPath.bind RPath.ancestorsShallow slotQuery.RunWith }

    /// <summary>
    /// Gets all descendants of each slot without component data.
    /// </summary>
    /// <param name="slotQuery">The query containing Slot values.</param>
    /// <returns>An RPathBuilder containing all descendant slots (reference only).</returns>
    [<Extension>]
    static member inline DescendantsShallow(slotQuery: RPathBuilder<Slot>) =
        { Link = slotQuery.Link
          RunWith = RPath.bind RPath.descendantsShallow slotQuery.RunWith }

    /// <summary>
    /// Gets the components attached to each slot.
    /// </summary>
    /// <param name="slotQuery">The query containing Slot values.</param>
    /// <returns>An RPathBuilder containing all components from the slots.</returns>
    [<Extension>]
    static member inline Components(slotQuery: RPathBuilder<Slot>) =
        { Link = slotQuery.Link
          RunWith = RPath.bind RPath.components slotQuery.RunWith }

/// <summary>
/// Extension methods for working with enumerable results in RPath queries.
/// </summary>
[<Extension>]
type EnumerableQueryBuilderExtensions =
    /// <summary>
    /// Projects each element to a sequence and flattens the results.
    /// </summary>
    /// <param name="items">The query containing sequence values.</param>
    /// <param name="collector">A function that maps each element to a sequence.</param>
    /// <returns>An RPathBuilder with flattened results.</returns>
    [<Extension>]
    static member inline SelectMany(items: RPathBuilder<'T seq>, collector: Func<'T, 'U seq>) =
        { Link = items.Link
          RunWith = RPath.flatmap collector.Invoke items.RunWith }

/// <summary>
/// Extension methods for working with Component queries in RPath.
/// </summary>
[<Extension>]
type ComponentQueryBuilderExtensions =
    /// <summary>
    /// Filters components to only include those of the specified type.
    /// </summary>
    /// <param name="componentQuery">The query containing Component values.</param>
    /// <param name="componentTypeName">The fully qualified type name of the component (e.g., "FrooxEngine.ReferenceField&lt;FrooxEngine.Slot&gt;").</param>
    /// <returns>An RPathBuilder containing only components matching the specified type.</returns>
    [<Extension>]
    static member inline OfType(componentQuery: RPathBuilder<Component>, componentTypeName: string) =
        { Link = componentQuery.Link
          RunWith = RPath.ofType componentTypeName componentQuery.RunWith }

    /// <summary>
    /// Gets a member from each component by name and casts it to the specified type.
    /// </summary>
    /// <typeparam name="T">The expected member type (must inherit from ResoniteLink.Member).</typeparam>
    /// <param name="componentQuery">The query containing Component values.</param>
    /// <param name="memberName">The name of the member to retrieve.</param>
    /// <returns>An RPathBuilder containing the typed members.</returns>
    /// <remarks>Components without a matching member or with a member of incompatible type are excluded from results.</remarks>
    [<Extension>]
    static member inline Member<'T when 'T :> ResoniteLink.Member>
        (componentQuery: RPathBuilder<Component>, memberName: string)
        =
        { Link = componentQuery.Link
          RunWith = RPath.getMember<'T> memberName componentQuery.RunWith }
