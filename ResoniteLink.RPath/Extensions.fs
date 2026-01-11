namespace ResoniteLink.RPath

open System
open System.Runtime.CompilerServices
open ResoniteLink

[<Struct>]
type RPathBuilder<'T> =
    { Link: ILinkInterface
      RunWith: RPath<'T> }

[<Extension>]
type LinkInterfaceExtensions =
    [<Extension>]
    static member RPath(linkInterface: ILinkInterface) =
        { Link = linkInterface
          RunWith = RPath.root }

    [<Extension>]
    static member RPath(linkInterface: LinkInterface) =
        { Link = linkInterface.ToInterface()
          RunWith = RPath.root }

    [<Extension>]
    static member RPath(linkInterface: ILinkInterface, initialValue) =
        { Link = linkInterface
          RunWith = RPath.wrap initialValue }

    [<Extension>]
    static member RPath(linkInterface: LinkInterface, initialValue) =
        { Link = linkInterface.ToInterface()
          RunWith = RPath.wrap initialValue }

    [<Extension>]
    static member inline ToInterface(link: LinkInterface) =
        ILinkInterface.ofResoniteLinkInterface link


type RPathBuilder<'T> with
    member inline this.Select(projection: Func<'T, 'U>) =
        { Link = this.Link
          RunWith = RPath.map projection.Invoke this.RunWith }

    member inline this.SelectAll(projection: Func<'T seq, 'U seq>) =
        { Link = this.Link
          RunWith = RPath.mapAll projection.Invoke this.RunWith }

    member inline this.Where(predicate: Func<'T, bool>) =
        { Link = this.Link
          RunWith = RPath.filter predicate.Invoke this.RunWith }

    member inline this.SelectMany(collector: Func<'T, RPath<'U>>) =
        { Link = this.Link
          RunWith = RPath.flatmap collector.Invoke this.RunWith }

    member inline this.Take(count: int) =
        { Link = this.Link
          RunWith = RPath.take count this.RunWith }

    member inline this.Skip(count: int) =
        { Link = this.Link
          RunWith = RPath.skip count this.RunWith }

    member inline this.Slice(start: int, stop: int) =
        { Link = this.Link
          RunWith = RPath.slice (start, stop) this.RunWith }

    member inline this.ToResultAsync() =
        RPath.toResultAsync this.Link this.RunWith

    member inline this.ToArrayAsync() =
        RPath.toArrayAsync this.Link this.RunWith

    member inline this.AsEnumerableAsync() = RPath.toSeqAsync this.Link this.RunWith

    member inline this.ToListAsync() =
        RPath.toResizeArray this.Link this.RunWith

[<Extension>]
type ReferenceBuilderExtensions =
    [<Extension>]
    static member inline DereferenceSlot(referenceQuery: RPathBuilder<Reference>) =
        { Link = referenceQuery.Link
          RunWith = RPath.dereferenceSlotDeep referenceQuery.RunWith }

    [<Extension>]
    static member inline DereferenceSlotShallow(referenceQuery: RPathBuilder<Reference>) =
        { Link = referenceQuery.Link
          RunWith = RPath.dereferenceSlotShallow referenceQuery.RunWith }

    [<Extension>]
    static member inline DereferenceComponent(referenceQuery: RPathBuilder<Reference>) =
        { Link = referenceQuery.Link
          RunWith = RPath.dereferenceComponent referenceQuery.RunWith }

[<Extension>]
type SlotQueryBuilderExtensions =
    [<Extension>]
    static member inline Children(slotQuery: RPathBuilder<Slot>) =
        { Link = slotQuery.Link
          RunWith = RPath.flatmap RPath.childrenDeep slotQuery.RunWith }

    [<Extension>]
    static member inline Parent(slotQuery: RPathBuilder<Slot>) =
        { Link = slotQuery.Link
          RunWith = RPath.flatmap RPath.parentDeep slotQuery.RunWith }

    [<Extension>]
    static member inline Ancestors(slotQuery: RPathBuilder<Slot>) =
        { Link = slotQuery.Link
          RunWith = RPath.flatmap RPath.ancestorsDeep slotQuery.RunWith }

    [<Extension>]
    static member inline Descendants(slotQuery: RPathBuilder<Slot>) =
        { Link = slotQuery.Link
          RunWith = RPath.flatmap RPath.descendantsDeep slotQuery.RunWith }

    [<Extension>]
    static member inline ChildrenShallow(slotQuery: RPathBuilder<Slot>) =
        { Link = slotQuery.Link
          RunWith = RPath.flatmap RPath.childrenShallow slotQuery.RunWith }

    [<Extension>]
    static member inline ParentShallow(slotQuery: RPathBuilder<Slot>) =
        { Link = slotQuery.Link
          RunWith = RPath.flatmap RPath.parentShallow slotQuery.RunWith }

    [<Extension>]
    static member inline AncestorsShallow(slotQuery: RPathBuilder<Slot>) =
        { Link = slotQuery.Link
          RunWith = RPath.flatmap RPath.ancestorsShallow slotQuery.RunWith }

    [<Extension>]
    static member inline DescendantsShallow(slotQuery: RPathBuilder<Slot>) =
        { Link = slotQuery.Link
          RunWith = RPath.flatmap RPath.descendantsShallow slotQuery.RunWith }

    [<Extension>]
    static member inline Components(slotQuery: RPathBuilder<Slot>) =
        { Link = slotQuery.Link
          RunWith = RPath.flatmap RPath.components slotQuery.RunWith }

[<Extension>]
type ComponentQueryBuilderExtensions =
    [<Extension>]
    static member inline OfType(componentQuery: RPathBuilder<Component>, componentTypeName: string) =
        { Link = componentQuery.Link
          RunWith = RPath.ofType componentTypeName componentQuery.RunWith }

    [<Extension>]
    static member inline Member<'T when 'T :> ResoniteLink.Member>
        (componentQuery: RPathBuilder<Component>, memberName: string)
        =
        { Link = componentQuery.Link
          RunWith = RPath.getMember<'T> memberName componentQuery.RunWith }
