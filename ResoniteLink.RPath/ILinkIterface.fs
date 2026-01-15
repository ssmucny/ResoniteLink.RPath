namespace ResoniteLink

open System.Threading.Tasks
open ResoniteLink

/// <summary>
/// Interpreter interface that defines the abstract operations available on a link that can interact with the data model.
/// </summary>
/// <remarks>
/// This enables using a tagless final API design to decouple from the specific link implementation.
/// This could be implemented for unit tests with a mock link, or even an in-game Resonite mod.
/// </remarks>
type ILinkInterface =
    /// <summary>Retrieves slot data from the data model.</summary>
    /// <param name="request">The request specifying which slot to retrieve.</param>
    /// <returns>A task containing the slot data.</returns>
    abstract member GetSlotData: request: GetSlot -> Task<SlotData>

    /// <summary>Retrieves component data from the data model.</summary>
    /// <param name="request">The request specifying which component to retrieve.</param>
    /// <returns>A task containing the component data.</returns>
    abstract member GetComponentData: request: GetComponent -> Task<ComponentData>

    /// <summary>Adds a new slot to the data model.</summary>
    /// <param name="request">The request specifying the slot to add.</param>
    /// <returns>A task containing the operation response.</returns>
    abstract member AddSlot: request: AddSlot -> Task<Response>

    /// <summary>Updates an existing slot in the data model.</summary>
    /// <param name="request">The request specifying the slot update.</param>
    /// <returns>A task containing the operation response.</returns>
    abstract member UpdateSlot: request: UpdateSlot -> Task<Response>

    /// <summary>Removes a slot from the data model.</summary>
    /// <param name="request">The request specifying the slot to remove.</param>
    /// <returns>A task containing the operation response.</returns>
    abstract member RemoveSlot: request: RemoveSlot -> Task<Response>

    /// <summary>Adds a new component to the data model.</summary>
    /// <param name="request">The request specifying the component to add.</param>
    /// <returns>A task containing the operation response.</returns>
    abstract member AddComponent: request: AddComponent -> Task<Response>

    /// <summary>Updates an existing component in the data model.</summary>
    /// <param name="request">The request specifying the component update.</param>
    /// <returns>A task containing the operation response.</returns>
    abstract member UpdateComponent: request: UpdateComponent -> Task<Response>

    /// <summary>Removes a component from the data model.</summary>
    /// <param name="request">The request specifying the component to remove.</param>
    /// <returns>A task containing the operation response.</returns>
    abstract member RemoveComponent: request: RemoveComponent -> Task<Response>

    /// <summary>Imports a texture from a file.</summary>
    /// <param name="request">The request specifying the texture file to import.</param>
    /// <returns>A task containing the asset data.</returns>
    abstract member ImportTexture: request: ImportTexture2DFile -> Task<AssetData>

    /// <summary>Imports a texture from raw data.</summary>
    /// <param name="request">The request specifying the raw texture data to import.</param>
    /// <returns>A task containing the asset data.</returns>
    abstract member ImportTexture: request: ImportTexture2DRawData -> Task<AssetData>

/// <summary>Functions for working with ILinkInterface instances.</summary>
module ILinkInterface =
    /// <summary>
    /// Wraps a ResoniteLink.LinkInterface into an ILinkInterface for use with RPath queries.
    /// </summary>
    /// <param name="link">The ResoniteLink.LinkInterface to wrap.</param>
    /// <returns>An ILinkInterface implementation that delegates to the provided LinkInterface.</returns>
    let ofResoniteLinkInterface (link: ResoniteLink.LinkInterface) =
        // Because LinkInterface does not implement the ILinkInterface we 'implement' it here
        // If the class was converted to implement the interface directly, then this would no longer be necessary
        { new ILinkInterface with
            member this.AddComponent(request) = link.AddComponent request
            member this.AddSlot(request) = link.AddSlot request
            member this.GetComponentData(request) = link.GetComponentData request
            member this.GetSlotData(request) = link.GetSlotData request
            member this.ImportTexture(request: ImportTexture2DFile) : Task<AssetData> = link.ImportTexture request
            member this.ImportTexture(request: ImportTexture2DRawData) : Task<AssetData> = link.ImportTexture request
            member this.RemoveComponent(request) = link.RemoveComponent request
            member this.RemoveSlot(request) = link.RemoveSlot request
            member this.UpdateComponent(request) = link.UpdateComponent request
            member this.UpdateSlot(request) = link.UpdateSlot request }
