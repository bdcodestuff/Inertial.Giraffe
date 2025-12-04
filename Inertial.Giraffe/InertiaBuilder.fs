namespace Inertial.Giraffe

open Inertial.Lib
open Inertial.Lib.Types

/// Simplified builder API for Inertia pages
/// Uses attributes and reflection to reduce boilerplate
[<AutoOpen>]
module InertiaBuilder =

    /// Configuration for rendering an Inertia page
    type PageConfig<'TProps> = {
        Props: 'TProps
        ComponentName: string option
        Title: string option
        ReloadOnMountFields: string list option
        CacheStorage: CacheStorage option
        CacheRetrieval: CacheRetrieval option
        RefreshOnBack: bool
        RealTime: bool
    }

    /// Default configuration with sensible defaults
    let defaultConfig<'TProps> (props: 'TProps) : PageConfig<'TProps> = {
        Props = props
        ComponentName = None
        Title = None
        ReloadOnMountFields = None
        CacheStorage = None
        CacheRetrieval = None
        RefreshOnBack = false
        RealTime = true
    }

    /// Get the component name from [<InertiaPage>] attribute or use type name
    let private getComponentName<'TProps> () =
        match Reflection.inertiaPageName<'TProps> with
        | Some name -> name
        | None -> typeof<'TProps>.Name

    /// Get reload-on-mount fields from [<ReloadOnMount>] attributes
    let private getReloadOnMountFields<'TProps> () =
        let fields = Reflection.reloadOnMountFields<'TProps>
        if fields.Length > 0 then Some (Array.toList fields)
        else None

    /// Get cacheable fields from [<Cacheable>] attributes
    let private getCacheableFields<'TProps> () =
        let fields = Reflection.cacheableFields<'TProps>
        if fields.Length > 0 then Some (Array.toList fields)
        else None

    // ============================================================================
    // Fluent configuration functions
    // ============================================================================

    /// Set a custom component name (overrides [<InertiaPage>] attribute)
    let withComponentName (name: string) (config: PageConfig<'TProps>) =
        { config with ComponentName = Some name }

    /// Set the page title
    let withTitle (title: string) (config: PageConfig<'TProps>) =
        { config with Title = Some title }

    /// Specify fields to reload on mount (overrides [<ReloadOnMount>] attributes)
    let withReloadOnMount (fields: string list) (config: PageConfig<'TProps>) =
        { config with ReloadOnMountFields = Some fields }

    /// Specify fields to reload using type-safe quotations
    let withReloadOnMountFields (exprs: Quotations.Expr<'TProps -> _> list) (config: PageConfig<'TProps>) =
        let fieldNames = FieldRef.names exprs |> Array.toList
        { config with ReloadOnMountFields = Some fieldNames }

    /// Set cache storage behavior
    let withCacheStorage (storage: CacheStorage) (config: PageConfig<'TProps>) =
        { config with CacheStorage = Some storage }

    /// Set cache retrieval behavior
    let withCacheRetrieval (retrieval: CacheRetrieval) (config: PageConfig<'TProps>) =
        { config with CacheRetrieval = Some retrieval }

    /// Enable caching for all async fields
    let withCachingEnabled (config: PageConfig<'TProps>) =
        { config with
            CacheStorage = Some StoreAll
            CacheRetrieval = Some CheckForAll }

    /// Enable caching for specific fields using [<Cacheable>] attributes
    let withAttributeBasedCaching (config: PageConfig<'TProps>) =
        match getCacheableFields<'TProps>() with
        | Some fields ->
            { config with
                CacheStorage = Some (StoreToCache (Array.ofList fields))
                CacheRetrieval = Some (CheckForCached (Array.ofList fields)) }
        | None -> config

    /// Enable refresh on back navigation
    let withRefreshOnBack (config: PageConfig<'TProps>) =
        { config with RefreshOnBack = true }

    /// Disable real-time SSE updates
    let withoutRealTime (config: PageConfig<'TProps>) =
        { config with RealTime = false }

    // ============================================================================
    // Helper to get resolved configuration values
    // ============================================================================

    /// Resolve all configuration values, applying attribute-based defaults
    let resolveConfig<'TProps> (config: PageConfig<'TProps>) =
        let componentName =
            match config.ComponentName with
            | Some name -> name
            | None -> getComponentName<'TProps>()

        let title =
            match config.Title with
            | Some t -> t
            | None -> componentName

        let reloadFields =
            match config.ReloadOnMountFields with
            | Some fields -> Some fields
            | None -> getReloadOnMountFields<'TProps>()

        (componentName, title, reloadFields, config.CacheStorage, config.CacheRetrieval, config.RefreshOnBack, config.RealTime)

    // ============================================================================
    // Simplified page rendering function
    // ============================================================================

    /// Create a page configuration from props with attribute-based defaults
    /// Usage: inertiaPage props |> withTitle "Dashboard" |> renderWith inertia
    let inertiaPage<'TProps> (props: 'TProps) : PageConfig<'TProps> =
        let config = defaultConfig props

        // Apply attribute-based defaults
        let config =
            match getReloadOnMountFields<'TProps>() with
            | Some fields -> { config with ReloadOnMountFields = Some fields }
            | None -> config

        config

    /// Extension methods for InertiaResponse to apply configuration
    type InertiaResponseExtensions =

        /// Apply a PageConfig to an InertiaResponse
        static member inline FromConfig
            (inertia: Inertia<'Props, 'Shared, 'SSE>)
            (config: PageConfig<'TInner>)
            (wrap: 'TInner -> 'Props)
            : InertiaResponse<'Props, 'Shared, 'SSE> =

            let (componentName, title, reloadFields, cacheStorage, cacheRetrieval, refreshOnBack, realTime) =
                resolveConfig config

            let props = wrap config.Props

            // Start with the component
            let response = inertia.Component(props, title)

            // Apply reload on mount if specified
            let response =
                match reloadFields, cacheStorage, cacheRetrieval with
                | Some fields, Some storage, Some retrieval when fields.Length > 0 ->
                    response.SetReloadOnMount(fields, storage, retrieval)
                | Some fields, Some storage, None when fields.Length > 0 ->
                    response.SetReloadOnMount(fields, storage)
                | Some fields, None, Some retrieval when fields.Length > 0 ->
                    response.SetReloadOnMount(fields, cacheRetrieval = retrieval)
                | Some fields, None, None when fields.Length > 0 ->
                    response.SetReloadOnMount(fields)
                | _ -> response

            // Apply other options
            let response =
                if refreshOnBack then response.SetRefreshOnBack()
                else response

            let response =
                if not realTime then response.DisableRealtime()
                else response

            response
