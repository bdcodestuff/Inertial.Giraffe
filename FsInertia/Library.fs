module FsInertia

open System
open System.Threading.Tasks
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Http.Extensions
open Microsoft.Extensions.Primitives
open Microsoft.AspNetCore.Antiforgery 
open System.Text.Json
open Giraffe
open Giraffe.ViewEngine

open FSharpx.Collections
open FSharp.Reflection

/// Determine if the given header is present
let private hdr (headers : IHeaderDictionary) hdr =
    match headers[hdr] with it when it = StringValues.Empty -> None | it -> Some it[0]

/// Extensions to the header dictionary
type IHeaderDictionary with
  
    /// Inertia Request
    member this.Inertia with get () = hdr this "X-Inertia" |> Option.map bool.Parse

    /// Inertia Version
    member this.InertiaVersion with get () = hdr this "X-Inertia-Version"

    /// Inertia Location
    member this.InertiaLocation with get () = hdr this "X-Inertia-Location"

    /// Inertia Partial Data
    member this.InertiaPartialData with get () = hdr this "X-Inertia-Partial-Data"
    
    /// Inertia Partial Component
    member this.InertiaPartialComponent with get () = hdr this "X-Inertia-Partial-Component"

    /// Get the token from request set by axios when XSRF-COOKIE is present
    member this.XSRFToken with get () = hdr this "X-XSRF-TOKEN"

    // MODAL

    /// Referer
    member this.TryGetReferer with get () = hdr this "Referer"

    /// The modal key header contained in the request
    member this.InertiaModalKey with get () = hdr this "X-Inertia-Modal-Key"

    /// The modal redirect url in the request
    member this.InertiaModalRedirectUrl with get () = hdr this "X-Inertia-Modal-Redirect"

/// Extensions for the request object
type HttpRequest with

    /// If X-Inertia-Modal-Key header is present then return the key otherwise create a new guid key
    member this.getInertiaModalKey with get () = 
        this.Headers.InertiaModalKey 
        |> Option.defaultValue (Guid.NewGuid().ToString())

    /// Check whether this request was initiated from Inertia
    member this.IsInertia with get () = this.Headers.Inertia |> Option.defaultValue false

type Page =
    {
        ``component`` : string
        props : Map<string,obj>
        version : string
        url : string
    }

    member x.toJson () =
        JsonSerializer.Serialize<Page>(x)

type ModalComponent = 
    {
        ``component`` : string
        props : Map<string,obj>
        redirectUrl : string
        key : string
    }

type ModalPage =
    {
        props : Map<string,obj>
        version : string
        url : string
    }

    member x.toJson () =
        JsonSerializer.Serialize<ModalPage>(x)


module Handlers =


    
    let mergeWithSharedProps (existingProps:Map<string,obj>) (ctx:HttpContext) =
            match ctx.Items.TryGetValue("InertiaSharedData") with
            | (false, _) -> Map.empty<string,obj>
            | (true, a) ->
                let sharedProps = try a :?> Map<string,obj> with exn -> failwith exn.Message
                Map.union existingProps sharedProps

    let handleProps (ctx:HttpContext) componentName (props:Map<string,obj>) =
        task {
            // check if partial data request with specified component name
        let isPartialReq, filter =
            match ctx.Request.Headers.InertiaPartialData, ctx.Request.Headers.InertiaPartialComponent with
            | Some partialData, Some comp when comp = componentName ->
                true,
                partialData.Split(',') 
                |> Array.filter (fun x -> String.IsNullOrEmpty x |> not)
                |> Array.map (fun x -> x.Trim())
            | _ ->
                false,
                [||]
        let filteredProps =
            match filter with
            | [||] -> props
            | filter ->
                props |> Map.filter (fun x y -> Array.contains x filter)

        let props = mergeWithSharedProps filteredProps ctx
        
        let functions, nonFunctions =
            props
            |> Map.partition (fun _ v -> FSharpType.IsFunction(v.GetType()))
        
        if isPartialReq then
            // partition function type props into maps of either async or sync functions based on type signature
            let asyncFunctions, syncFunctions =
                functions
                |> Map.partition (fun _ v -> 
                    match v with
                    | :? (unit -> Task<obj>) -> true
                    | :? (unit -> Async<obj>) -> true
                    | b -> false
                )
            
            // convert tasks to async values, separate keys and values
            let asyncKeys, asyncValues =
                asyncFunctions 
                    |> Map.map (fun _ y ->
                        match y with 
                        | :? (unit -> Task<obj>) as f -> f () |> Async.AwaitTask
                        | :? (unit -> Async<obj>) as f -> f ()
                        | b -> failwith $"unable to handle func prop with type: {b.GetType()}")
                        |> Map.toList
                        |> List.fold (fun (keys,values) (k,v) -> ( (k::keys),(v::values) )) ([],[])
            
            // evaluate the non-async functions
            let evaluatedSyncs =
                syncFunctions 
                    |> Map.map (fun _ y ->
                        match y with 
                        | :? (unit -> obj) as f -> f ()
                        | b -> failwith $"unable to handle func prop with type: {b.GetType()}" )
            
            // evaluate all the async functions in parallel
            let! asyncFuncValues = Async.Parallel asyncValues

            // zip the keys back to the values and convert back to a Map structure
            let evaluatedAsyncs = 
                asyncFuncValues 
                |> List.ofArray
                |> List.zip asyncKeys
                |> Map.ofList

            // merge the evaluated async and sync function maps
            let evaluatedMerged =
                Map.union evaluatedAsyncs evaluatedSyncs

            // return the merged evaluated functions and non-functions
            return Map.union nonFunctions evaluatedMerged

        else 
            // ignore all function type props on full page loads
            return nonFunctions
        }
        
    let generatePage (nextHandler: Page -> HttpHandler) ctx componentName (props:Map<string,obj>) (url:string option) version : HttpHandler =
        fun next ctx -> 
            task {
                let! evaluatedProps = handleProps ctx componentName props
                let page =
                    {
                        ``component`` = componentName
                        props = evaluatedProps
                        version = version
                        url = defaultArg url (ctx.Request.GetEncodedPathAndQuery())
                    }
                return! (nextHandler page) next ctx
            }
        
    let checkRedirect : HttpHandler =
        fun next ctx -> 
            if 
                [HttpMethods.Put ; HttpMethods.Patch; HttpMethods.Delete] |> List.contains ctx.Request.Method && 
                [ 301; 302] |> List.contains ctx.Response.StatusCode 
            then
                ctx.SetStatusCode 303
            next ctx

    let forceRefresh : HttpHandler =
        fun next ctx -> 
            ctx.SetHttpHeader("X-Inertia", "true")
            ctx.SetHttpHeader("X-Inertia-Location",ctx.Request.GetEncodedUrl())
            ctx.SetContentType("text/html")
            next ctx

    /// Validate CSRF for both client initiated inertia reqs and full page server reload reqs
    let handleRequest version : HttpHandler =
        fun next ctx -> 
            task {
                // check if this request initiates from inertiajs
                if ctx.Request.IsInertia then
                    // check header for token sent by client and for matching cookie set by server
                    match ctx.Request.Headers.XSRFToken, ctx.GetCookieValue("XSRF-TOKEN") with
                    | Some token, Some cookie ->
                        // verify they match
                        if token = cookie then
                            // pass through to next handler
                            // if GET
                            if ctx.Request.Method = HttpMethods.Get then
                                // check asset version
                                match ctx.Request.Headers.InertiaVersion with
                                | Some a when a <> version ->
                                    return! forceRefresh next ctx
                                // versions match so pass through to response handler
                                | _ -> 
                                    return! next ctx
                            // Other method type so check if redirect
                            else
                                return! checkRedirect next ctx
                        else
                            // clear reponse, set 403 status and return early
                            return! (clearResponse >=> setStatusCode StatusCodes.Status403Forbidden) earlyReturn ctx
                    | _ ->
                        return! (clearResponse >=> setStatusCode StatusCodes.Status419AuthenticationTimeout) earlyReturn ctx
                else
                    let antiFrg = ctx.GetService<IAntiforgery>()
                    // this is true if request uses a safe HTTP method or contains a valid antiforgery token
                    let! isValidServerCSRF = antiFrg.IsRequestValidAsync ctx
                    if isValidServerCSRF then
                        // if we have valid CSRF tokens (cookies and headers match) then set CSRF token cookie for client calls to mirror back via header
                        let tokenSet = ctx.GetService<IAntiforgery>().GetTokens(ctx)
                        let options = new CookieOptions()
                        options.SameSite <- SameSiteMode.Strict
                        ctx.Response.Cookies.Append("XSRF-TOKEN",tokenSet.CookieToken,options)
                        // pass through to response handler
                        return! next ctx
                    else 
                        return! (clearResponse >=> setStatusCode StatusCodes.Status403Forbidden) earlyReturn ctx
            }

    let setResponse (withTemplate: string -> XmlNode) (page:Page) : HttpHandler =
        fun next ctx ->
            if ctx.Request.IsInertia then
                ctx.SetHttpHeader("Vary","Accept")
                ctx.SetHttpHeader("X-Inertia","true")
                ( page |> json) next ctx
            else
                (page.toJson() |> withTemplate |> htmlView) next ctx

let redirectUrl baseUrl forceBase (ctx:HttpContext) : string =
    if forceBase then baseUrl else
    match ctx.Request.Headers.InertiaModalRedirectUrl with
    | Some redirectUrl -> redirectUrl
    | None ->
        match ctx.Request.Headers.TryGetReferer with
        | Some referer when ctx.Request.IsInertia ->
            referer
        | _ ->
            baseUrl

let makeModalPage (modal:ModalComponent) version url ctx =
    let mergedProps = Handlers.mergeWithSharedProps Map.empty<string,obj> ctx
    let pageProps = mergedProps.Add("modal",modal)
    {
        props = pageProps
        version = version
        url = url
    }

let setModalResponse (modalPage:ModalPage) : HttpHandler =
        fun next ctx ->
            ctx.SetHttpHeader("X-Inertia-Modal","true")
            ( modalPage |> json) next ctx


let handleModalRequest modalComponentName modalProps refreshBackdrop baseUrl forceBase withTemplate version: HttpHandler =
    fun next ctx ->
        task {
            let! executedProps = Handlers.handleProps ctx modalComponentName modalProps
            let redirectUrl = redirectUrl baseUrl forceBase ctx
            // construct the modal component
            let modalComponent = 
                {
                    ``component`` = modalComponentName
                    props = executedProps
                    redirectUrl = redirectUrl
                    key = defaultArg ctx.Request.Headers.InertiaModalKey (Guid.NewGuid().ToString())
                }
            
            if ctx.Request.IsInertia && not refreshBackdrop then
                // render the modal
                let modalPage = makeModalPage modalComponent version baseUrl ctx 
                return! setModalResponse modalPage next ctx
            else
                // if not rendering the standard modal
                // check for partial component header
                match ctx.Request.Headers.InertiaPartialComponent with
                | Some partialComponent when ctx.Request.IsInertia ->
                    // render partialComponent
                    return! (Handlers.handleRequest version
                                >=> Handlers.generatePage withTemplate ctx partialComponent Map.empty<string,obj> (Some baseUrl) version)
                                next ctx
                | None ->
                    // redirect to url
                    return! redirectTo false redirectUrl next ctx
        
        }


[<AutoOpen>]
module Core =
    open Handlers

    
    let shareData (props:HttpContext -> Map<string,obj>) : HttpHandler =
        fun next ctx ->
            ctx.Items["InertiaSharedData"] <- (props ctx)
            next ctx

    let renderInertia componentName (props:Map<string,obj>) withTemplate assetsVersion url : HttpHandler =
        fun next ctx ->
            (handleRequest assetsVersion
                // >=> checkInertiaRequestAndVersion assetsVersion
                >=> generatePage (setResponse withTemplate) ctx componentName props url assetsVersion)
                next ctx

type MiddleWare () =
    member val RootView = ""

type InertiaResponse (componentName:string,props:Map<string,obj>,rootView:(string->XmlNode)option,?version:string) =
    let defaultRootView dataPage =
        html [_lang "en"] [
            head [] [
                title [] [ str "Index" ]
                
            ]
            body [] [
                div [_id "app" ; attr "data-page" dataPage ] []
                script [ _type "text/javascript" ; _src "/js/index.js"] []
            ]
        ]
    member val ComponentName = componentName
    member val Props = props with get, set
    member val RootView = defaultArg rootView defaultRootView with get, set
    member val Version = defaultArg version "" with get, set
    member val ViewData = Map.empty<string,obj> with get, set
    member x.With (props:Map<string,obj>) =
        x.Props <- Map.union x.Props props
    member x.WithViewData (data:Map<string,obj>) =
        x.ViewData <- Map.union x.ViewData data
    member x.SetRootView (rootView:string -> XmlNode) =
        x.RootView <- rootView
    member x.ToResponse () : HttpHandler =
        fun next ctx ->
            task {
                let! propResult = Handlers.handleProps ctx x.ComponentName x.Props
                let page =
                    {
                        ``component`` = x.ComponentName
                        props = propResult
                        version = x.Version
                        url = ctx.Request.GetEncodedPathAndQuery()
                    }
                ctx.SetHttpHeader("Vary","Accept")
                ctx.SetHttpHeader("X-Inertia","true")
                return! json page next ctx

            }

type Inertia (?rootView:string->XmlNode,?version:string) =
    member val RootView = rootView with get, set
    member val SharedProps = Map.empty<string,obj> with get, set
    member val Version : string = defaultArg version "1" with get, set
    member x.SetRootView(template:string -> XmlNode) = 
        x.RootView <- Some template
    member x.Share(shared:Map<string,obj>) =
        x.SharedProps <- Map.union shared x.SharedProps
    member x.GetShared () = x.SharedProps
    member x.FlushShared () = x.SharedProps <- Map.empty<string,obj>
    member x.GetVersion () = x.Version
    member x.SetVersion (version:string) = x.Version <- version
    member x.Render(componentName:string,props:Map<string,obj>) =
        let mergedProps = Map.union x.SharedProps props
        new InertiaResponse(componentName,mergedProps,rootView=x.RootView,version=x.GetVersion())

type InertiaModal(componentName:string,props:Map<string,obj>,version:string) =
    member val BaseUrl : string = "" with get, set
    member val RefeshBackdrop : bool = false with get, set
    member val ForceBase : bool = false with get, set
    member val Props : Map<string,obj> = props
    member val ComponentName : string = componentName
    member val Version : string = version with get, set
    member x.SetBaseUrl (url:string) = x.BaseUrl <- url
    member x.SetRefreshBackdrop (set:bool) = x.RefeshBackdrop <- set
    member x.SetForceBase (set:bool) = x.ForceBase <- set

    member x.RedirectUrl (ctx:HttpContext) =
        if x.ForceBase then
            x.BaseUrl
        else 
            match ctx.Request.Headers.InertiaModalRedirectUrl, ctx.Request.Headers.TryGetReferer with
            | Some url, _ -> url
            | None, Some ref -> ref
            | _ ->
                x.BaseUrl

    member x.Component ctx : ModalComponent =
        {
            ``component`` = x.ComponentName
            redirectUrl = x.RedirectUrl(ctx)
            props = x.Props
            key = defaultArg ctx.Request.Headers.InertiaModalKey (Guid.NewGuid().ToString())
        }

    member x.RenderModal () : HttpHandler =
        fun next ctx ->
            let mergedProps = Handlers.mergeWithSharedProps Map.empty<string,obj> ctx
            let modalComponent = x.Component ctx
            let pageProps = mergedProps.Add("modal",modalComponent)
            let page =
                {
                    props = pageProps
                    url = ctx.Request.GetEncodedPathAndQuery()
                    version = x.Version
                }
            ctx.Response.Headers.Add("X-Inertial-Modal","true")
            json page next ctx

    member x.Render () : HttpHandler =
        fun next ctx ->
            task {
                if ctx.Request.IsInertia && not x.RefeshBackdrop then
                    return! x.RenderModal () next ctx
                else
                    match ctx.Request.Headers.InertiaPartialComponent with
                    | Some partialComponent when ctx.Request.IsInertia ->
                        let inertia = Inertia().Render(partialComponent,Map.empty<string,obj>).ToResponse()
                        return! inertia next ctx
                    | _ ->
                        return! redirectTo false (x.RedirectUrl ctx) next ctx
            }