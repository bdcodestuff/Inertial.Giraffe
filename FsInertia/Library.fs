module FsInertia

open System
open System.Threading.Tasks
open Newtonsoft.Json
open Microsoft.FSharpLu.Json
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Http.Extensions
open Microsoft.AspNetCore.Antiforgery
open Microsoft.Extensions.Primitives
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.DependencyInjection.Extensions
open System.Runtime.CompilerServices
open Giraffe
open Giraffe.ViewEngine
open FSharpx.Collections
open FSharp.Reflection

// JSON
let Formatting = Compact.CamelCaseNoFormatting.CompactCamelCaseNoFormattingSettings.formatting
let Settings = Compact.CamelCaseNoFormatting.CompactCamelCaseNoFormattingSettings.settings

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

    /// Get the referer
    member this.TryGetReferer with get () = hdr this "Referer"


/// Extensions for the request object
type HttpRequest with

    /// Check whether this request was initiated from Inertia
    member this.IsInertia with get () = this.Headers.Inertia |> Option.defaultValue false

/// evaluate any "lazy" props based on client request for full v. partial reload
let private evaluateProps (ctx:HttpContext) (componentName:string) (props:Map<string,obj>) =
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
        
        let functions, nonFunctions =
            filteredProps
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

type FlashType =
    | Success
    | Info
    | Error'
    | Warning

type Flash = 
    {
        msg : string
        flashType : FlashType
    }

type Page =
    {
        ``component`` : string
        props : Map<string,obj>
        version : string
        url : string
    }

    member x.toJson () =
        JsonConvert.SerializeObject(x, Formatting, Settings)

[<AutoOpen>]
module Core =

    type Inertia (?sharePropHandler:HttpHandler,?rootView:string -> XmlNode) =
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
        // default shared handler does nothing
        let defaultSharedPropHandler () : HttpHandler =
            fun next ctx -> next ctx
        
        member val SharedPropHandler = defaultArg sharePropHandler (defaultSharedPropHandler ()) with get, set
        member val RootView = defaultArg rootView defaultRootView with get, set
        member val SharedProps = Map.empty<string,obj> with get, set
        member val Version : string = "1" with get, set
        

        member private x.SharePropsHandler () = 
            fun next ctx ->
                task {
                    x.FlushShared() |> ignore
                    return! x.SharedPropHandler next ctx
                }
        
        member x.SetRootView(templateFn:string -> XmlNode) = 
            x.RootView <- templateFn
            x

        member x.ShareProp(k:string,v:obj) =
            x.SharedProps <- x.SharedProps.Add(k,v)
            x
        member x.SharePropMap(map:Map<string,obj>) =
            x.SharedProps <- Map.union x.SharedProps map
            x
        member x.Unshare(key:string) =
            x.SharedProps <- x.SharedProps.Remove(key)
            x
        member x.GetShared () = 
            x.SharedProps
        member x.FlushShared () = 
            x.SharedProps <- Map.empty<string,obj>
            x
        member x.GetVersion () = 
            x.Version
        member x.SetVersion (version:string) = 
            x.Version <- version
            x
        member _.Location(url:string) : HttpHandler =
            fun next ctx ->
                ctx.SetHttpHeader("X-Inertia","true")
                ctx.SetHttpHeader("X-Inertia-Location",url)
                ctx.SetContentType("text/html")
                redirectTo false url next ctx
        member x.Component(componentName:string) =
            // send shared props to response
            new InertiaResponse(componentName,x.SharePropsHandler(),x.GetShared(),rootView=x.RootView)
          
    and InertiaResponse (componentName:string,sharedPropsHandler:HttpHandler,sharedProps:Map<string,obj>,rootView:(string->XmlNode)) =        
        member val ComponentName = componentName
        member val SharePropsHandler = sharedPropsHandler with get, set
        member val Props = sharedProps with get, set
        member val RootView = rootView with get, set
        member x.WithProp(k:string,v:obj) =
            x.Props <- x.Props.Add(k,v)
            x
        member x.WithPropMap (map:Map<string,obj>) =
            x.Props <- Map.union x.Props map
            x

        member _.ReturnJsonPage (page:Page) : HttpHandler =
            fun next ctx ->
                task {
                    ctx.SetHttpHeader("X-Inertia","true")
                    ctx.SetHttpHeader("Vary","accept")
                    return! json page next ctx
                }

        member _.ForceRefresh (url) : HttpHandler =
            fun next ctx ->
                task {
                    ctx.SetHttpHeader("X-Inertia","true")
                    ctx.SetHttpHeader("X-Inertia-Location",url)
                    ctx.SetContentType("text/html")
                    ctx.SetStatusCode StatusCodes.Status409Conflict
                    return! next ctx
                }
                

        member x.ResponseHandler (?url:string,?version:string) : HttpHandler =
            fun next ctx ->
                task {
                    let v = defaultArg version "1"
                    let url = defaultArg url (ctx.Request.GetEncodedPathAndQuery())
                    let! propResult = evaluateProps ctx x.ComponentName x.Props
                    let page =
                        {
                            ``component`` = x.ComponentName
                            props = propResult
                            version = v
                            url = url
                        }
                    
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
                                    | Some a when a <> v ->
                                        return! x.ForceRefresh(url) next ctx
                                    // versions match so pass through to json response
                                    | _ -> 
                                        return! x.ReturnJsonPage page next ctx
                                // Other method type so check if redirect
                                else
                                    if
                                        [ HttpMethods.Put ; HttpMethods.Patch; HttpMethods.Delete ] 
                                            |> List.contains ctx.Request.Method && 
                                        [ StatusCodes.Status301MovedPermanently; StatusCodes.Status302Found ] |> List.contains ctx.Response.StatusCode 
                                    then
                                        ctx.SetStatusCode StatusCodes.Status303SeeOther
                                        return! next ctx
                                    else
                                        return! x.ReturnJsonPage page next ctx
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
                            // pass through json as string to body data-page tag in full page handler
                            return! (page.toJson() |> x.RootView |> htmlView) next ctx
                        else 
                            return! (clearResponse >=> setStatusCode StatusCodes.Status403Forbidden) earlyReturn ctx  
                }
        member x.Render (?url:string,?version:string,?skipShared:bool) =
            match url, version, skipShared with
            | Some url, Some version, Some true ->
                warbler (fun _ -> x.ResponseHandler(url=url, version=version))
            | Some url, None, Some true ->
                warbler (fun _ -> x.ResponseHandler(url=url))
            | None, Some version, Some true ->
                warbler (fun _ -> x.ResponseHandler(version=version))

            | Some url, Some version, _ ->
                warbler (fun _ -> x.SharePropsHandler >=> x.ResponseHandler(url=url, version=version))
            | Some url, None, _ ->
                warbler (fun _ -> x.SharePropsHandler >=> x.ResponseHandler(url=url))
            | None, Some version, _ ->
                warbler (fun _ -> x.SharePropsHandler >=> x.ResponseHandler(version=version))
            | _ ->
                warbler (fun _ -> x.SharePropsHandler >=> x.ResponseHandler())

    [<Extension>]
    type ServiceCollectionExtensions() =
        /// <summary>
        /// Adds default Inertia service to the ASP.NET Core service container.
        /// </summary>
        /// <returns>Returns an <see cref="Microsoft.Extensions.DependencyInjection.IServiceCollection"/> builder object.</returns>
        [<Extension>]
        static member AddInertia(svc : IServiceCollection, sharePropHandler : HttpHandler) =
            svc.TryAddSingleton<Inertia>(fun _ -> Inertia(sharePropHandler))
            svc
