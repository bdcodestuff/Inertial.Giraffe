module Inertial.Giraffe

open System
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
open FSharp.Reflection

// JSON
let Formatting = Compact.CamelCaseNoFormatting.CompactCamelCaseNoFormattingSettings.formatting
let Settings = Compact.CamelCaseNoFormatting.CompactCamelCaseNoFormattingSettings.settings


/// Determine if the given header is present
let private hdr (headers : IHeaderDictionary) hdr =
    match headers[hdr] with it when it = StringValues.Empty -> None | it -> Some it[0]

/// Helper to print prop function signature
let private funString o =
    let rec loop nested t =
        if FSharpType.IsTuple t then
            FSharpType.GetTupleElements t
            |> Array.map (loop true)
            |> String.concat " * "
        elif FSharpType.IsFunction t then
            let fs = if nested then sprintf "(%s -> %s)" else sprintf "%s -> %s"
            let domain, range = FSharpType.GetFunctionElements t
            fs (loop true domain) (loop false range)
        else
            t.FullName
    loop false (o.GetType())

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

let private partialReq (ctx:HttpContext) (componentName:string) =
    // Check if partial data request with specified component name
    let isPartialReq, filter =
        match ctx.Request.Headers.InertiaPartialData, ctx.Request.Headers.InertiaPartialComponent with
        | Some partialData, Some comp when comp = componentName ->
            true,
            partialData.Split(',') 
            |> Array.filter (fun x -> String.IsNullOrEmpty x |> not)
            |> Array.map (_.Trim())
        | _ ->
            false,
            [||]
            
    (isPartialReq, filter)
   
type Page<'Props,'Shared> =
    {
        ``component`` : string
        version : string
        url : string
        title : string
        props : 'Props option
        refreshOnBack : bool
        shared : 'Shared
    }    
    member x.toJson() =
        //JsonSerializer.Serialize(x,options)
        JsonConvert.SerializeObject(x, Formatting, Settings)

[<AutoOpen>]
module Core =
    
    /// <summary>
    /// Inertia Options Class
    /// </summary>
    /// <returns>Inertia Options with default settings</returns>
    type InertiaOptions (?jsPath:string,?cssPath:string,?headView:XmlNode,?rootView:string -> XmlNode,?sharePropHandler:HttpHandler) =
        
        let defaultJsPath = "/js/index.js"
        
        let defaultCssPath = "/js/style.css"
        // if not otherwise specified, this is the root HTML head section for the app on first view and full-page reloads
        let defaultHtmlHead =
            head [] [
                title [] [ str "Index" ]
                link [ _rel "stylesheet" ; _href (defaultArg cssPath defaultCssPath) ] 
            ]
        
        /// if not otherwise specified, this is the root HTML view function for the app on first view and full-page reloads.
        /// Takes the data page json object as argument and outputs XmlNode for Giraffe View Engine rendering       
        let defaultRootView head dataPage  =
            html [_lang "en" ; _style "height: 100vh;"] [
                head
                body [ _style "height: 100%;" ] [
                    div [_id "sutil-app" ; _style "height: 100%;" ; attr "data-page" dataPage ] []
                    script [ _src (defaultArg jsPath defaultJsPath) ] []
                ]
            ]
            
        // default shared handler does nothing
        let defaultSharedPropHandler () : HttpHandler = fun next ctx -> next ctx
            
        member val SharedPropHandler : HttpHandler = defaultArg sharePropHandler (defaultSharedPropHandler ()) with get, set
        member val RootView = defaultArg rootView (defaultRootView defaultHtmlHead) with get, set
    
    /// <summary>
    /// Inertia Singleton Base Class
    /// </summary>
    /// <returns>Inertia object</returns>
    type Inertia<'Props,'Shared> (options: InertiaOptions, resolver:'Props -> string * ('Props option -> bool -> string array -> Async<'Props option>), shared:'Shared) =
        
        member val Resolver = resolver with get
        member val SharedProps = shared with get, set
        member val Version : string = "1" with get, set
        member val RootView = options.RootView with get
        
        // internal share prop handler
        member private x.SharePropsHandler () = 
            fun next ctx ->
                x.FlushShared() |> ignore // clear the shared props on each req/resp cycle
                options.SharedPropHandler next ctx

        member x.ShareProp(s:'Shared) =
            x.SharedProps <- s
            x

        member x.GetShared () = 
            x.SharedProps
            
        member x.FlushShared () = 
            x.SharedProps <- shared
            x
        member x.GetVersion () = 
            x.Version
            
        member x.SetVersion (version:string) = 
            x.Version <- version
            x
            
        /// Location method will force a client-side redirect to the url specified
        member _.Location(url:string) : HttpHandler =
            fun next ctx ->
                ctx.SetHttpHeader("X-Inertia-Location",url)
                ctx.SetContentType("text/html")
                ctx.SetStatusCode StatusCodes.Status409Conflict
                next ctx

        //member x.Component(componentName:string,props:'Props,?title:string) =
        member x.Component(props:'Props,?title:string) =
            
            // get name and evaluator function from resolver
            let componentName, evaluator = x.Resolver props
            
            // send shared props to response
            InertiaResponse<'Props,'Shared>(
                componentName = componentName,
                props = Some props,
                title = defaultArg title componentName, // if title is not specified default to using the component name
                sharedPropsHandler = x.SharePropsHandler(),
                sharedProps = x.GetShared(),
                evaluator=evaluator,
                rootView = x.RootView)
          
    and InertiaResponse<'Props,'Shared> (
        componentName:string,
        title:string,
        sharedPropsHandler:HttpHandler,
        props:'Props option,
        sharedProps:'Shared,
        evaluator: 'Props option -> bool -> string array -> Async<'Props option>,
        rootView:string->XmlNode) =        
        member val private ComponentName = componentName
        member val private Title = title
        member val private SharePropsHandler = sharedPropsHandler with get, set
        // initial response props are passed in from singleton shared props
        member val private Props = props with get, set
        member val private Evaluator = evaluator with get, set
        member val private SharedProps = sharedProps with get, set
        member val private RootView = rootView with get, set
        member val private RefreshOnBack = false with get, set
        member val private ClearShareDuringResponse = (false,id) with get, set
        
        // override root HTML view on a per response basis
        member x.OverrideRootView(templateFn:string -> XmlNode) = 
            x.RootView <- templateFn
            x
            
        // add prop
        // member x.WithProp(p: 'Props) =
        //     x.Props <- Some p
        //     x                
               
        // indicate that this component should trigger the client side router to refresh itself when user lands on it via back/foward buttons
        member x.SetRefreshOnBack() =
            x.RefreshOnBack <- true
            x
            
        
        member x.UpdateShared(?emptyShare:'Shared ->'Shared ) =
            let emptyShare = defaultArg emptyShare id
            x.ClearShareDuringResponse <- (true,emptyShare)
            x
                       
        // function to return JSON page handler
        member private _.ReturnJsonPage (page:Page<'Props,'Shared>) : HttpHandler =
            fun next ctx ->
                task {
                    ctx.SetHttpHeader("X-Inertia","true")
                    ctx.SetHttpHeader("Vary","accept")
                    return! json page next ctx
                }
        // function refresh if asset versioning is off
        member private _.ForceRefresh (url:string) : HttpHandler =
            fun next ctx ->
                task {
                    ctx.SetHttpHeader("X-Inertia-Location",url)
                    ctx.SetContentType("text/html")
                    ctx.SetStatusCode StatusCodes.Status409Conflict
                    return! next ctx
                }
        // main handler logic
        member private x.ResponseHandler
            (?url:string,
             ?version:string) : HttpHandler =
            fun next ctx ->
                task {
                    let v = defaultArg version "1"
                    let url = defaultArg url (ctx.Request.GetEncodedPathAndQuery())
                    let isPartial, filter = partialReq ctx x.ComponentName
                    
                    // evaluate any asynchronous functions if is a partial reload of same component
                    let! propResultFn = x.Evaluator x.Props isPartial filter
 
                    let clearShare, withFn = x.ClearShareDuringResponse
                    
                    let sharedProps =
                        if clearShare then withFn x.SharedProps else x.SharedProps
                    
                    let page : Page<'Props,'Shared> = {
                        ``component``= x.ComponentName
                        props=propResultFn
                        shared=sharedProps
                        version=v
                        url=url
                        title=x.Title
                        refreshOnBack=x.RefreshOnBack
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
                                // clear response, set 403 status and return early
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
                            let options = CookieOptions()
                            options.SameSite <- SameSiteMode.Strict
                            ctx.Response.Cookies.Append("XSRF-TOKEN",tokenSet.CookieToken,options)
                            // pass through json as string to body data-page tag in full page handler
                            return! (page.toJson() |> x.RootView |> htmlView) next ctx
                        else 
                            return! (clearResponse >=> setStatusCode StatusCodes.Status403Forbidden) earlyReturn ctx  
                }
        
        // public rendering function 
        member x.Render (?url:string,?version:string,?skipShared:bool) =
            let skipShared = defaultArg skipShared false

            let response =
                match url, version with
                | Some url, Some version ->
                    x.ResponseHandler(url=url, version=version)
                | Some url, None ->
                    x.ResponseHandler(url=url)
                | None, Some version ->
                    x.ResponseHandler(version=version)
                | None, None ->
                    x.ResponseHandler()

            if skipShared then
                warbler (fun _ -> response)
            else
                warbler (fun _ -> x.SharePropsHandler >=> response)
            
    type ServiceCollectionExtensions() =
        /// <summary>
        /// Adds default Inertia service to the ASP.NET Core service container.
        /// A Props union type of individual page data and a Shared record type must be supplied at instantiation
        /// </summary>
        /// <returns>Returns an <see cref="Microsoft.Extensions.DependencyInjection.IServiceCollection"/> builder object.</returns>
        [<Extension>]
        static member AddInertia<'Props,'Shared>(svc : IServiceCollection, sharePropHandler : HttpHandler, jsPath: string, cssPath: string, resolver, shared:'Shared) =
            svc.TryAddSingleton<Inertia<'Props,'Shared>>(fun _ -> Inertia(InertiaOptions(jsPath=jsPath,cssPath=cssPath,sharePropHandler=sharePropHandler),resolver,shared))
            svc
