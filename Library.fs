namespace Inertial.Giraffe

open System
open System.Threading.Tasks
open System.Reactive.Subjects
open FSharp.Control.Reactive
open Microsoft.FSharp.Core
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Http.Extensions
open Microsoft.AspNetCore.Antiforgery
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.DependencyInjection.Extensions
open System.Runtime.CompilerServices
open Newtonsoft.Json
open FableJson
open Types
open Giraffe
open Giraffe.ViewEngine
open Reflection

[<AutoOpen>]
module Core =
    
    
    type RealTimePredicates =
    | ComponentIsOneOf of string array
    | ComponentIsAnyExcept of string array
    | ComponentIsAny
    | UserIdIsOneOf of string array
        
    type PropsToEval =
    | Eager // evaluate all async values before returning
    | Lazy // evaluate all non-async values but purposely leave async unevaluated
    | EagerOnly of string array // evaluate all non-async AND evaluate the async values with member name specified in the string array
       
    type ReloadOnMount =
        {
            shouldReload : bool
            propsToEval : PropsToEval option
        }
       
    type Predicates =
        {
            predicates : RealTimePredicates array
            propsToEval : PropsToEval
        }
        
    type NextSSE<'SSE> =
        {
            nextMsg : Option<string -> string -> 'SSE>
            nextPred : bool option
        }
        
    type InertialSSEEvent =
        {
            id : Guid
            title : string
            connectionId: string
            predicates : Predicates
            origin : string 
            firedOn : DateTime
        }
        static member empty () =
            {
                id = Guid.Empty
                title = ""
                connectionId = ""
                predicates = { predicates = [||]; propsToEval = Eager  }
                origin = ""
                firedOn = DateTime.UtcNow
            }.toJson()
        
        /// Create a new SSE event that is sent to client as json for decoding
        static member create (title:string) (propsToEvaluate: PropsToEval) (ifPredicatesMatch : RealTimePredicates list) url cid =
            {
                id = Guid.NewGuid()
                title = title
                connectionId = cid
                predicates = { predicates = (Array.ofList ifPredicatesMatch) ; propsToEval = propsToEvaluate }
                origin = url
                firedOn = DateTime.UtcNow
            }.toJson()
            
        member x.toJson() = JsonConvert.SerializeObject(x,converters=[|fableConverter|])
        
    type Page<'Props,'Shared> =
        {
            ``component`` : string
            version : string
            connectionId: string
            url : string
            title : string
            props : 'Props option
            refreshOnBack : bool
            reloadOnMount : ReloadOnMount
            realTime : bool
            shared : 'Shared
        }    
        member x.toJson() =
            JsonConvert.SerializeObject(x,converters=[|fableConverter|])
    
    
    let private partialReq (ctx:HttpContext) (componentName:string) =
        // Check if partial data request with specified component name
        let isPartialReq, filter =
            match ctx.Request.Headers.InertialPartialData, ctx.Request.Headers.InertialPartialComponent with
            | Some partialData, Some comp when comp = componentName ->
                true,
                partialData.Split(',') 
                |> Array.filter (fun x -> String.IsNullOrEmpty x |> not)
                |> Array.map (_.Trim())
            | _ ->
                false,
                [||]
                
        (isPartialReq, filter)
       
    let private fullReq (ctx:HttpContext) (componentName:string) =
        // Check if full data request with specified component name
        let isFullReq =
            match ctx.Request.Headers.InertialFullComponent with
            | Some comp when comp = componentName -> true
            | _ -> false
                
        isFullReq
               
    let private isSSE (ctx:HttpContext) = ctx.Request.IsInertialSSE
    
    let private isReload (ctx:HttpContext) = ctx.Request.IsInertialReload
    
    /// <summary>
    /// Inertia Options Class
    /// </summary>
    /// <returns>Inertia Options with default settings</returns>
    type InertiaOptions (?jsPath:string,?cssPath:string,?headView:XmlNode,?rootView:string -> XmlNode) =
        
        let defaultJsPath = "/js/index.js"
        
        let defaultCssPath = "/css/style.css"
        let defaultNProgressPath = "/css/nprogress.css"
        // if not otherwise specified, this is the root HTML head section for the app on first view and full-page reloads
        let defaultHtmlHead =
            head [] [
                title [] [ str "Index" ]
                link [ _rel "icon" ; _href "data:," ]
                link [ _rel "stylesheet" ; _href (defaultArg cssPath defaultCssPath) ]
                link [ _rel "stylesheet" ; _href defaultNProgressPath ]
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
        //let defaultSharedPropHandler () : HttpHandler = fun next ctx -> next ctx
            
        //member val SharedPropHandler : HttpHandler = defaultArg sharePropHandler (defaultSharedPropHandler ()) with get, set
        member val RootView = defaultArg rootView (defaultRootView defaultHtmlHead) with get, set
    
    /// <summary>
    /// Inertia Singleton Base Class
    /// </summary>
    /// <returns>Inertia object</returns>
    type Inertia<'Props,'Shared,'SSE> (options: InertiaOptions, shareFn: HttpContext -> Task<'Shared>, sseInit: 'SSE) =
        
        //member val Resolver = Reflection.resolver
        member val ShareFn = shareFn with get, set
        member val SSE = 
            sseInit
            |> Subject.behavior
            |> Subject.Synchronize
            with get, set

        member val RootView = options.RootView with get
                            
        /// Define component with 'Props value and optional page title (defaults to component name if not specified)
        member x.Component(props:'Props,?title:string) =
            
            // get name and evaluator function from resolver
            //let componentName = x.Resolver props
            let componentName = resolver props

            // send shared props to response
            InertiaResponse<'Props,'Shared,'SSE>(
                componentName = componentName,
                props = Some props,
                title = defaultArg title componentName, // if title is not specified default to using the component name
                sse=x.SSE,
                shareFn=x.ShareFn,
                rootView = x.RootView)
          
    and InertiaResponse<'Props,'Shared, 'SSE> (
        componentName:string,
        title:string,
        sse:ISubject<'SSE>,
        props:'Props option,
        shareFn,
        rootView:string->XmlNode) =        
        member val private ComponentName = componentName
        
        member val private NextSSE : NextSSE<'SSE> =
            { nextMsg = None ; nextPred = None } with get, set        
        member val private Title = title

        // initial response props are passed in from singleton shared props
        member val private Props = props with get, set
        member val private RootView = rootView with get, set
        member val private RefreshOnBack = false with get, set
        member val private ReloadOnMount = { shouldReload = false; propsToEval = None } with get, set // default is no reload
        member val private RealTime = true with get, set
        member val private ModifyShareDuringResponse = (false,id) with get, set
        
        // override root HTML view on a per response basis
        member x.OverrideRootView(templateFn:string -> XmlNode) = 
            x.RootView <- templateFn
            x      
               
        // indicate that this component should trigger the client side router to refresh itself when user lands on it via back/foward buttons
        member x.SetRefreshOnBack() =
            x.RefreshOnBack <- true
            x
            
        /// Indicate that this component should trigger the client side router to perform a partial data reload of itself after it first loads, evaluating just those fieldnames specified in toGet.
        /// This is the main mechanism used to achieve asynchronous data loading on the server.  Props that have an Async function type signature don't get evaluated on first load, but do get evaluated when the component reloads.
        member x.SetReloadOnMount(toGet: string list) =
            let data = Array.ofList toGet
            match data with
            | [||] ->
                //x.ReloadOnMount <- { shouldReload = true; propsToEval = Some Lazy }
                failwith "If SetReloadOnMount is specified the toGet array cannot be empty"
            | a ->
                x.ReloadOnMount <- { shouldReload = true; propsToEval = Some <| EagerOnly a }
            x
        
        /// Components listen for server-sent events at "/sse" by default; Calling this indicates that this component should not listen client-side.
        member x.DisableRealtime() =
            x.RealTime <- false
            x
            
        /// Pass in arbitrary function with signature 'Shared -> 'Shared that can alter the 'Shared data in this response only
        member x.UpdateShared(?updater:'Shared ->'Shared ) =
            let emptyShare = defaultArg updater id
            x.ModifyShareDuringResponse <- (true,emptyShare)
            x
                       
        // function to return JSON page handler
        member private _.ReturnJsonPage (page:Page<'Props,'Shared>) isReload : HttpHandler =
            fun next ctx ->
                task {
                    ctx.SetHttpHeader("X-Inertial","true")
                    if isReload then
                        ctx.SetHttpHeader("X-Inertial-Reload","true")
                    ctx.SetHttpHeader("Vary","accept")
                    return! json page next ctx
                }
                
        // function refresh if asset versioning is off
        member private _.ForceRefresh (url:string) : HttpHandler =
            fun next ctx ->
                task {
                    ctx.SetHttpHeader("X-Inertial-Location",url)
                    ctx.SetContentType("text/html")
                    ctx.SetStatusCode StatusCodes.Status409Conflict
                    return! next ctx
                }
              
        // Method to do change the SSE json message and trigger SSE event client-side
        member x.BroadcastSSE(nextMessage:string -> string -> 'SSE,?broadcastPredicate:bool) =
            x.NextSSE <- { nextMsg = Some nextMessage; nextPred = broadcastPredicate }
            x
                
        // main handler logic
        member private x.ResponseHandler
            (?url:string, ?connectionId:string, ?version:string) : HttpHandler =
                fun next ctx ->
                    task {
                        let version = defaultArg version "1" // use default of 1
                        let url = defaultArg url (ctx.Request.GetEncodedPathAndQuery())
                        let isPartial, filter = partialReq ctx x.ComponentName
                        let isFull = fullReq ctx x.ComponentName
                        let isSSE = ctx.Request.IsInertialSSE
                        let isReload = ctx.Request.IsInertialReload
                        
                        // check for client side id passed in via header
                        let connectionId =
                            match connectionId with
                            | Some serverId -> serverId
                            | None -> ctx.Request.InertialId // hit this branch on full page refresh; will be sent to client then back again in header on XML calls
                        
                        // Evaluate asynchronous props
                        let! propResultAsync = evaluated x.Props filter isPartial isFull
                        
                        // Cast back tp 'Props option type
                        let propResult = propResultAsync :?> 'Props |> Some

                        let shouldModifyShare, withFn = x.ModifyShareDuringResponse
                        
                        // Gets the shared props
                        let! sharedProps = shareFn ctx
                        
                        // Modifies the shared props based on user-specified function
                        let shared = if shouldModifyShare then withFn sharedProps else sharedProps
                        
                        let reloadOnMount : ReloadOnMount =
                            if isReload then
                                { shouldReload = false; propsToEval = None }
                            else
                                if isPartial || isFull then // full eager load or partial eager load
                                    { shouldReload = false; propsToEval = None }
                                else // if lazy or full page reload default to server-specified behavior for this component
                                    x.ReloadOnMount
                        
                        let page : Page<'Props,'Shared> = {
                            ``component``= x.ComponentName
                            connectionId = connectionId
                            props=propResult
                            shared=shared
                            version=version
                            url=url
                            title=x.Title
                            refreshOnBack=x.RefreshOnBack
                            reloadOnMount=reloadOnMount
                            realTime=x.RealTime 
                        }

                        let fireNextEvent () =
                            let matchesUserPred = defaultArg x.NextSSE.nextPred true
                            // only fire SSE on certain conditions
                            if matchesUserPred && not isSSE && not isReload then
                                match x.NextSSE.nextMsg with
                                | Some nextFn ->
                                    //printfn $"isSSE:{isSSE}, isReload:{isReload} -- sending SSE message "
                                    sse.OnNext(nextFn url connectionId)
                                | None -> ()
                        
                        
                        // check if this request initiates from inertia
                        if ctx.Request.IsInertial then
                            // check header for token sent by client and for matching cookie set by server
                            match ctx.Request.Headers.XSRFToken, ctx.GetCookieValue("XSRF-TOKEN") with
                            | Some token, Some cookie ->
                                // verify they match
                                if token = cookie then
                                    // pass through to next handler
                                    // if GET
                                    if ctx.Request.Method = HttpMethods.Get then
                                        // check asset version against latest version in headers
                                        match ctx.Request.Headers.InertialVersion with
                                        | Some a when a <> version ->
                                            return! x.ForceRefresh(url) next ctx
                                        // versions match so pass through to json response
                                        | _ -> 
                                            fireNextEvent()
                                            
                                            return! x.ReturnJsonPage page isReload next ctx
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
                                            fireNextEvent()
                                            return! x.ReturnJsonPage page isReload next ctx
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
                                
                                fireNextEvent()
                                // pass through json as string to body data-page tag in full page handler
                                return! (page.toJson() |> x.RootView |> htmlView) next ctx
                            else 
                                return! (clearResponse >=> setStatusCode StatusCodes.Status403Forbidden) earlyReturn ctx  
                }

            
        /// Public rendering function that calls HttpHandler compatible with Giraffe pipeline
        member x.Render (?url:string,?version:string) =
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

            response
            
    type ServiceCollectionExtensions() =
        /// <summary>
        /// Adds to the ASP.NET Core service container a default Inertial service and a custom JSON serializer that is compatible with the client-side inertial router
        /// Requires the following:
        /// 1.) Values specifying the public path of the js folder (jsPath) and css folder (cssPath)
        /// 2.) A Props union type that wraps each individual "page" record type
        /// 3.) A "shared" function that takes the HTTPContext and returns a Shared data record that is present for each component
        /// 4.) An initial server-sent event (SSE) message
        /// </summary>
        /// <returns>Returns an <see cref="Microsoft.Extensions.DependencyInjection.IServiceCollection"/> builder object.</returns>
        [<Extension>]
        static member AddInertia<'Props,'Shared,'SSE>(
                svc : IServiceCollection,
                jsPath: string,
                cssPath: string,
                shareFn: HttpContext -> Task<'Shared>,
                sseInit:'SSE
            ) =
                svc.AddSingleton<Json.ISerializer, FableRemotingJsonSerializer>() |> ignore
                svc.TryAddSingleton<Inertia<'Props,'Shared,'SSE>>(fun _ -> Inertia(InertiaOptions(jsPath=jsPath,cssPath=cssPath),shareFn,sseInit))
                svc
