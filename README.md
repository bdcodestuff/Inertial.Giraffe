# Inertial.Giraffe

## What is this?

You're looking at an attempt (by an FSharp novice) to bring [InertiaJS](https://inertiajs.com/) to the FSharp ecosystem with a few additional bells and whistles.

This library is a [Giraffe server plugin](https://github.com/bdcodestuff/Inertial.Giraffe) that reads the Http headers of incoming requests from a Sutil app using the complementary [Inertial.Client](https://github.com/bdcodestuff/Inertial.Client) library and determines how to respond accordingly.

## In Detail

To make this work you need to do the following:
1. Create a Giraffe app like normal
2. Install the Inertial.Giraffe package from nuget
3. Create a Netstandard2.0 library project in the same solution with the server-side app that sits "above" both the Server app and the Client app in the same solution.  Make sure both server and client reference this project.  In this library (let's call it the "Common" library for demonstration purposes) you define the app domain with top-level "Props" and "Shared" types that the server and client will both reference.  Props are types describing data available for a given "page".  Shared is a type describing data that gets made available to all components all of the time -- think details of the signed-in user or flash messages.  The top-level types also need functions (or as below static methods) that decode themselves from JSON.  I chose to use Thoth.Json for this task because it has reliable "auto" decoders and for more complex scenarios allows for very intuitive composition.  The decoder function for the top-level Props DU should take in a string name that is pattern matched to determine which child decoder is needed (see below for an example implementation):
   ```fsharp
   open Thoth.Json
   
   // some helper functions for our decoders
   module Helpers =
           
        let resultDecoder<'T> (decoder: Decoder<'T>) =
        
            let decoder: Decoder<Result<'T,string>> =                
                    let decodeOK =
                        Decode.field "Ok" decoder |> Decode.map Ok
                    let decodeError =
                        Decode.field "Error" (Decode.string) |> Decode.map Error

                    Decode.oneOf [ decodeOK ; decodeError ]
                
            decoder
         
        let asyncChoice2Decoder<'T> (placeholder : Async<Result<'T,string>>) (decoder: Decoder<'T>) =
            let decoder =                
                let decodeChoice1 =
                    Decode.field "Choice1Of2" (emptyDecoder |> Decode.andThen (fun _ ->  Decode.succeed (Choice1Of2 placeholder) ))
            
                let decodeChoice2 =
                    Decode.field "Choice2Of2" (resultDecoder decoder) |> Decode.map Choice2Of2

                Decode.oneOf [ decodeChoice1 ; decodeChoice2 ]
            
            decoder
            
   
   type Widget {
        name: string
        description: string
   }
   static member decoder = Decode.Auto.generateDecoder<Widget>()
    
   type User = {
        email : string
        username : string
   }
   static member decoder = Decode.Auto.generateDecoder<User option>()
   static member encoder userOpt = Encode.Auto.generateEncoder<User option>()
   
   type IndexPage = {
        widgets : Widget list
        asyncWidgets : Choice<Async<Result<Widget list,string>>,Result<Widget list,string>>>
   }
   static member decoder =
        Decode.object (fun get ->
            {
                widgets = get.Required.Field "widgets" (Decode.list Widget.decoder)
                asyncWidgets =
                    get.Required.Field
                        "asyncWidgets"
                        (Helpers.asyncChoice2Decoder
                             (async { return Ok <| [] }) // this is a placeholder that has the same type signature
                             Widget.decoder)
                        
            })
   
   type Props =
        | Index of IndexPage
        static member index = nameof Props.Index
   
        static member decoder (name: string) : Decoder<Props option> = 
        // note that this decoder is a function that takes a string matching the component name 
        // and returns a decoder that has been mapped back to Option<Props>
            match name with
            | name when name = Props.index ->
                IndexPage.decoder 
                |> Decode.map Index
                |> Decode.map Some
            | notFound -> 
                 failwith 
                     $"Could not find matching decoder for component named: {notFound}"
   
   type Shared = 
        {
            user : User option
            flashMessage = string option
        }
        let extra =
            Extra.empty
            |> Extra.withCustom User.encoder User.decoder
        
        Decode.Auto.generateDecoder<Shared option>(extra=extra)
    
   ```
4. Create a Sutil app and the Inertial.Client package [see here]()
5. In your Giraffe startup, add the Inertia Service to your IServiceCollection.  The services.AddInertia extension function has the following type signature:
```fsharp
   services.AddInertia<'Props,'Shared,'SSE>(
    svc : IServiceCollection,
    jsPath: string,
    cssPath: string,
    resolver: 'Props -> string,
    shareFn: HttpContext -> Task<'Shared>,
    sseInit:'SSE)
```
9. 'Props and 'Shared are defined from Props and Shared types in the "Common" library and 'SSE generic type refers to the server sent messsage type that is observed -- most often just some JSON in string form
10. The jsPath and cssPath arguments refer to the path of the js app bundle and cssPath bundle in the wwwroot folder of the web app.  This should be customized to your specific scenario.
11. The resolver function takes in a 'Props and returns the string name of the associated component.  In my example below I attached an instance method to the 'Props type that pattern matches and returns the name:
```fsharp
// based on this type definition in your "Common" library project
type Props =
    | Index of Index
    
    member x.name =
        match x with
        | Index _ -> Props.index
    
    static member index = nameof Props.Index

// you can use the following as your "resolver" argument:
let resolverFn = fun prop -> prop.name

``` 
12. The shareFunction takes the HttpContext and returns an instance of the 'Shared type wrapped in Task.  The idea here is that this function runs each time the app loads a component and it pulls in data that should be included in each view, think signed in user data or flash messages.  This is an example implementation assuming the use of Asp.Identity:
```fsharp

// assuming the following Shared type in "Common" project

type Shared =
    {
        user: User option
    }
    and User =
    {
        email : string
        id : string
    } 



open Inertial.Giraffe

module SharedHandler =
    let shareFn (ctx:HttpContext) =
        task {
            if ctx.User.Identity.IsAuthenticated then 
                let userIdClaim = ctx.User.Claims |> Seq.tryFind (fun x ->  x.Type = ClaimTypes.NameIdentifier)                  
            
                match userIdClaim with
                | Some claim ->
                    // lookup user using identity manager
                    let userManager = ctx.GetService<UserManager<IdentityUser>>()
                    let! user = userManager.FindByIdAsync claim.Value

                    // data gets shared across current request/response
                    return { user = Some { email = user.Email; id = claim.Value } }
                | None ->
                    return { user = None }
                
            else
                return { user = None }
               
        }
```
13. The sseInit argument is the starting message.  If you 'SSE type is string this could be an empty "".  If you are using a more complex JSON string to pass messages it might be some initialized record type converted to JSON like below:
```fsharp

open Fable.Remoting.Json
open Newtonsoft.Json

// assuming you define a fableConverter
let fableConverter = FableJsonConverter() :> JsonConverter

// if you have a custom type 
type InertialSSEEvent =
        {
            title : string
            connectionId: string option
            predicates : Predicates
            firedOn : System.DateTime
        }
        
        member x.toJson() = JsonConvert.SerializeObject(x,converters=[|fableConverter|])
        
        static member empty () =
            { title = ""; connectionId = None; predicates = { predicates = [||]; propsToEval = EvalAllProps  } ; firedOn = System.DateTime.UtcNow }.toJson()

// then your sseInit would be
let sseInit = InertialSSEEvent.empty()
```
14. Putting it all together you can add Inertia to the app:
```fsharp

// include this line to your startup when adding services to you IServiceCollection
services.AddInertia<Props,Shared,string>("/js/App.js","/css/style.css",(_.name),SharedHandler.shareFn,InertialSSEEvent.empty()) |> ignore
```