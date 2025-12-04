module Reflection

open System
open Inertial.Giraffe
open Inertial.Giraffe.Types
open Inertial.Lib
open Microsoft.FSharp.Core
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Quotations.Patterns

/// We assume that a Props value is a union case wrapping a single record value
/// This function takes the optional Props union case and returns a tuple of the inner union case boxed as obj and the type of the outer union
let innerRecord (props: 'Props option) =
    match props with
    | Some p ->
        let outerPropsType = p.GetType()
        if (FSharpType.IsUnion outerPropsType) then
            let out =
                FSharpValue.GetUnionFields (p, outerPropsType)
                |> snd
                |> Seq.head, outerPropsType
            out
        else failwith "Provided props are not in required union type format"
    | None -> failwith "No props found for evaluation"

// active recognizer for union cases by name
let (|UC|_|) e o =
      match e with
      | Lambdas(_,NewUnionCase(uc,_)) | NewUnionCase(uc,[]) ->
          if (box o = null) then
            // Need special case logic in case null is a valid value (e.g. Option.None)
            let attrs = uc.DeclaringType.GetCustomAttributes(typeof<CompilationRepresentationAttribute>, false)
            if attrs.Length = 1
               && (attrs.[0] :?> CompilationRepresentationAttribute).Flags &&& CompilationRepresentationFlags.UseNullAsTrueValue <> enum 0
               && uc.GetFields().Length = 0
            then Some []
            else None
          else
            let t = o.GetType()
            if FSharpType.IsUnion t then
              let uc2, fields = FSharpValue.GetUnionFields(o,t)
              let getGenType (t:System.Type) = if t.IsGenericType then t.GetGenericTypeDefinition() else t
              if uc2.Tag = uc.Tag && getGenType (uc2.DeclaringType) = getGenType (uc.DeclaringType) then
                Some(fields |> List.ofArray)
              else None
            else None
      | _ -> failwith "The UC pattern can only be used against simple union cases"

// ============================================================================
// Helper functions for path-based filtering
// ============================================================================

/// Check if a type is Deferred<_> (new simplified type)
/// Deferred is a discriminated union, so we need to check both the type itself
/// and its declaring type (for union cases like Pending, Loaded, Failed)
let private isDeferredType (t: Type) =
    // Direct check for the Deferred type itself
    (t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<Deferred<_>>) ||
    // Check for nested union case types like Deferred`1+Pending
    (t.DeclaringType <> null && t.DeclaringType.IsGenericType &&
     t.DeclaringType.GetGenericTypeDefinition() = typedefof<Deferred<_>>)

/// Check if a type is one of our AsyncData variants (legacy)
/// AsyncData cases like Choice2List are nested types, so we check both the type itself
/// and its declaring type (for union cases)
let private isAsyncDataType (t: Type) =
    // Direct check for the AsyncData type itself
    (t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<AsyncData<_>>) ||
    // Check for nested union case types like AsyncData`1+Choice2List
    (t.DeclaringType <> null && t.DeclaringType.IsGenericType &&
     t.DeclaringType.GetGenericTypeDefinition() = typedefof<AsyncData<_>>)

/// Check if a type is either Deferred<_> or AsyncData<_>
let private isAsyncFieldType (t: Type) =
    isDeferredType t || isAsyncDataType t

/// Check if a path matches the filter exactly or via wildcard
let private pathMatchesFilter (path: string) (filter: string array) =
    filter |> Array.exists (fun f ->
        f = "*" || f = path)

/// Check if we should recurse into this path (filter might target something deeper)
let private shouldRecurseIntoPath (path: string) (filter: string array) =
    filter |> Array.exists (fun f ->
        f = "*" ||
        f = path ||
        f.StartsWith(path + "."))

/// Evaluate the inner Choice value of an AsyncData, executing the async if filter matches
let private evaluateAsyncChoice (choiceOption: obj) (path: string) (filter: string array) (asyncDataName: AsyncDataName) =
    async {
        let shouldEvalThis = pathMatchesFilter path filter

        match choiceOption with
        | UC <@ Choice1Of2 @> [v] when shouldEvalThis ->
            // This is an unevaluated async - execute it
            let fsharpFuncArgs = choiceOption.GetType().GetGenericArguments()
            let asyncOfB = fsharpFuncArgs.[0]
            let genArgAsyncOfB = asyncOfB.GetGenericArguments()
            let typeBFromAsyncOfB = genArgAsyncOfB.[0]

            let asyncBoxer = typedefof<AsyncBoxer<_>>.MakeGenericType(typeBFromAsyncOfB)
                            |> Activator.CreateInstance
                            :?> IAsyncBoxer

            let choice2Boxer = typedefof<Choice2Boxer<_,_>>.MakeGenericType(fsharpFuncArgs)
                            |> Activator.CreateInstance
                            :?> IChoice2Boxer

            let! asyncResult = asyncBoxer.BoxAsyncResult v
            let choiceResult = Choice2Of2 asyncResult
            let result = choice2Boxer.Reboxer choiceResult
            let resultBox = choice2Boxer.BoxChoice2Result result
            let resultAsyncDataBox = choice2Boxer.ReboxToAsyncData resultBox asyncDataName

            return resultAsyncDataBox

        | UC <@ Choice1Of2 @> [v] ->
            // Async not in filter - rewrap without executing
            let fsharpFuncArgs = choiceOption.GetType().GetGenericArguments()

            let choice2Boxer = typedefof<Choice2Boxer<_,_>>.MakeGenericType(fsharpFuncArgs)
                            |> Activator.CreateInstance
                            :?> IChoice2Boxer
            let choiceResult = Choice1Of2 v
            let result = choice2Boxer.Reboxer choiceResult
            let resultBox = choice2Boxer.BoxChoice2Result result
            let resultAsyncDataBox = choice2Boxer.ReboxToAsyncData resultBox asyncDataName
            return resultAsyncDataBox

        | UC <@ Choice2Of2 @> [v] ->
            // Already evaluated (Choice2Of2) - rewrap without re-executing
            let fsharpFuncArgs = choiceOption.GetType().GetGenericArguments()

            let choice2Boxer = typedefof<Choice2Boxer<_,_>>.MakeGenericType(fsharpFuncArgs)
                            |> Activator.CreateInstance
                            :?> IChoice2Boxer
            let choiceResult = Choice2Of2 v
            let result = choice2Boxer.Reboxer choiceResult
            let resultBox = choice2Boxer.BoxChoice2Result result
            let resultAsyncDataBox = choice2Boxer.ReboxToAsyncData resultBox asyncDataName
            return resultAsyncDataBox
        | _ ->
            // Unknown case - return as-is (shouldn't happen)
            return choiceOption
    }

/// Evaluate a Deferred<'T> value, executing the async if it's Pending and filter matches
let private evaluateDeferred (value: obj) (path: string) (filter: string array) : Async<obj> =
    async {
        let shouldEvalThis = pathMatchesFilter path filter
        let typ = value.GetType()

        if not shouldEvalThis then
            // Not in filter - return as-is
            return value
        else
            // Get the inner type T from Deferred<T>
            // For F# discriminated unions, we use FSharpType to get union case info
            // which gives us the actual constructed generic type
            let unionType =
                let cases = FSharpType.GetUnionCases(typ)
                if cases.Length > 0 then
                    // The DeclaringType of a union case gives us the full constructed type
                    cases.[0].DeclaringType
                elif typ.IsGenericType && typ.GetGenericTypeDefinition() = typedefof<Deferred<_>> then
                    typ
                else
                    failwith $"Expected Deferred type but got {typ.FullName}"

            let innerType = unionType.GetGenericArguments().[0]

            // Create a DeferredBoxer<T> to handle the evaluation
            let deferredBoxer =
                typedefof<DeferredBoxer<_>>.MakeGenericType(innerType)
                |> Activator.CreateInstance
                :?> IDeferredBoxer

            return! deferredBoxer.EvaluateAsync(value)
    }

// ============================================================================
// Recursive evaluation engine
// ============================================================================

/// Recursively evaluate async fields in any object structure
let rec private evaluateValue (value: obj) (currentPath: string) (filter: string array) : Async<obj> =
    async {
        if isNull value then
            return value
        else
            let typ = value.GetType()

            // Case 1: Deferred<'T> - new simplified async type
            if isDeferredType typ then
                return! evaluateDeferred value currentPath filter

            // Case 2: AsyncData - legacy async type
            elif isAsyncDataType typ then
                return! evaluateAsyncData value currentPath filter

            // Case 3: F# Record - recurse into fields
            elif FSharpType.IsRecord typ then
                return! evaluateRecord value currentPath filter

            // Case 4: F# List - map over elements
            elif typ.IsGenericType && typ.GetGenericTypeDefinition() = typedefof<list<_>> then
                return! evaluateList value currentPath filter

            // Case 5: Array - map over elements
            elif typ.IsArray then
                return! evaluateArray value currentPath filter

            // Case 6: Option - unwrap and evaluate inner
            elif typ.IsGenericType && typ.GetGenericTypeDefinition() = typedefof<option<_>> then
                return! evaluateOption value currentPath filter

            // Case 7: Other types - return as-is
            else
                return value
    }

and private evaluateAsyncData (value: obj) (path: string) (filter: string array) : Async<obj> =
    async {
        match value with
        | UC <@ Choice2List @> [choiceOption] ->
            return! evaluateAsyncChoice choiceOption path filter AsyncDataName.Choice2List
        | UC <@ Choice2Option @> [choiceOption] ->
            return! evaluateAsyncChoice choiceOption path filter AsyncDataName.Choice2Option
        | UC <@ Choice2ResultList @> [choiceOption] ->
            return! evaluateAsyncChoice choiceOption path filter AsyncDataName.Choice2ResultList
        | UC <@ Choice2OptionListDecoder @> [choiceOption] ->
            return! evaluateAsyncChoice choiceOption path filter AsyncDataName.Choice2OptionList
        | _ ->
            return value
    }

and private evaluateRecord (value: obj) (currentPath: string) (filter: string array) : Async<obj> =
    async {
        let typ = value.GetType()
        let fields = FSharpType.GetRecordFields(typ)
        let fieldValues = FSharpValue.GetRecordFields(value)

        let! evaluatedFields =
            fields
            |> Array.mapi (fun i field ->
                async {
                    let fieldPath =
                        if String.IsNullOrEmpty currentPath then field.Name
                        else $"{currentPath}.{field.Name}"

                    // Only recurse if filter might target something at or below this path
                    if shouldRecurseIntoPath fieldPath filter then
                        return! evaluateValue fieldValues.[i] fieldPath filter
                    else
                        return fieldValues.[i]
                })
            |> Async.Parallel

        // Reconstruct the record using FSharpValue
        return FSharpValue.MakeRecord(typ, evaluatedFields)
    }

and private evaluateList (value: obj) (currentPath: string) (filter: string array) : Async<obj> =
    async {
        let typ = value.GetType()
        let elementType = typ.GetGenericArguments().[0]

        // Check if elements could contain async fields (records, Deferred, AsyncData, options)
        let elementNeedsEvaluation =
            FSharpType.IsRecord elementType ||
            isAsyncFieldType elementType ||
            (elementType.IsGenericType && elementType.GetGenericTypeDefinition() = typedefof<option<_>>)

        if not elementNeedsEvaluation then
            return value
        else
            // Convert list to seq for iteration
            let enumerable = value :?> System.Collections.IEnumerable
            let elements = [ for item in enumerable -> item ]

            let! evaluatedElements =
                elements
                |> List.mapi (fun i elem ->
                    async {
                        // Use index notation in path for list elements
                        let elemPath = $"{currentPath}[{i}]"
                        return! evaluateValue elem elemPath filter
                    })
                |> Async.Parallel

            // Reconstruct as F# list using reflection
            let listModule = typeof<list<_>>.Assembly.GetType("Microsoft.FSharp.Collections.ListModule")
            let ofArrayMethod = listModule.GetMethod("OfArray").MakeGenericMethod([|elementType|])

            let typedArray = Array.CreateInstance(elementType, evaluatedElements.Length)
            evaluatedElements |> Array.iteri (fun i v -> typedArray.SetValue(v, i))

            return ofArrayMethod.Invoke(null, [|typedArray|])
    }

and private evaluateArray (value: obj) (currentPath: string) (filter: string array) : Async<obj> =
    async {
        let arr = value :?> Array
        let elementType = value.GetType().GetElementType()

        // Check if elements could contain async fields (Deferred, AsyncData, records, options)
        let elementNeedsEvaluation =
            FSharpType.IsRecord elementType ||
            isAsyncFieldType elementType ||
            (elementType.IsGenericType && elementType.GetGenericTypeDefinition() = typedefof<option<_>>)

        if not elementNeedsEvaluation || arr.Length = 0 then
            return value
        else
            let! evaluatedElements =
                [| for i in 0 .. arr.Length - 1 -> arr.GetValue(i) |]
                |> Array.mapi (fun i elem ->
                    async {
                        let elemPath = $"{currentPath}[{i}]"
                        return! evaluateValue elem elemPath filter
                    })
                |> Async.Parallel

            let result = Array.CreateInstance(elementType, evaluatedElements.Length)
            evaluatedElements |> Array.iteri (fun i v -> result.SetValue(v, i))
            return box result
    }

and private evaluateOption (value: obj) (currentPath: string) (filter: string array) : Async<obj> =
    async {
        let typ = value.GetType()
        let cases = FSharpType.GetUnionCases(typ)
        let case, fields = FSharpValue.GetUnionFields(value, typ)

        if case.Name = "None" then
            return value
        else
            // It's Some(x) - evaluate the inner value
            let! evaluatedInner = evaluateValue fields.[0] currentPath filter
            let someCase = cases |> Array.find (fun c -> c.Name = "Some")
            return FSharpValue.MakeUnion(someCase, [|evaluatedInner|])
    }

// ============================================================================
// Main entry point
// ============================================================================

/// Evaluate async props in a Props union case, reconstructing with evaluated values
/// Uses FSharpValue for all reconstruction (no quotations)
let evaluated (propsToEval: 'Props option) (filter: string array) (isPartial: bool) (isFull: bool) : Async<obj> =
    async {
        let innerProps, outerPropsUCType = innerRecord propsToEval

        // Determine effective filter
        let effectiveFilter =
            if isFull then [|"*"|]
            elif isPartial then filter
            else [||] // No evaluation needed

        // Evaluate the inner record (recursively handles nested records, lists, etc.)
        let! evaluatedInnerRecord =
            if effectiveFilter.Length > 0 then
                evaluateValue innerProps "" effectiveFilter
            else
                async { return innerProps }

        // Find the matching union case and wrap the evaluated record
        let unionCase =
            FSharpType.GetUnionCases(outerPropsUCType)
            |> function
                | [||] -> failwith "Provided props input does not have required format of a union type of records"
                | [|singleCase|] -> singleCase // single case union type
                | cases ->
                    cases
                    |> Array.find (fun uc -> uc.Name = outerPropsUCType.Name)

        // Reconstruct the union case with the evaluated record
        return FSharpValue.MakeUnion(unionCase, [|evaluatedInnerRecord|])
    }
    
let resolver (p: 'Props) =
    let outerType = p.GetType()
    let unionCases = FSharpType.GetUnionCases(typeof<'Props>)
    
    let matchType =
        match unionCases with
        | [||] -> failwith "Props must be a union case"
        | [|a|] -> a // single case union case
        | uc ->
            uc |> Array.pick (
                function
                | x when x.Name = outerType.Name -> Some x
                | _ -> None)
    matchType.Name