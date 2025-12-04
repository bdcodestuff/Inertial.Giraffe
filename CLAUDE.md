# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build Commands

```bash
# Build the project
dotnet build Inertial.Giraffe/Inertial.Giraffe.fsproj

# Pack for NuGet
dotnet pack Inertial.Giraffe/Inertial.Giraffe.fsproj
```

## Project Overview

Inertial.Giraffe is an F# server-side library that brings InertiaJS-style SPA behavior to the Giraffe web framework. It works in conjunction with [Inertial.Client](https://github.com/bdcodestuff/Inertial.Client) (a Sutil-based client library) to enable server-driven SPA navigation without building a traditional API.

## Architecture

### Core Flow
1. Client sends requests with `X-Inertial` headers indicating it's an Inertia request
2. Server determines whether to return full HTML page (initial load) or JSON response (subsequent navigation)
3. Props are evaluated lazily - async props can be deferred and loaded on reload via partial data requests
4. Server-sent events (SSE) enable real-time updates to connected clients

### Key Types (Library.fs)

- **`Inertia<'Props,'Shared,'SSE>`**: Main singleton service registered at startup. Holds URL map, share function, and SSE subject.
- **`InertiaResponse<'Props,'Shared,'SSE>`**: Fluent builder for component responses with methods like `SetReloadOnMount()`, `DisableRealtime()`, `UpdateShared()`, and `BroadcastSSE()`.
- **`InertiaOptions`**: Configuration for JS/CSS paths and root HTML view template.

### Type Parameters
- `'Props`: Discriminated union where each case wraps a page-specific record type
- `'Shared`: Record type for data available to all components (e.g., signed-in user, flash messages)
- `'SSE`: Server-sent event message type (typically string JSON)

### File Structure

- **FableJson.fs**: JSON serialization setup using Fable.Remoting.Json for F# type compatibility
- **Types.fs**: Header extensions (`X-Inertial-*`), boxing helpers for async/choice types, custom JSON serializer
- **UrlMap.fs**: Route definition system with `RouteData` and `RouteHandler` for type-safe routing with parameters
- **Reflection.fs**: Runtime evaluation of async props using F# quotations and reflection
- **Library.fs**: Main Inertia service, response handling, and DI extensions

### Async Props Pattern

Props can contain `AsyncData<'T>` fields (Choice types wrapping `Async<'T>`) that are:
- Serialized as placeholders on initial page load
- Evaluated server-side when client requests reload with specific field names via `X-Inertial-Partial-Data` header

### Request Headers

The library reads/writes these custom headers:
- `X-Inertial`: Indicates Inertia client request
- `X-Inertial-Version`: Asset versioning for cache busting
- `X-Inertial-Partial-Data`: Comma-separated field names for partial reload
- `X-Inertial-Partial-Component`: Component name for partial requests
- `X-Inertial-SSE`: Indicates SSE-triggered reload
- `X-Inertial-CacheStorage`/`X-Inertial-CacheRetrieval`: Client-side caching directives

### Dependencies

- Giraffe (web framework)
- Fable.Remoting.Json (F# type-aware JSON serialization)
- FSharp.Control.Reactive (SSE via reactive subjects)
- Microsoft.FSharpLu.Json (compact camelCase JSON settings)
- Inertial.Lib (shared types with client)
