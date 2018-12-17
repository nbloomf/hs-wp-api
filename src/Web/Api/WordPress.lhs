---
title: Web/Api/WordPress
---

This library implements bindings to the WordPress REST API in the Haskell programming language.

Our library can be divided into three essential components.

* At the core we have a WordPress _monad transformer_ and its associated types. This is a barebones type and interface that handles the configuration and execution of HTTP sessions against the WordPress API. This transformer is built on top of the Script monad pattern.
* Next we have a family of _types_ representing the objects accepted and returned by the API, and utility functions for translating between these types and raw JSON. This will allow us to take advantage of type inference when building API interactions; a _post_ is not just a JSON object, it is a value of an algebraic _type_, and its properties are _typed_.
* Then we have an _endpoint_ layer. This is the interface we'll use to interact with the API. It is implemented in terms of the WordPress monad transformer and uses typed objects for safety and readability.

Users of the library can build interactions in terms of the endpoint functions and the monad interface. Notably, these interactions are _pure values_.

> module Web.Api.WordPress (
>     module Web.Api.WordPress.Monad
>   , module Web.Api.WordPress.Types
>   , module Web.Api.WordPress.Endpoints
> ) where
> 
> import Web.Api.WordPress.Monad
> import Web.Api.WordPress.Types
> import Web.Api.WordPress.Endpoints
