---
title: Web/Api/WordPress/Monad
---

In this module we define the `WordPressClient` class and its core functions, the `WordPressT` monad transformer, and some auxiliary types. It is not necessary to understand all the details of this module just to _use_ the library, but it certainly helps, and the patterns here can be applied in other situations.



Contents
--------

* [Imports and Exports](#imports-and-exports)
* [WordPressT](#wordpresst): A concrete monad transformer.
* [WordPressClient](#wordpressclient): An interface for monad transformer stacks including `WordPressT`.
* [Session Configuration](#session-configuration)
* [Session Execution](#session-execution)



Imports and Exports
-------------------

This section is just bookkeeping information the compiler requires to be at the top of the module. You can safely ignore it. Do note that all names are explicitly imported or qualified, to make it easier to see where they come from.

> {-# LANGUAGE Rank2Types #-}
> module Web.Api.WordPress.Monad where
> 
> import Control.Exception
>   ( IOException )
> import Control.Monad
>   ( ap )
> import Control.Monad.Trans.Class
>   ( MonadTrans(..) )
> import Control.Monad.Trans.Identity
>   ( IdentityT(..) )
> import Data.Aeson
>   ( Value(), FromJSON )
> import Data.ByteString.Lazy
>   ( ByteString )
> import Data.Text
>   ( Text )
> import qualified Network.HTTP.Client as N
>   ( HttpException(..), HttpExceptionContent(..) )
> import System.IO
>   ( Handle )
> 
> import qualified Control.Monad.Script.Http as Http



`WordPressT`
------------

Interacting with an HTTP API is fundamentally sequential, and an interactive _session_ is fundamentally stateful. The basic functional pattern for building sequential, stateful interactions is to bundle the side effects we want to use in a _monad_.

Anytime we have a monad, a natural question is whether it can be turned into a monad _transformer_. This is a much more powerful abstraction that allows consumers of the monadic interface to specify some side effects of their own without requiring support from us, the library author. For example, this makes it simple to combine two or more otherwise independent API clients in a single program in a principled and modular way.

With this in mind, we will implement our WordPress API client as a monad transformer called `WordPressT`.

The `WordPressT` transformer is a specialization of the `HttpT` transformer, which itself is a specialization of the `ScriptT` transformer. Each of these generalization steps contributes something to the library.

`ScriptT` is a plain unrolled stack of error, reader, writer, state, and prompt transformers. Each layer in this stack gives us some principled side effects:

  * _Error_ gives us the ability to short-circuit a computation with a typed, semantic error.
  * _Writer_ gives us a write-only state, used for the interaction log.
  * _Reader_ gives us a read-only state, used for configuration.
  * _State_ gives us a mutable state, to be used only as a last resort when reader is not appropriate.
  * _Prompt_ is probably the least familiar of the bunch, even among FPers. Prompt is a kind of limited continuation monad. It gives us the ability to define certain side effects of our choosing (like network or filesystem access) as pure values which are _interpreted_ in some effect monad. Moreover, the choice of effect monad can be deferred to the _caller_ of the value, rather than the _definer_. This allows us to mock out `IO` interactions for unit testing.

`HttpT` specializes `ScriptT` with extra functions for running generic HTTP sessions.

`WordPressT` further specializes `HttpT` with functions and types specific to the WordPress API. It is defined as a wrapper around `HttpT` as follows.

> -- | A WordPress REST API interaction returning a value of type @a@,
> --   with effects taking place in the monad @eff@.
> data WordPressT eff a
>   = WPT { unWPT :: Http.HttpT WPError WPEnv WPLog WPState WPAct eff a }

Note that `HttpT` has five type parameters for the error, reader, writer, state, and prompt types, which `WordPressT` fixes. The details of these types are covered in the section on [session configuration](#session-configuration). Now `WordPressT` inherits functor, applicative, monad, and transformer instances from `HttpT`.

> instance Functor (WordPressT m) where
>   fmap f = WPT . fmap f . unWPT
> 
> instance Applicative (WordPressT m) where
>   pure = return
>   (<*>) = ap
> 
> instance Monad (WordPressT m) where
>   return = WPT . return
>   (WPT x) >>= f = WPT (x >>= (unWPT . f))
> 
> instance MonadTrans WordPressT where
>   lift = WPT . Http.liftHttpT




> -- | `WordPressT` over `IdentityT`.
> type WordPress eff a = WordPressT (IdentityT eff) a









`WordPressClient`
-----------------

At this point we could proceed by defining WordPress API bindings directly in terms of `WordPressT`. Instead we're going to introduce a little indirection now that will save users of this library a lot of effort later.

In practice, we expect that `WordPressT` will be just one layer in a larger stack of monad transformers in an application. This being the case, if all the core functions of this library are hardcoded against the `WordPressT` type, users will be forced to sprinkle their code with explicit `lift`s to inject `WordPressT` values into their custom monads. This is verbose and brittle.

What we want instead is to build the library against an interface representing monad stacks into which `WordPressT` values can be lifted.

> -- | The class of monad transformers which `WordPressT` values can
> --   be lifted into.
> class WordPressClient t where
>   liftWPT :: WordPressT eff a -> t eff a

Now `WordPressT` is trivially an instance of this class, and users can define their own instances as well.

> instance WordPressClient WordPressT where
>   liftWPT = id

The `HttpT` transformer provides a simple interface for manipulating the state, working with errors, and performing HTTP requests. Wrapped versions of these utilities will form the core of the `WordPressClient` interface.

> wpHPutStrLn
>   :: (WordPressClient t)
>   => Handle
>   -> String
>   -> t eff ()
> wpHPutStrLn h =
>   liftWPT . WPT . Http.hPutStrLn h


> wpCatchError
>   :: (WordPressClient t)
>   => WordPressT eff a -- ^ @WordPressT@ interaction that may fail with a @WPError@
>   -> (WPError -> WordPressT eff a) -- ^ Error handler
>   -> t eff a
> wpCatchError x h = liftWPT $ WPT $ Http.catchError (unWPT x) (unWPT . h)


> wpThrowJsonError
>   :: (WordPressClient t)
>   => Http.JsonError
>   -> t eff a
> wpThrowJsonError =
>   liftWPT . WPT . Http.throwJsonError

> -- | Capures `HttpException`s.
> wpHttpGet
>   :: (WordPressClient t)
>   => Http.Url
>   -> t m Http.HttpResponse
> wpHttpGet =
>   liftWPT . WPT . Http.httpGet

> -- | Does not write request or response info to the log, except
> --   to note that a request occurred. Capures `HttpException`s.
> wpHttpSilentGet
>   :: (WordPressClient t)
>   => Http.Url
>   -> t eff Http.HttpResponse
> wpHttpSilentGet =
>   liftWPT . WPT . Http.httpSilentGet


> -- | Get a computed value from the environment
> wpFromEnv
>   :: (WordPressClient t)
>   => (Http.R WPError WPLog WPEnv -> a)
>   -> t eff a
> wpFromEnv =
>   liftWPT . WPT . Http.reader

> wpThrowHttpException
>   :: (WordPressClient t)
>   => N.HttpException
>   -> t eff a
> wpThrowHttpException =
>   liftWPT . WPT . Http.throwHttpException

> wpThrowIOException
>   :: (WordPressClient t)
>   => IOException
>   -> t m a
> wpThrowIOException =
>   liftWPT . WPT . Http.throwIOException

> -- | Get a computed value from the state
> wpFromState
>   :: (WordPressClient t)
>   => (Http.S WPState -> a)
>   -> t m a
> wpFromState =
>   liftWPT . WPT . Http.gets

> -- | Mutate the state
> wpModifyState
>   :: (WordPressClient t)
>   => (Http.S WPState -> Http.S WPState)
>   -> t m ()
> wpModifyState =
>   liftWPT . WPT . Http.modify

> -- | Write a comment to the log.
> wpComment
>   :: (WordPressClient t)
>   => String
>   -> t m ()
> wpComment =
>   liftWPT . WPT . Http.comment


> wpWait
>   :: (WordPressClient t)
>   => Int -- ^ In milliseconds
>   -> t m ()
> wpWait =
>   liftWPT . WPT . Http.wait

> wpThrowError
>   :: (WordPressClient t)
>   => WPError
>   -> t m a
> wpThrowError =
>   liftWPT . WPT . Http.throwError

> -- | Rethrows other error types
> wpCatchJsonError
>   :: (WordPressClient t)
>   => WordPressT m a
>   -> (Http.JsonError -> WordPressT m a)
>   -> t m a
> wpCatchJsonError x h =
>   liftWPT . WPT $ Http.catchJsonError (unWPT x) (unWPT . h)

> -- | Rethrows other error types
> wpCatchHttpException
>   :: (WordPressClient t)
>   => WordPressT eff a
>   -> (N.HttpException -> WordPressT eff a)
>   -> t eff a
> wpCatchHttpException x h =
>   liftWPT . WPT $ Http.catchHttpException (unWPT x) (unWPT . h)

> -- | Rethrows other error types
> wpCatchIOException
>   :: (WordPressClient t)
>   => WordPressT eff a
>   -> (IOException -> WordPressT eff a)
>   -> t eff a
> wpCatchIOException x h =
>   liftWPT . WPT $ Http.catchIOException (unWPT x) (unWPT . h)

> -- | May throw a `JsonError`.
> wpParseJson
>   :: (WordPressClient t)
>   => ByteString
>   -> t eff Value
> wpParseJson =
>   liftWPT . WPT . Http.parseJson

> -- | May throw a `JsonError`.
> wpLookupKeyJson
>   :: (WordPressClient t)
>   => Text
>   -> Value
>   -> t eff Value
> wpLookupKeyJson key =
>   liftWPT . WPT . Http.lookupKeyJson key

> -- | May throw a `JsonError`.
> wpConstructFromJson
>   :: (WordPressClient t, FromJSON a)
>   => Value
>   -> t eff a
> wpConstructFromJson =
>   liftWPT . WPT . Http.constructFromJson




Session Configuration
---------------------

> -- | Errors specific to WordPress API interactions.
> data WPError = WPError

> -- | A read-only environment for WordPress API interactions.
> data WPEnv = WPEnv

> data WPLog = WPLog

> data WPState = WPState

> data WPAct a = WPAct a


> data WordPressConfig eff = WordPressConfig
>   { _initState :: Http.S WPState
>   , _env :: Http.R WPError WPLog WPEnv
>   , _eval :: forall a. Http.P WPAct a -> eff a
>   }



Session Execution
-----------------

> -- | Execute a `WordPressT` session.
> execWordPressT
>   :: (Monad eff, Monad (m eff))
>   => WordPressConfig eff
>   -> (forall u. eff u -> m eff u) -- ^ Lift effects to the inner monad
>   -> WordPressT (m eff) a
>   -> m eff (Either (Http.E WPError) a, Http.S WPState, Http.W WPError WPLog)
> execWordPressT config lift = Http.execHttpTM
>   (_initState config) (_env config) (_eval config) lift . unWPT

-- | For testing with QuickCheck.
checkWebDriverT
  :: (Monad eff, Monad (m eff))
  => WebDriverConfig eff
  -> (forall u. eff u -> m eff u) -- ^ Lift effects to the inner monad
  -> (m eff (Either (Http.E WDError) t, Http.S WDState, Http.W WDError WDLog) -> IO q) -- ^ Condense to `IO`
  -> (q -> Bool) -- ^ Result check
  -> WebDriverT (m eff) t
  -> Property
checkWebDriverT config lift cond check =
  Http.checkHttpTM
    (_initialState config)
    (_environment config)
    (_evaluator config)
    lift cond check . unWDT

