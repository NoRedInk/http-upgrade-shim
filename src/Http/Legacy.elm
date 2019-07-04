module Http.Legacy exposing
    ( Request, send, Error(..)
    , getString, get
    , post
    , request
    , Header, header
    , Body, emptyBody, jsonBody, stringBody, multipartBody, Part, stringPart
    , Expect, expectString, expectJson, expectStringResponse, Response
    , toTask
    , toHttp2Error
    )

{-| Create and send HTTP requests.

This is a shim version of the `elm/http` 1.0.0 API, including its
documentation. Copyright (c) 2016-present, Evan Czaplicki.

The purpose of this module is to allow a smooth upgrade to version 2.x of
elm/http without requiring a large amount of changes across the codebase.

[url]: /packages/elm/url/latest


# Send Requests

@docs Request, send, Error


# GET

@docs getString, get


# POST

@docs post


# Custom Requests

@docs request


## Headers

@docs Header, header


## Request Bodies

@docs Body, emptyBody, jsonBody, stringBody, multipartBody, Part, stringPart


## Responses

@docs Expect, expectString, expectJson, expectStringResponse, Response


# Low-Level

@docs toTask


# Helpers for use during the elm/http 2.0.0 upgrade

@docs toHttp2Error

-}

import Dict exposing (Dict)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Task exposing (Task)



-- REQUESTS


{-| Describes an HTTP request.
-}
type alias Request a =
    { method : String
    , headers : List Http.Header
    , url : String
    , body : Http.Body
    , expect : Expect a
    , timeout : Maybe Float
    , withCredentials : Bool
    }


{-| Logic for interpreting a response body.
-}
type alias Expect a =
    { expect : Http.Expect (Result Error (Response String))
    , decode : Response String -> Result Error a
    }


{-| Send a `Request`. We could get the text of “War and Peace” like this:

    import Http

    type Msg = Click | NewBook (Result Http.Error String)

    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg model =
      case msg of
        Click ->
          ( model, getWarAndPeace )

        NewBook (Ok book) ->
          ...

        NewBook (Err _) ->
          ...

    getWarAndPeace : Cmd Msg
    getWarAndPeace =
      Http.send NewBook <|
        Http.getString "https://example.com/books/war-and-peace.md"

-}
send : (Result Error a -> msg) -> Request a -> Cmd msg
send callback { method, headers, url, body, expect, timeout, withCredentials } =
    let
        request_ =
            { method = method
            , headers = headers
            , url = url
            , body = body
            , expect = expect.expect
            , timeout = timeout
            , tracker = Nothing
            }

        handleResponse : Result Error (Response String) -> msg
        handleResponse response =
            response
                |> Result.andThen expect.decode
                |> callback
    in
    Cmd.map handleResponse <|
        if withCredentials then
            Http.riskyRequest request_

        else
            Http.request request_


{-| Convert a `Request` into a `Task`. This is only really useful if you want
to chain together a bunch of requests (or any other tasks) in a single command.
-}
toTask : Request a -> Task Error a
toTask req =
    let
        taskReq =
            { body = req.body
            , headers = req.headers
            , method = req.method
            , resolver = Http.stringResolver Ok
            , timeout = req.timeout
            , url = req.url
            }

        task =
            if req.withCredentials then
                Http.riskyTask taskReq

            else
                Http.task taskReq
    in
    task
        |> Task.andThen (handleTaskResponse req)


handleTaskResponse : Request a -> Http.Response String -> Task Error a
handleTaskResponse req response =
    case fromHttpResponse response of
        Err err ->
            Task.fail err

        Ok response_ ->
            case req.expect.decode response_ of
                Ok payload ->
                    Task.succeed payload

                Err err ->
                    Task.fail err


fromHttpResponse : Http.Response String -> Result Error (Response String)
fromHttpResponse response =
    case response of
        Http.BadUrl_ url ->
            Err (BadUrl url)

        Http.Timeout_ ->
            Err Timeout

        Http.NetworkError_ ->
            Err NetworkError

        Http.BadStatus_ meta body ->
            Err <|
                BadStatus (metadataAndBodyToResponse meta body)

        Http.GoodStatus_ meta body ->
            Ok (metadataAndBodyToResponse meta body)


metadataAndBodyToResponse : Http.Metadata -> a -> Response a
metadataAndBodyToResponse meta body =
    { url = meta.url
    , status =
        { code = meta.statusCode
        , message = meta.statusText
        }
    , headers = meta.headers
    , body = body
    }


{-| A `Request` can fail in a couple ways:

  - `BadUrl` means you did not provide a valid URL.
  - `Timeout` means it took too long to get a response.
  - `NetworkError` means the user turned off their wifi, went in a cave, etc.
  - `BadStatus` means you got a response back, but the [status code][sc]
    indicates failure.
  - `BadPayload` means you got a response back with a nice status code, but
    the body of the response was something unexpected. The `String` in this
    case is a debugging message that explains what went wrong with your JSON
    decoder or whatever.

[sc]: https://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html

-}
type Error
    = BadUrl String
    | Timeout
    | NetworkError
    | BadStatus (Response String)
    | BadPayload String (Response String)



-- GET


{-| Create a `GET` request and interpret the response body as a `String`.

    import Http.Legacy as Http

    getWarAndPeace : Http.Request String
    getWarAndPeace =
        Http.getString "<https://example.com/books/war-and-peace">

**Note:** Use [`elm/url`][url] to build URLs.
[url]: /packages/elm/url/latest

-}
getString : String -> Request String
getString url =
    { method = "GET"
    , headers = []
    , url = url
    , body = emptyBody
    , expect = expectString
    , timeout = Nothing
    , withCredentials = False
    }


{-| Expect the response body to be a `String`.
-}
expectString : Expect String
expectString =
    { expect = Http.expectStringResponse (fromHttpResponse << combine) Ok
    , decode = Ok << .body
    }


{-| Expect the response body to be JSON. You provide a `Decoder` to turn that
JSON into an Elm value. If the body cannot be parsed as JSON or if the JSON
does not match the decoder, the request will resolve to a `BadPayload` error.
-}
expectJson : Decoder a -> Expect a
expectJson decoder =
    { expect = Http.expectStringResponse (fromHttpResponse << combine) Ok
    , decode =
        \response ->
            Decode.decodeString decoder response.body
                |> Result.mapError (\err -> BadPayload (Decode.errorToString err) response)
    }


{-| Maybe you want the whole `Response`: status code, headers, body, etc. This
lets you get all of that information. From there you can use functions like
`Json.Decode.decodeString` to interpret it as JSON or whatever else you want.
-}
expectStringResponse : (Response String -> Result String a) -> Expect a
expectStringResponse fromResponse =
    { expect = Http.expectStringResponse (fromHttpResponse << combine) Ok
    , decode =
        \response ->
            fromResponse response
                |> Result.mapError (\err -> BadPayload err response)
    }



-- Http.expectStringResponse fromResponse


{-| Create a `GET` request and try to decode the response body from JSON to
some Elm value.

    import Http
    import Json.Decode exposing (list, string)

    getBooks : Http.Request (List String)
    getBooks =
        Http.get "https://example.com/books" (list string)

You can learn more about how JSON decoders work [here] in the guide.

[here]: https://guide.elm-lang.org/interop/json.html

**Note:** Use [`elm/url`][url] to build URLs.

[url]: /packages/elm/url/latest

-}
get : String -> Decode.Decoder a -> Request a
get url decoder =
    { method = "GET"
    , headers = []
    , url = url
    , body = emptyBody
    , expect = expectJson decoder
    , timeout = Nothing
    , withCredentials = False
    }



-- POST


{-| Create a `POST` request and try to decode the response body from JSON to
an Elm value. For example, if we want to send a POST without any data in the
request body, it would be like this:

    import Http
    import Json.Decode exposing (list, string)

    postBooks : Http.Request (List String)
    postBooks =
        Http.post "https://example.com/books" Http.emptyBody (list string)

See [`jsonBody`](#jsonBody) to learn how to have a more interesting request
body. And check out [this section][here] of the guide to learn more about
JSON decoders.

[here]: https://guide.elm-lang.org/interop/json.html

-}
post : String -> Body -> Decode.Decoder a -> Request a
post url body decoder =
    { method = "POST"
    , headers = []
    , url = url
    , body = body
    , expect = expectJson decoder
    , timeout = Nothing
    , withCredentials = False
    }



-- -- CUSTOM REQUESTS


{-| Create a custom request. For example, a custom PUT request would look like
this:

    put : String -> Body -> Request ()
    put url body =
        request
            { method = "PUT"
            , headers = []
            , url = url
            , body = body
            , expect = expectStringResponse (\_ -> Ok ())
            , timeout = Nothing
            , withCredentials = False
            }

The `timeout` is the number of milliseconds you are willing to wait before
giving up.

-}
request :
    { method : String
    , headers : List Header
    , url : String
    , body : Body
    , expect : Expect a
    , timeout : Maybe Float
    , withCredentials : Bool
    }
    -> Request a
request =
    identity



-- -- HEADERS


{-| An HTTP header for configuring requests. See a bunch of common headers
[here].

[here]: https://en.wikipedia.org/wiki/List_of_HTTP_header_fields

-}
type alias Header =
    Http.Header


{-| Create a `Header`.

    header "If-Modified-Since" "Sat 29 Oct 1994 19:43:31 GMT"

    header "Max-Forwards" "10"

    header "X-Requested-With" "XMLHttpRequest"

**Note:** In the future, we may split this out into an `Http.Headers` module
and provide helpers for cases that are common on the client-side. If this
sounds nice to you, open an issue [here] describing the helper you want and
why you need it.

[here]: https://github.com/elm/http/issues

-}
header : String -> String -> Header
header =
    Http.header



-- BODY


{-| Represents the body of a `Request`.
-}
type alias Body =
    Http.Body


{-| Create an empty body for your `Request`. This is useful for GET requests
and POST requests where you are not sending any data.
-}
emptyBody : Body
emptyBody =
    Http.emptyBody


{-| Put some JSON value in the body of your `Request`. This will automatically
add the `Content-Type: application/json` header.
-}
jsonBody : Encode.Value -> Body
jsonBody =
    Http.jsonBody


{-| Put some string in the body of your `Request`. Defining `jsonBody` looks
like this:

    import Json.Encode as Encode

    jsonBody : Encode.Value -> Body
    jsonBody value =
        stringBody "application/json" (Encode.encode 0 value)

Notice that the first argument is a [MIME type][mime] so we know to add
`Content-Type: application/json` to our request headers. Make sure your
MIME type matches your data. Some servers are strict about this!

[mime]: https://en.wikipedia.org/wiki/Media_type

-}
stringBody : String -> String -> Body
stringBody =
    Http.stringBody


{-| Create multi-part bodies for your `Request`, automatically adding the
`Content-Type: multipart/form-data` header.
-}
multipartBody : List Part -> Body
multipartBody =
    Http.multipartBody


{-| Contents of a multi-part body. Right now it only supports strings, but we
will support blobs and files when we get an API for them in Elm.
-}
type alias Part =
    Http.Part


{-| A named chunk of string data.

    body =
        multipartBody
            [ stringPart "user" "tom"
            , stringPart "payload" "42"
            ]

-}
stringPart : String -> String -> Part
stringPart =
    Http.stringPart



-- RESPONSES


{-| The response from a `Request`.
-}
type alias Response body =
    { url : String
    , status :
        { code : Int
        , message : String
        }
    , headers : Dict String String
    , body : body
    }



-- -- CONVERSIONS


{-| Convert an error from this module to the original `Http` module type.
This function leaks the current implementation using the elm/http 1.0.0 API, so
every place outside of this module we need to use this indicates a spot that
will require a bit of massaging when we upgrade to version 2.0.0.
-}
toHttp2Error : Error -> Http.Error
toHttp2Error error =
    case error of
        BadUrl str ->
            Http.BadUrl str

        Timeout ->
            Http.Timeout

        NetworkError ->
            Http.NetworkError

        BadStatus response ->
            Http.BadStatus response.status.code

        BadPayload str response ->
            Http.BadBody str


combine : Result a a -> a
combine result =
    case result of
        Ok x ->
            x

        Err x ->
            x
