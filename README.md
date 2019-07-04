# NoRedInk/http-upgrade-shim

## Helping you gradually upgrade your application to the elm/http 2.0 API!

The APIs between elm/http 1.0 and 2.0 changed a lot. This shim consumes elm/http 2.0 and exposes an API that's mostly compatible with elm/http 1.0

## Upgrading elm/http from 1.0 to 2.0
1. Use [elm-json](https://github.com/zwilias/elm-json) to find out if any packages are preventing you from upgrading.
```
$ elm-json install elm/http@2.0.0

-- NO VALID PACKAGE VERSIONS FOUND ---------------------------------------------

Because NoRedInk/elm-string-conversions 1.0.0 depends on elm/http
1.0.0 <= v < 2.0.0 and this project depends on elm/http 2.0.0,
NoRedInk/elm-string-conversions 1.0.0 is incompatible with this project.

And because this project depends on NoRedInk/elm-string-conversions 1.0.0,
no valid set of package versions could be found.
```
2. Copy the source code of any of those packages directly into your codebase, and, remove the packages from your `elm.json`
3. Upgrade to elm/http 2.0 `elm install elm/http`
4. In your code base as well as the packages copied over, replace `import Http` with `import Http.Legacy as Http`
5. Done!

## Limitations
- This doesn't support `Http.Progress`
- This hasn't been tested

---
[![NoRedInk](https://cloud.githubusercontent.com/assets/1094080/9069346/99522418-3a9d-11e5-8175-1c2bfd7a2ffe.png)](http://noredink.com/about/team)
