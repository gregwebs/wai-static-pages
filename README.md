wai-static-pages provides 2 function for generating static pages from a dynamic WAI application.
`parseRoutePaths` provides a DSL for generating request paths.

`renderStaticPages` will run requests agains a WAI app and write out the responses to files in the given directory.


``` haskell
{-# Language OverloadedStrings #-}
import Shakespeare.Text (st)

-- staticPaths == ["/pages/about", "/pages/faq", "/pages"]
staticPaths = parseRoutePaths [st|
/pages
        about
        faq
        /
|]

-- app is a WAI application that responds to the given routes
main = renderStaticPages app "site/" staticPaths
```
