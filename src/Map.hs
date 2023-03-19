module Map
  ( initializeMap,
    getValue,
    setValue,
    MapType (HTML, PLAINTEXT, NONE),
    ResponseType (GET, PUT, ERROR, OTHER),
    ServerMap,
    Response,
  )
where

{-
  Look into Debug.trace for debugging
  Look into Control.Monad.Trans.State

  also look into IORef if I have trouble with IO later
-}

import Data.Map as DM

data MapType = HTML | PLAINTEXT | NONE
  deriving (Eq, Show)

data ResponseType = GET | PUT | ERROR | OTHER
  deriving (Eq, Show)

type ServerMap = DM.Map String (MapType, String)

type Response = (ResponseType, String, MapType, ServerMap)

initializeMap :: ServerMap
initializeMap =
  DM.fromList
    [ ("Hello", (PLAINTEXT, "there")),
      ("how", (PLAINTEXT, "are you")),
      ("x", (PLAINTEXT, "5")),
      ( "index.html",
        ( HTML,
          "<!DOCTYPE html> \
          \ <header>index</header> \
          \ <p> Quis nisi anim magna cillum qui velit adipisicing est est. \
          \ Exercitation consequat eu qui sint deserunt veniam eiusmod enim \
          \ nisi amet dolore nulla occaecat. Non consectetur veniam commodo \
          \ mollit minim nostrud aute. Nisi duis aliquip adipisicing ex deserunt \
          \ commodo sint ex duis sit anim. \
          \ </p> <br/>\
          \ <ul> \
          \ Wow, a list! \
          \ <li> lorem </li>\
          \ <li> ipsum </li>\
          \ <li> duis </li>\
          \ <li> idk some more latin </li>\
          \ </ul> \
          \ <br />\
          \ <p> \
          \ Proident esse do ullamco adipisicing incididunt. Non nulla elit sit ex \
          \ incididunt adipisicing do aute ut commodo ipsum elit consectetur tempor. \
          \ Dolor ullamco laborum mollit elit. Deserunt ex non amet duis fugiat duis \
          \ eu ea mollit veniam. Nisi ex consectetur esse quis aliquip ex ullamco sint \
          \ Lorem. Cillum nulla labore aliquip non cillum proident consequat voluptate \
          \ dolor cillum. \
          \ </p> <br/>\
          \ <p> \
          \ Duis amet consectetur proident excepteur fugiat cupidatat exercitation qui \
          \ qui minim. Duis eu sint non consequat eu deserunt et minim nostrud. Laboris \
          \ do tempor dolor culpa mollit dolore nostrud in aute. Ullamco eu nostrud sint \
          \ aute incididunt pariatur sunt ad reprehenderit labore aute ut. Labore fugiat \
          \ elit consequat commodo tempor excepteur officia. Consectetur laboris ex aliqua \
          \ tempor ut occaecat aliqua id fugiat magna. \
          \ </p> \
          \ </html>"
        )
      )
    ]

{- Got the minimum viable HTTP response from here:
 - https://stackoverflow.com/questions/33784127/minimal-http-server-reply
 - -}
getValue :: ServerMap -> String -> Response
getValue dataMap key = case DM.lookup key dataMap of
  Just (dataType, value) -> (GET, value, dataType, dataMap)
  Nothing -> (ERROR, "Not found", PLAINTEXT, dataMap)

setValue :: ServerMap -> String -> String -> MapType -> Response
setValue dataMap key value valueType = do
  let newMap = DM.insert key (valueType, value) dataMap
  (PUT, key, valueType, newMap)