module Map
  ( initializeMap,
    getValue,
    setValue,
    MapType (Html, Plaintext, None),
    ResponseType (Get, Put, Error, Other),
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

-- | defines the type of the data being stored in the map so it can be
-- | displayed correctly, None is the default; will cause errors in parsing
data MapType = Html | Plaintext | None
  deriving (Eq, Show)

-- | defines the type of response to a request from a client, other is the
-- | default, and will spit back an error if it makes it through parsing
data ResponseType = Get | Put | Error | Other
  deriving (Eq, Show)

-- | used for the server map itself
type ServerMap = DM.Map String (MapType, String)

type Response = (ResponseType, String, MapType, ServerMap)

initializeMap :: ServerMap
initializeMap =
  DM.fromList
    [ ("Hello", (Plaintext, "there")),
      ("how", (Plaintext, "are you")),
      ("x", (Plaintext, "5")),
      ( "index.html",
        ( Html,
          "<!Doctype html> \
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

getValue :: ServerMap -> String -> Response
getValue dataMap key = case DM.lookup key dataMap of
  Just (dataType, value) -> (Get, value, dataType, dataMap)
  Nothing -> (Error, "Not found", Plaintext, dataMap)

setValue :: ServerMap -> String -> String -> MapType -> Response
setValue dataMap key value valueType = do
  let newMap = DM.insert key (valueType, value) dataMap
  (Put, key, valueType, newMap)