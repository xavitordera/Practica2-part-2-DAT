
{-# LANGUAGE OverloadedStrings #-}

module Handler
    -- Exporta les seguents declaracions d'aquest modul
    ( Handler, dispatchHandler, HandlerResponse(..)
    , getMethod, getSession, setSession, getPostQuery
    -- Export tambe les funcions 'lookupParam' i 'lookupParams' de 'WaiUtils'
    , U.lookupParams, U.lookupParam
    )
where
import           Network.Wai
import           Network.HTTP.Types
import           Web.Cookie

import qualified WaiUtils as U
import           TextUtils
import           Html

import           Data.Text (Text)
import           Control.Monad
import           Control.Applicative

-- ****************************************************************

-- Tipus correponent al monad 'Handler'.
-- El context d'un Handler compren:
--      L'argument Request que permet obtenir informacio sobre la peticio.
--      L'estat del Handler (argument i resultat de les operacions).
newtype Handler a = HandlerC (Request -> HandlerState -> IO (a, HandlerState))

runHandler :: Handler a -> Request -> HandlerState -> IO (a, HandlerState)
runHandler (HandlerC h) = h

-- L'estat del Handler compren:
--      'Cache' dels parametres de la peticio.
--      L'estat de la sessio que s'obte de les corresponents 'cookies'.
--        Aquest estat de sessio es una llista de parelles nom-valor.
data HandlerState = HandlerStateC (Maybe Query) [(Text, Text)]

-- Funcions auxiliars per obtenir informació de l'estat del handler
hsQuery :: HandlerState -> Maybe Query
hsQuery (HandlerStateC q _) = q
hsSession :: HandlerState -> [(Text, Text)]
hsSession (HandlerStateC _ s) = s

-- Funcions auxiliars per modificar l'estat del handler
hsSetQuery :: Maybe Query -> HandlerState -> HandlerState
hsSetQuery q (HandlerStateC _ s) = HandlerStateC q s
hsSetSession :: [(Text, Text)] -> HandlerState -> HandlerState
hsSetSession s (HandlerStateC q _) = HandlerStateC q s

instance Functor Handler where
    -- tipus dels metodes en aquesta instancia:
    --          fmap :: (a -> b) -> Handler a -> Handler b
    fmap f (HandlerC h) = HandlerC $ \ req st -> do
        ( x, st1 ) <- h req st
        pure ( f x, st1 )

instance Applicative Handler where
    -- tipus dels metodes en aquesta instancia:
    --          pure  :: a -> Handler a
    --          (<*>) :: Handler (a -> b) -> Handler a -> Handler b
     --FIXME
    pure x = HandlerC $ \ req st -> do
	pure (x, st) 
    -- END
    HandlerC hf <*> HandlerC hx = HandlerC $ \ req st -> do
        ( f, st1 ) <- hf req st
        ( x, st2 ) <- hx req st1
        pure ( f x, st2 )

instance Monad Handler where
    -- tipus dels metodes en aquesta instancia:
    --          (>>=) :: Handler a -> (a -> Handler b) -> Handler b
    return = pure
    --FIXME
    HandlerC hx >>= f = HandlerC $ \ req st -> do
       (resp, state) <- hx req st
       runHandler (f resp) req state
    --END
       -- error "Handler.(>>=): A completar per l'estudiant"

-- ****************************************************************

-- Tipus que ha de tenir el resultat del handler que se li passa a 'dispatchHandler'.
data HandlerResponse =
        HRHtml Html             -- Resposta normal. Parametre: Contingut HTML.
      | HRRedirect Text         -- Redireccio. Parametre: URL.
      | HRError Status Text     -- Resposta anormal. Parametres: Codi d'estat HTTP i missatge.

-- 'dispatchHandler' converteix (adapta) un 'Handler' a una aplicacio WAI,
-- realitzant els passos seguents:
--      Obte l'estat inicial (st0) del handler amb una sessio inicial a partir
--        de les cookies rebudes en la peticio WAI.
--      Executa el handler passant-li la peticio i l'estat inicial.
--      Amb l'execucio del handler s'obte el parell format
--        pel resultat del handler i l'estat final (st1).
--      Construeix la corresponent resposta WAI i l'envia.
--        La resposta WAI depen del nou estat de sessio en st1.
dispatchHandler :: Handler HandlerResponse -> Application
dispatchHandler handler req respond = do
    let st0 = HandlerStateC Nothing (U.requestSession req)
    ( resp, st1 ) <- runHandler handler req st0
    let newsession = hsSession st1
        wairesp = case resp of
            HRHtml html -> U.htmlResponse (runHtmlToText html) newsession
            HRRedirect url -> U.redirectResponse url newsession
            HRError status msg -> U.errorResponse status msg
    respond wairesp

-- ****************************************************************

-- Obte el metode HTTP de la peticio
getMethod :: Handler Method
getMethod = HandlerC $ \req st -> do
	pure (requestMethod req, st)

-- Obte el valor de l'atribut de sessio indicat amb el nom.
-- Retorna Nothing si l'atribut indicat no existeix.
getSession :: Read a => Text -> Handler (Maybe a)
-- FIXME
getSession name =  do
    rawParam <- getSession_ (name)
    --let text = readt name in
    case rawParam of 
        Just text ->
            pure (readt text)
        Nothing ->
            pure (Nothing)
-- END
    -- NOTA: Useu la funcio 'getSession_' i 'readt' (que parseja un text).
    --error "Handler.getSession: A completar per l'estudiant"

-- Obte el valor de l'atribut de sessio indicat amb el nom.
-- Retorna Nothing si l'atribut indicat no existeix o no te la sintaxis adequada.
getSession_ :: Text -> Handler (Maybe Text)
getSession_ name = HandlerC $ \req st -> do
    -- pure (requestSession req, st)
    -- FIXME
    let param = U.lookupParam name req in
        pure (param, st)
    --error "Handler.getSession: A completar per l'estudiant"

-- Fixa l'atribut de sessio indicat amb el nom i valor indicats.
setSession :: Show a => Text -> a -> Handler ()
setSession name value =
    -- NOTA: Useu les funcions 'setSession_' i 'showt' (que converteix a text).
    -- FIXME
    --let param = (showt value) in
    setSession_ (name, showt value)

    --error "Handler.setSession: A completar per l'estudiant"


-- Fixa l'atribut de sessio indicat amb el nom i valor indicats.
setSession_ :: Text -> Text -> Handler ()
setSession_ name value = HandlerC $ \ req st -> do
    let newsession = (name, value) : filter ((name /=) . fst) (hsSession st)
    -- FIXME
    pure ((), (hsSetSession newsession) st)
    --error "Handler.setSession: A completar per l'estudiant"

-- Obte els parametres del contingut de la peticio.
getPostQuery :: Handler Query
getPostQuery = HandlerC $ \ req st -> do
    -- Si previament ja s'havien obtingut els parametres (i guardats en l'estat del handler)
    -- aleshores es retornen aquests, evitant tornar a llegir el contingut de la peticio
    -- (veieu el comentari de 'requestGetPostQuery' en el modul 'WaiUtils').
    case hsQuery st of
        Just query ->
            pure ( query, st )
        Nothing -> do
            query <- U.requestGetPostQuery req
            pure ( query, hsSetQuery (Just query) st )

