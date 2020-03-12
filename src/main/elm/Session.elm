module Session exposing (Session, baseUrl, init, navKey)

import Browser.Navigation as Nav
import Url exposing (Url)



-- TYPES


type Session
    = Session Url Nav.Key



-- INFO


navKey : Session -> Nav.Key
navKey session =
    case session of
        Session _ key ->
            key


baseUrl : Session -> Url
baseUrl session =
    case session of
        Session url _ ->
            url



-- CHANGES


init : Url -> Nav.Key -> Session
init url key =
    Session url key
