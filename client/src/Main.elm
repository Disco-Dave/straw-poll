module Main exposing (main)

import Browser exposing (Document, UrlRequest(..), application)
import Browser.Navigation as Navigation
import Page exposing (Page)
import Route exposing (Route(..))
import Url exposing (Url)


type alias Model =
    { navigationKey : Navigation.Key
    , page : Page
    }


type Message
    = UrlRequested UrlRequest
    | UrlChanged Url
    | GotPageMessage Page.Message


init : Url -> Navigation.Key -> ( Model, Cmd Message )
init url navigationKey =
    ( { navigationKey = navigationKey
      , page = Page.init url
      }
    , Cmd.none
    )


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        UrlChanged url ->
            ( { model | page = Page.init url }
            , Cmd.none
            )

        UrlRequested urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Navigation.pushUrl model.navigationKey (Url.toString url)
                    )

                External href ->
                    ( model, Navigation.load href )

        GotPageMessage pageMessage ->
            let
                ( updatePageModel, pageCommand ) =
                    Page.update pageMessage model.page
            in
            ( { model | page = updatePageModel }
            , Cmd.map GotPageMessage pageCommand
            )


view : Model -> Document Message
view { page } =
    Page.view GotPageMessage page


main : Program () Model Message
main =
    application
        { init = always init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }
