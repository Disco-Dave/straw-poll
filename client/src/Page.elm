module Page exposing
    ( Message(..)
    , Page(..)
    , init
    , update
    , view
    )

import Browser exposing (Document)
import Html
import Pages.CreateStrawPoll as CreateStrawPollPage
import Pages.NotFound as NotFoundPage
import Pages.StrawPoll as StrawPollPage
import Route
import Url exposing (Url)


type Page
    = CreateStrawPoll CreateStrawPollPage.Model
    | StrawPoll StrawPollPage.Model
    | NotFound


init : Url -> Page
init url =
    case Route.fromUrl url of
        Just Route.CreateStrawPoll ->
            CreateStrawPoll CreateStrawPollPage.init

        Just (Route.StrawPoll key) ->
            StrawPoll <| StrawPollPage.init key

        Nothing ->
            NotFound


type Message
    = GotCreateStrawPoll CreateStrawPollPage.Message
    | GotStrawPoll StrawPollPage.Message


update : Message -> Page -> ( Page, Cmd Message )
update message page =
    let
        mapUpdate toMessage toModel ( newPageModel, pageCommand ) =
            ( toModel newPageModel, Cmd.map toMessage pageCommand )
    in
    case ( message, page ) of
        ( GotCreateStrawPoll pageMessage, CreateStrawPoll pageModel ) ->
            CreateStrawPollPage.update pageMessage pageModel
                |> mapUpdate GotCreateStrawPoll CreateStrawPoll

        ( GotStrawPoll pageMessage, StrawPoll pageModel ) ->
            StrawPollPage.update pageMessage pageModel
                |> mapUpdate GotStrawPoll StrawPoll

        _ ->
            ( page, Cmd.none )


view : (Message -> a) -> Page -> Document a
view toMessage page =
    let
        mapDocument toPageMessage { body, title } =
            { title = title
            , body = List.map (Html.map (toMessage << toPageMessage)) body
            }
    in
    case page of
        CreateStrawPoll model ->
            CreateStrawPollPage.view model
                |> mapDocument GotCreateStrawPoll

        StrawPoll model ->
            StrawPollPage.view model
                |> mapDocument GotStrawPoll

        NotFound ->
            NotFoundPage.view
