module Page exposing
    ( Message(..)
    , Page(..)
    , init
    , update
    , view
    )

import Browser exposing (Document)
import Html
import Pages.CreatePoll as CreatePollPage
import Pages.NotFound as NotFoundPage
import Pages.Poll as PollPage
import Route
import Url exposing (Url)


type Page
    = CreatePoll CreatePollPage.Model
    | Poll PollPage.Model
    | NotFound


init : Url -> Page
init url =
    case Route.fromUrl url of
        Just Route.CreatePoll ->
            CreatePoll CreatePollPage.init

        Just (Route.Poll key) ->
            Poll <| PollPage.init key

        Nothing ->
            NotFound


type Message
    = GotCreatePoll CreatePollPage.Message
    | GotPoll PollPage.Message


update : Message -> Page -> ( Page, Cmd Message )
update message page =
    let
        mapUpdate toMessage toModel ( newPageModel, pageCommand ) =
            ( toModel newPageModel, Cmd.map toMessage pageCommand )
    in
    case ( message, page ) of
        ( GotCreatePoll pageMessage, CreatePoll pageModel ) ->
            CreatePollPage.update pageMessage pageModel
                |> mapUpdate GotCreatePoll CreatePoll

        ( GotPoll pageMessage, Poll pageModel ) ->
            PollPage.update pageMessage pageModel
                |> mapUpdate GotPoll Poll

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
        CreatePoll model ->
            CreatePollPage.view model
                |> mapDocument GotCreatePoll

        Poll model ->
            PollPage.view model
                |> mapDocument GotPoll

        NotFound ->
            NotFoundPage.view
