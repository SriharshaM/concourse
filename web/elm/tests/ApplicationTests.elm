module ApplicationTests exposing (all)

import Application.Application as Application
import Browser
import Common exposing (queryView)
import Expect
import Message.Effects as Effects
import Message.Subscription as Subscription exposing (Delivery(..))
import Message.TopLevelMessage as Msgs
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (style)
import Url


all : Test
all =
    describe "top-level application"
        [ test "bold and antialiasing on dashboard" <|
            \_ ->
                Common.init "/"
                    |> queryView
                    |> Query.has
                        [ style "-webkit-font-smoothing" "antialiased"
                        , style "font-weight" "700"
                        ]
        , test "bold and antialiasing on resource page" <|
            \_ ->
                Common.init "/teams/t/pipelines/p/resources/r"
                    |> queryView
                    |> Query.has
                        [ style "-webkit-font-smoothing" "antialiased"
                        , style "font-weight" "700"
                        ]
        , test "bold and antialiasing everywhere else" <|
            \_ ->
                Common.init "/teams/team/pipelines/pipeline"
                    |> queryView
                    |> Query.has
                        [ style "-webkit-font-smoothing" "antialiased"
                        , style "font-weight" "700"
                        ]
        , test "should subscribe to clicks from the not-automatically-linked boxes in the pipeline, and the token return" <|
            \_ ->
                Common.init "/teams/t/pipelines/p/"
                    |> Application.subscriptions
                    |> Expect.all
                        [ Common.contains Subscription.OnNonHrefLinkClicked
                        , Common.contains Subscription.OnTokenReceived
                        ]
        , test "should subscribe to the favorited pipelines response" <|
            \_ ->
                Common.init "/teams/t/pipelines/p/"
                    |> Application.subscriptions
                    |> Common.contains Subscription.OnFavoritedPipelinesReceived
        , test "should load favorited pipelines on init" <|
            \_ ->
                Application.init
                    { turbulenceImgSrc = ""
                    , notFoundImgSrc = "notfound.svg"
                    , csrfToken = "csrf_token"
                    , authToken = ""
                    , pipelineRunningKeyframes = "pipeline-running"
                    }
                    { protocol = Url.Http
                    , host = ""
                    , port_ = Nothing
                    , path = "/teams/t/pipelines/p/"
                    , query = Nothing
                    , fragment = Nothing
                    }
                    |> Tuple.second
                    |> Common.contains Effects.LoadFavoritedPipelines
        , test "clicking a not-automatically-linked box in the pipeline redirects" <|
            \_ ->
                Common.init "/teams/t/pipelines/p/"
                    |> Application.update
                        (Msgs.DeliveryReceived <|
                            NonHrefLinkClicked "/foo/bar"
                        )
                    |> Tuple.second
                    |> Expect.equal [ Effects.LoadExternal "/foo/bar" ]
        , test "received token is passed to all subsquent requests" <|
            \_ ->
                let
                    pipelineIdentifier =
                        { pipelineName = "p", teamName = "t" }
                in
                Common.init "/"
                    |> Application.update
                        (Msgs.DeliveryReceived <|
                            TokenReceived <|
                                Ok "real-token"
                        )
                    |> Tuple.first
                    |> .session
                    |> .csrfToken
                    |> Expect.equal "real-token"
        ]
