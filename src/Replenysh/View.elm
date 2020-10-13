module Replenysh.View exposing (view)

import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Modal as Modal
import Bootstrap.Text as Text
import Css
    exposing
        ( backgroundColor
        , color
        , hex
        , hover
        , marginTop
        , px
        )
import Html as H
import Html.Styled exposing (Html, div, h1, hr, text, toUnstyled)
import Html.Styled.Attributes exposing (css, for, value)
import Html.Styled.Events exposing (onInput)
import Replenysh.Types exposing (..)
import Validate as V


displayError : Maybe (Result (List String) value) -> Html msg
displayError maybeErrors =
    maybeErrors
        |> Maybe.map
            (\e ->
                case e of
                    Ok _ ->
                        text ""

                    Err ary ->
                        (ary |> String.join "") |> text
            )
        |> Maybe.withDefault (text "")


view : Model -> H.Html Msg
view model =
    -- let
    -- vendorquantityerrors =
    --     Debug.log "errors" <| V.validate quantityValidator model.vendor
    -- firstNameErrors =
    --     Debug.log "firstname errors" <| V.validate firstNameValidator model.customer
    -- lastNameErrors =
    --     Debug.log "firstname errors" <| V.validate lastNameValidator model.customer
    -- productNameErrors =
    --     Debug.log "product name errors" <| V.validate productNameValidator model.vendor
    -- emailErrors =
    --     Debug.log "product name errors" <| V.validate emailValidator model.customer
    -- in
    Grid.container []
        [ Grid.row [ Row.aroundMd, Row.attrs [ css [ marginTop (px 50) ] ] ]
            [ Grid.col [ Col.md6 ]
                [ Grid.row [ Row.leftXs ]
                    [ Grid.col [ Col.md6, Col.textAlign Text.alignXsLeft ]
                        [ h1 [] [ text "SignUp" ] ]
                    , Grid.col [ Col.md3 ]
                        [ customButton2 "Customer" (model.currentForm == CustomerForm) (ChangeForm CustomerForm) ]
                    , Grid.col [ Col.md3, Col.textAlign Text.alignXsRight ]
                        [ customButton2 "Vendor" (model.currentForm == VendorForm) (ChangeForm VendorForm) ]
                    ]
                , Grid.row []
                    [ Grid.col [] [ hr [ css [ backgroundColor (hex "000000") ] ] [] ] ]
                ]
            ]
        , Grid.row [ Row.aroundMd ]
            [ Grid.col [ Col.md6 ]
                [ --   customer form  ---
                  if model.currentForm == CustomerForm then
                    Form.form []
                        [ formField "Email"
                            "customerEmail"
                            model.customer.email
                            CustomerEmail
                            model.customerValidation.email
                        , formField "First Name (Optional)"
                            "customerFirstName"
                            model.customer.firstName
                            CustomerFirstName
                            model.customerValidation.firstName
                        , formField "Last Name (Optional)"
                            "customerLastName"
                            model.customer.lastName
                            CustomerLastName
                            model.customerValidation.lastName
                        , div [ css [ Css.textAlign Css.right ] ]
                            [ customButton2 "Submit" True CustomerSubmit ]
                        , showModal "Customer Created!"
                            (model.customerValidation.valid |> Maybe.withDefault False)
                            CloseModal
                        ]

                  else
                    --   vendor form  ---
                    Form.form []
                        [ formField "Email"
                            "vendorEmail"
                            model.vendor.email
                            VendorEmail
                            model.vendorValidation.email
                        , formField "Company Name"
                            "companyName"
                            model.vendor.companyName
                            VendorCompanyName
                            model.vendorValidation.companyName
                        , formField "Product Name"
                            "productName"
                            model.vendor.productName
                            VendorProductName
                            model.vendorValidation.productName
                        , formField "Product Price"
                            "productPrice"
                            model.vendor.price
                            VendorPrice
                            model.vendorValidation.price
                        , formField "Product Quantity"
                            "productQuantity"
                            model.vendor.quantity
                            VendorQuantity
                            model.vendorValidation.quantity
                        , div [ css [ Css.textAlign Css.right ] ]
                            [ customButton2 "Submit" True VendorSubmit
                            ]
                        , showModal "Vendor Created!"
                            (model.vendorValidation.valid |> Maybe.withDefault False)
                            CloseModal
                        ]
                ]
            ]
        ]
        |> toUnstyled


formField : String -> String -> String -> (String -> msg) -> Maybe (Result (List String) value) -> Html msg
formField label forLabel val msg errors =
    Form.group []
        [ Form.label [ for forLabel ]
            [ text label
            , Form.invalidFeedback
                [ css [ Css.display Css.inline ]
                ]
                [ text " ", displayError errors ]
            ]
        , Input.email
            [ Input.id forLabel
            , Input.attrs [ value val, onInput msg ]
            ]
        ]


showModal : String -> Bool -> msg -> Html msg
showModal message valid closeMsg =
    Modal.config closeMsg
        |> Modal.small
        |> Modal.body []
            [ div
                [ css
                    [ Css.textAlign Css.center
                    , Css.padding (px 30)
                    , Css.fontSize (px 24)
                    ]
                ]
                [ text message
                ]
            , div
                [ css
                    [ Css.textAlign Css.center
                    , Css.padding (px 10)
                    , Css.fontSize (px 20)
                    ]
                ]
                [ customButton "Done"
                    closeMsg
                    [ backgroundColor (hex "000000")
                    , color (hex "FFF")
                    , hover [ backgroundColor (hex "000000"), color (hex "FFF") ]
                    , Css.paddingLeft (px 20)
                    , Css.paddingRight (px 20)
                    ]
                ]
            ]
        |> Modal.view
            (if valid then
                Modal.shown

             else
                Modal.hidden
            )


customButton2 : String -> Bool -> msg -> Html msg
customButton2 label active msg =
    (if active then
        [ backgroundColor (hex "000000")
        , color (hex "FFF")
        , hover [ backgroundColor (hex "000000"), color (hex "FFF") ]
        ]

     else
        [ backgroundColor (hex "ffffff")
        , color (hex "000")
        , hover [ backgroundColor (hex "000000"), color (hex "FFF") ]
        ]
    )
        |> customButton label msg


customButton : String -> msg -> List Css.Style -> Html msg
customButton label msg styles =
    Button.button
        [ Button.outlineSecondary
        , Button.attrs
            [ css styles ]
        , Button.onClick msg
        ]
        [ text label ]
