module Replenysh.State exposing (..)

import Bootstrap.Modal as Modal
import Replenysh.Types exposing (..)
import Result.Extra exposing (isOk)
import Validate as V


initialCustomer : Customer
initialCustomer =
    { email = ""
    , firstName = ""
    , lastName = ""
    }


initialVendor : Vendor
initialVendor =
    { email = ""
    , companyName = ""
    , productName = ""
    , price = ""
    , quantity = ""
    }


init : ( Model, Cmd Msg )
init =
    ( { customer = initialCustomer
      , vendor = initialVendor
      , currentForm = CustomerForm
      , customerValidation = initialCustomerValidation
      , vendorValidation = initialVendorValidation
      }
    , Cmd.none
    )


initialCustomerValidation : CustomerValidation
initialCustomerValidation =
    { email = Nothing
    , firstName = Nothing
    , lastName = Nothing
    , valid = Nothing
    }


initialVendorValidation : VendorValidation
initialVendorValidation =
    { email = Nothing
    , companyName = Nothing
    , productName = Nothing
    , price = Nothing
    , quantity = Nothing
    , valid = Nothing
    }



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        customer =
            model.customer

        vendor =
            model.vendor
    in
    case msg of
        ChangeForm frm ->
            ( { model | currentForm = frm }, Cmd.none )

        CustomerEmail str ->
            ( { model | customer = { customer | email = str } }, Cmd.none )

        CustomerFirstName str ->
            ( { model | customer = { customer | firstName = str } }, Cmd.none )

        CustomerLastName str ->
            ( { model | customer = { customer | lastName = str } }, Cmd.none )

        VendorEmail str ->
            ( { model | vendor = { vendor | email = str } }, Cmd.none )

        VendorCompanyName str ->
            ( { model | vendor = { vendor | companyName = str } }, Cmd.none )

        VendorProductName str ->
            ( { model | vendor = { vendor | productName = str } }, Cmd.none )

        VendorPrice str ->
            ( { model | vendor = { vendor | price = str } }, Cmd.none )

        VendorQuantity str ->
            ( { model | vendor = { vendor | quantity = str } }, Cmd.none )

        CustomerSubmit ->
            let
                email =
                    V.validate emailValidator model.customer

                firstName =
                    V.validate firstNameValidator model.customer

                lastName =
                    V.validate lastNameValidator model.customer

                valid =
                    List.all isOk [ email, firstName, lastName ]

                customerValidation =
                    { email = Just email
                    , firstName = Just firstName
                    , lastName = Just lastName
                    , valid = Just valid
                    }
                customerLog = if valid then
                        Debug.log "Customer" customer
                    else
                        customer
            in
            ( { model | customerValidation = customerValidation }, Cmd.none )

        VendorSubmit ->
            let
                email =
                    V.validate emailValidator model.vendor

                companyName =
                    V.validate companyNameValidator model.vendor

                productName =
                    V.validate productNameValidator model.vendor

                price =
                    V.validate priceValidator model.vendor

                quantity =
                    V.validate quantityValidator model.vendor

                valid =
                    List.all isOk [ email, companyName, productName, price, quantity ]

                vendorValidation =
                    { email = Just email
                    , companyName = Just companyName
                    , productName = Just productName
                    , price = Just price
                    , quantity = Just quantity
                    , valid = Just valid
                    }
                vendorLog = if valid then
                        Debug.log "Vendor" vendor
                    else
                        vendor
            in
            ( { model | vendorValidation = vendorValidation }, Cmd.none )

        CloseModal ->
            ( { model
                | customerValidation = initialCustomerValidation
                , vendorValidation = initialVendorValidation
                , customer = initialCustomer
                , vendor = initialVendor
              }
            , Cmd.none
            )
