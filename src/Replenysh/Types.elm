module Replenysh.Types exposing (..)

import Bootstrap.Modal as Modal
import Regex
import Validate as V


letters : Regex.Regex
letters =
    Maybe.withDefault Regex.never <|
        Regex.fromString "^[A-Za-z]*$"


lettersNumbers : Regex.Regex
lettersNumbers =
    Maybe.withDefault Regex.never <|
        Regex.fromString "^[A-Za-z0-9]*$"


type alias Model =
    { customer : Customer
    , vendor : Vendor
    , currentForm : CurrentForm
    , customerValidation : CustomerValidation
    , vendorValidation : VendorValidation
    }


type CurrentForm
    = CustomerForm
    | VendorForm


type alias Customer =
    { email : String
    , firstName : String
    , lastName : String
    }


type alias CustomerValidation =
    { email : Maybe (Result (List String) (V.Valid Customer))
    , firstName : Maybe (Result (List String) (V.Valid Customer))
    , lastName : Maybe (Result (List String) (V.Valid Customer))
    , valid : Maybe Bool
    }


type alias VendorValidation =
    { email : Maybe (Result (List String) (V.Valid Vendor))
    , companyName : Maybe (Result (List String) (V.Valid Vendor))
    , productName : Maybe (Result (List String) (V.Valid Vendor))
    , price : Maybe (Result (List String) (V.Valid Vendor))
    , quantity : Maybe (Result (List String) (V.Valid Vendor))
    , valid : Maybe Bool
    }


type alias Vendor =
    { email : String
    , companyName : String
    , productName : String
    , price : String
    , quantity : String
    }


type Msg
    = CustomerEmail String
    | CustomerFirstName String
    | CustomerLastName String
    | CustomerSubmit
    | VendorEmail String
    | VendorCompanyName String
    | VendorProductName String
    | VendorPrice String
    | VendorQuantity String
    | VendorSubmit
    | ChangeForm CurrentForm
    | CloseModal


emailValidator : V.Validator String { a | email : String }
emailValidator =
    V.firstError
        [ V.ifBlank .email " is required"
        , V.ifInvalidEmail .email (\_ -> " is invalid")
        ]


companyNameValidator : V.Validator String Vendor
companyNameValidator =
    V.firstError
        [ V.ifBlank .companyName " is required"
        ]


firstNameValidator : V.Validator String Customer
firstNameValidator =
    V.firstError
        [ V.ifFalse
            (\model -> Regex.contains letters model.firstName)
            " can only contain alphabets"
        ]


lastNameValidator : V.Validator String Customer
lastNameValidator =
    V.firstError
        [ V.ifFalse
            (\model -> Regex.contains letters model.lastName)
            " can only contain alphabets"
        ]


productNameValidator : V.Validator String Vendor
productNameValidator =
    V.firstError
        [ V.ifBlank .productName " is required"
        , V.ifFalse
            (\model -> Regex.contains lettersNumbers model.productName)
            " can only contain alphabets and letters"
        ]


quantityValidator : V.Validator String Vendor
quantityValidator =
    V.firstError
        [ V.ifBlank .quantity " is required"
        , V.ifNotInt .quantity (\_ -> " is not a number")
        , V.ifFalse
            (\model ->
                model.quantity
                    |> String.toInt
                    |> Maybe.map (\q -> q > 0)
                    |> Maybe.withDefault False
            )
            " has to be a positive number"
        ]


priceValidator : V.Validator String Vendor
priceValidator =
    V.firstError
        [ V.ifBlank .price " is required"
        , V.ifTrue (\model -> String.toFloat model.price == Nothing) " is not a number"
        ]
