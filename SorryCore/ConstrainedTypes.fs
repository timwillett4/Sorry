namespace Sorry.Core

module Int0to4 =

    type T = Int0to4 of int

    let create i =
        if i >= 0 && i <= 4 
        then Ok (Int0to4 i)
        else Error "Int value must be between 0 and 4"

    let apply f (Int0to4 i) = i |> f

    let value i = i |> apply id

module Int0to14 =

    type T = Int0to14 of int
    
    let create i =
        if i >= 0 && i <= 14 
        then Ok (Int0to14 i)
        else Error "Int value must be between 0 and 14"
    
    let apply f (Int0to14 i) = i |> f
    
    let value i = i |> apply id

module Int0to65 = 

    type T = Int0to65 of int

    let create i =
        if i >= 0 && i <= 65
        then Ok (Int0to65 i)
        else Error "Int value must be between 0 and 65"

    let apply f (Int0to65 i) = i |> f

    let value i = i |> apply id

module Int0to100 = 

    type T = Int0to100 of int

    let create i =
        if i >= 0 && i <= 100
        then Ok (Int0to100 i)
        else Error "Int value must be between 0 and 100"

    let apply f (Int0to100 i) = i |> f

    let value i = i |> apply id
    
module Name = 

    type T = Name of string

    // @TODO - add validation rules for name
    let create name =
        Ok name
    let apply f (Name s) = s |> f

    let value s = s |> apply id
