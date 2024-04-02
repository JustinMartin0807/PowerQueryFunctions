(
    optional stringToCheck as text
) as text =>
    let
        _checks =
            try
                if 
                    stringToCheck is null
                then
                    "0"
                else if
                    List.Contains (
                        {
                            "false",
                            "no",
                            "0"
                        },
                        Text.Lower ( stringToCheck )
                    ) or
                        fxIsNullOrEmpty ( stringToCheck )
                then
                    "0"
                else
                    "1"
            otherwise
                "0"
    in
        _checks
