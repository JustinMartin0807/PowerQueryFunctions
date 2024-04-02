let 
    fxFunction =
        (
            optional valueToCheck as any
        ) as logical =>
            let
                _checks =
                    if
                        valueToCheck is null 
                    then
                        true
                    else
                        try
                            List.Contains (
                                {
                                    "blank",
                                    "empty",
                                    "unknown",
                                    "null",
                                    ""
                                },
                                Text.Lower ( valueToCheck )
                            )
                        otherwise 
                            false
            in               
		_checks
in
    fxFunction
