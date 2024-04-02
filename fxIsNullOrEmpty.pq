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
		_checks,
    fxDocumentation =
        type function (
            optional valueToCheck as (
                type any meta [
                    Documentation.FieldCaption = "Value To Check",
                    Documentation.FieldDescription = "The value to check if null.",
                    Documentation.SampleValues = 
                        {
                            "1",
                            "null",
                            """blank""",
                            "test"
                        }
                ]
            )
        ) as logical meta
            [
                Documentation.Name = "fxIsNullOrEmpty",
                Documentation.Description = "Checks if a value is null or like null.",
                Documentation.Category = "Clean",
                Documentation.Examples =
                    {
                        [
                            Description = "",
                            Code = "null",
                            Result = "true"
                        ],
                        [
                            Description = "",
                            Code = """blank""",
                            Result = "true"
                        ],
                        [
                            Description = "",
                            Code = "1",
                            Result = "false"
                        ]                                                
                    }
            ],
    fxReplaceMeta =
        Value.ReplaceType (
            fxFunction,
            fxDocumentation
        ) 
in
    fxReplaceMeta
