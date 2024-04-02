[
    fxIsNullOrEmpty = 
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
    fxBooleanToBinaryString =
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
                _checks,
    fxConvertBooleans = 
        (
            tableToTransform as table,
            columnsToTransform as list
        ) as table =>
            let
                _convert_to_text = 
                    Table.TransformColumnTypes (
                        tableToTransform, 
                        List.Transform (
                            columnsToTransform, 
                            each { _, type text }
                        )
                    ),      
                _clean_boolean_columns = 
                    Table.TransformColumns (
                        _convert_to_text,
                        List.Transform (
                            columnsToTransform,
                            each
                                {
                                    _,
                                    each
                                        fxBooleanToBinaryString ( _ )
                                }
                            )
                        ),
                _restore_to_text = 
                    Table.TransformColumnTypes (
                        _clean_boolean_columns, 
                        List.Transform (
                            columnsToTransform, 
                            each { _, type text }
                        )
                    ),
                _replace_errors =
                    Table.ReplaceErrorValues (
                        _restore_to_text,
                        List.Transform (
                            columnsToTransform, 
                            each { _, "0" }
                        )  
                    )                       
        in
            _replace_errors                                           
]
