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
                                fxGetFunction ( "fxBooleanToBinaryString" ) ( _ )
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