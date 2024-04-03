[
    fxGetMonthNumberFromMonthName =
        let
            fxFunction =
                (
                    monthName as text
                ) as number =>
                    let
                        _month_number_position =
                            List.PositionOf (
                                {
                                    "jan",
                                    "feb",
                                    "mar",
                                    "apr",
                                    "may",
                                    "jun",
                                    "jul",
                                    "aug",
                                    "sep",
                                    "oct",
                                    "nov",
                                    "dec"
                                },
                                Text.Start ( Text.Lower ( Text.Trim ( monthName ) ), 3 )
                            ),
                        _month_number =
                            if _month_number_position = -1 then
                                error "Invalid month name"
                            else
                                _month_number_position + 1
                    in
                        _month_number,
            fxDocumentation =
                type function (
                    monthName as (
                        type text meta [
                            Documentation.FieldCaption = "Month Name",
                            Documentation.FieldDescription = "The month name as a string.",
                            Documentation.SampleValues = 
                                {
                                    "January",
                                    "Jun",
                                    "december"
                                }
                        ]
                    )
                ) as number meta
                    [
                        Documentation.Name = "fxGetMonthNumberFromMonthName",
                        Documentation.Description = "Replaces month string name with month number.",
                        Documentation.Category = "Date/Time",
                        Documentation.Examples =
                            {
                                [
                                    Description = "",
                                    Code = "fxGetMonthNumberFromMonthName ( ""June"" )",
                                    Result = "6"
                                ]
                            }
                    ],
            fxReplaceMeta =
                Value.ReplaceType (
                    fxFunction,
                    fxDocumentation
                ) 
        in
            fxReplaceMeta,
    fxGetDateFromFiscalMonth =
        let
            fxFunction =
                (
                    fiscalYear as number,
                    month as any
                ) as date =>
                    let
                        _month_number =
                            if
                                Value.Is (
                                    month,
                                    type text
                                )
                            then
                                fxGetMonthNumberFromMonthName ( month )
                            else
                                month,
                        _calendar_year =
                            fiscalYear -
                                (
                                    if
                                        _month_number >= 7
                                    then
                                        1
                                    else
                                        0
                                ),
                        _fist_day_of_month =
                            #date (
                                _calendar_year,
                                _month_number,
                                1
                            )
                    in
                        _fist_day_of_month,
            fxDocumentation =
                type function (
                    fiscalYear as (
                        type number meta [
                            Documentation.FieldCaption = "Fiscal Year",
                            Documentation.FieldDescription = "The fiscal year used for the date.",
                            Documentation.SampleValues = 
                                {
                                    "1995",
                                    "2024"
                                }
                        ]
                    ),
                    month as (
                        type any meta [
                            Documentation.FieldCaption = "Month",
                            Documentation.FieldDescription = "The month used for the date.",
                            Documentation.SampleValues = 
                                {
                                    "3",
                                    "12",
                                    "April"
                                }
                        ]
                    )            
                ) as date meta
                    [
                        Documentation.Name = "fxGetDateFromFiscalMonth",
                        Documentation.Description = "This function takes a fiscal year and a month (either as a number or text) and returns the first day of the month in the corresponding calendar year.",
                        Documentation.Category = "Date/Time",
                        Documentation.Examples =
                            {
                                [
                                    Description = "",
                                    Code = "fxGetDateFromFiscalMonth ( 2023, 5 )",
                                    Result = "5/1/2023"
                                ],
                                [
                                    Description = "",
                                    Code = "fxGetDateFromFiscalMonth ( 2023, ""July"" )",
                                    Result = "7/1/2022"
                                ]                        
                            }
                    ],
            fxReplaceMeta =
                Value.ReplaceType (
                    fxFunction,
                    fxDocumentation
                ) 
        in
            fxReplaceMeta,
    fxGetDateFromFiscalQuarter =
        let
            fxFunction =
                (
                    fiscalYear as number,
                    fiscalQuarter as number,
                    optional monthOffset as number
                ) as date =>
                    let
                        _cleaned_month_offset =
                            if
                                monthOffset is null
                            then
                                0
                            else
                                monthOffset,
                        _month_list =
                            {
                                7,
                                10,
                                1,
                                4
                            },
                        _month_number =
                            _month_list { fiscalQuarter - 1 } +
                                _cleaned_month_offset,
                        _date =
                            fxGetDateFromFiscalMonth (
                                fiscalYear,
                                _month_number
                            )
                    in
                        _date,
            fxDocumentation =
                type function (
                    fiscalYear as (
                        type number meta [
                            Documentation.FieldCaption = "Fiscal Year",
                            Documentation.FieldDescription = "The fiscal year used for the date.",
                            Documentation.SampleValues = 
                                {
                                    "1995",
                                    "2024"
                                }
                        ]
                    ),
                    fiscalQuarter as (
                        type number meta [
                            Documentation.FieldCaption = "Month",
                            Documentation.FieldDescription = "The quarter used for the date.",
                            Documentation.SampleValues = 
                                {
                                    "1",
                                    "3",
                                    "4"
                                }
                        ]
                    ),
                    optional monthOffset as (
                        type number meta [
                            Documentation.FieldCaption = "Month Offset",
                            Documentation.FieldDescription = "The number of months to shift the quarter month by.",
                            Documentation.SampleValues = 
                                {
                                    "1",
                                    "2",
                                    "3"
                                }
                        ]
                    )                        
                ) as date meta
                    [
                        Documentation.Name = "fxGetDateFromFiscalQuarter",
                        Documentation.Description = "This function takes a fiscal year, a fiscal quarter, and an optional month offset, and returns the date of the first day of the month in the corresponding calendar year, adjusted by the month offset.",
                        Documentation.Category = "Date/Time",
                        Documentation.Examples =
                            {
                                [
                                    Description = "",
                                    Code = "fxGetDateFromFiscalQuarter ( 2023, 1 )",
                                    Result = "7/1/2022"
                                ],
                                [
                                    Description = "",
                                    Code = "fxGetDateFromFiscalQuarter ( 2023, 2, 1 )",
                                    Result = "11/1/2022"
                                ]                        
                            }
                    ],
            fxReplaceMeta =
                Value.ReplaceType (
                    fxFunction,
                    fxDocumentation
                ) 
        in
            fxReplaceMeta,
    fxCleanAndLowerString =
        let
            fxFunction =
                (
                    stringToClean as text
                ) as text =>
                    let
                        _result =
                            Text.Trim (
                                Text.Clean (
                                    Text.Lower (
                                        stringToClean
                                    )
                                )
                            )
                    in
                        _result,
            fxDocumentation =
                type function (
                    stringToClean as (
                        type text meta [
                            Documentation.FieldCaption = "String to Clean",
                            Documentation.FieldDescription = "The string you want to clean and lower.",
                            Documentation.SampleValues = 
                                {
                                    "HeLLo  ",
                                    "  World"
                                }
                        ]
                    )
                ) as number meta
                    [
                        Documentation.Name = "fxCleanAndLowerString",
                        Documentation.Description = "Cleans, trims, and converts to lower case the input string.",
                        Documentation.Category = "Clean",
                        Documentation.Examples =
                            {
                                [
                                    Description = "",
                                    Code = "fxCleanAndLowerString ( "" HeLLo"" )",
                                    Result = "hello"
                                ],
                                [
                                    Description = "",
                                    Code = "fxCleanAndLowerString ( "" WORLd  "" )",
                                    Result = "world"
                                ]                        
                            }
                    ],
            fxReplaceMeta =
                Value.ReplaceType (
                    fxFunction,
                    fxDocumentation
                ) 
        in
            fxReplaceMeta,
    fxSetColumnTypesFromModel =
        let 
            fxFunction =
                (
                    tableToTransform as table,
                    modelTable as table 
                ) as table =>
                    let
                        _model_table_names = // Get column names of model table.
                            Table.ColumnNames ( modelTable ),
                        _model_table_types_text = // Get names of column types of model table.
                            Table.Schema ( modelTable )[Kind],
                        _get_model_table_types_from_text = // For each column type name, get column type.
                            List.Transform (
                                _model_table_types_text,
                                each
                                    Record.Field (
                                        [
                                            any = type any,
                                            binary = type binary,
                                            date = type date,
                                            datetime = type datetime,
                                            datetimezone = type datetimezone,
                                            duration = type duration,
                                            list = type list,
                                            logical = type logical,
                                            null = type any,
                                            number = type number,
                                            record = type record,
                                            table = type table,
                                            text = type text,
                                            time = type time
                                        ],
                                        _
                                    )
                            ),
                        _combine_names_and_types = // Create list of column names and types.
                            List.Zip (
                                {
                                    _model_table_names,
                                    _get_model_table_types_from_text
                                }
                            ),
                        _change_types = // Apply each column name and type to tableToTransform
                            List.Accumulate (
                                _combine_names_and_types,
                                tableToTransform,
                                ( state, current ) => 
                                    Table.TransformColumnTypes (
                                        state,
                                        {
                                            {
                                                current{0},
                                                current{1}
                                            }
                                        }
                                    )
                            )
                    in
                        _change_types,
            fxDocumentation =
                type function (
                    tableToTransform as (
                        type table meta [
                            Documentation.FieldCaption = "Table to Transform",
                            Documentation.FieldDescription = "The table on which you want to change the column types."
                        ]
                    ),
                    modelTable as (
                        type table meta [
                            Documentation.FieldCaption = "Model Table",
                            Documentation.FieldDescription = "The table containing the model column types."
                        ]
                    )            
                ) as table meta
                    [
                        Documentation.Name = "fxSetColumnTypesFromModel",
                        Documentation.Description = "This function replaces the column types of one table with the column types of another table with the same column names.",
                        Documentation.Category = "Clean"
                    ],
            fxReplaceMeta =
                Value.ReplaceType (
                    fxFunction,
                    fxDocumentation
                ) 
        in
            fxReplaceMeta,
    fxReplaceWithDefaultValues =
        let 
            fxFunction =
                (
                    tableToTransform as table,
                    columnsToTransform as list,
                    optional replaceNumbers as logical // if set to false, columns with number type are ignored
                ) as table =>
                    let 
                        _table_schema =
                            Table.Buffer (
                                Table.SelectRows (
                                    Table.Schema ( tableToTransform ),
                                    each
                                        List.Contains (
                                            columnsToTransform,
                                            [Name]
                                        )
                                )
                            ),  
                        _replace_skey_null =
                            Table.ReplaceValue (
                                tableToTransform,
                                null,
                                -1,
                                Replacer.ReplaceValue,
                                Table.SelectRows (
                                    _table_schema,
                                    each
                                        Text.Start (
                                            [Name],
                                            6
                                        ) = "_SKey " 
                                )[Name]
                            ),                    
                        _replace_text_null =
                            Table.TransformColumns (
                                _replace_skey_null,
                                List.Transform (
                                    Table.SelectRows (
                                        _table_schema,
                                        each
                                            [Kind] = "text"
                                    )[Name],
                                    each {
                                        _,
                                        each 
                                            if
                                                List.Contains (
                                                    {
                                                        null,
                                                        "blank",
                                                        "empty",
                                                        "unknown",
                                                        "null",                                  
                                                        ""
                                                    },   
                                                    Text.Lower ( _ )                             
                                                )
                                            then 
                                                "Unknown"
                                            else
                                                _,
                                        type
                                            text
                                    }
                                )
                            ),  
                        _replace_number_null =
                            if 
                                replaceNumbers
                            then
                                Table.ReplaceValue (
                                    _replace_text_null,
                                    null,
                                    0,
                                    Replacer.ReplaceValue,
                                    Table.SelectRows (
                                        _table_schema,
                                        each
                                            [Kind] = "number"
                                    )[Name]
                                )     
                            else
                                _replace_text_null
                    in
                        _replace_number_null,
            fxDocumentation =
                type function (
                    tableToTransform as (
                        type table meta [
                            Documentation.FieldCaption = "Table to Transform",
                            Documentation.FieldDescription = "The table you want to replace values on."
                        ]
                    ),
                    columnsToTransform as (
                        type list meta [
                            Documentation.FieldCaption = "Columns To Transform",
                            Documentation.FieldDescription = "The list of column names to perform the transformation on."
                        ]
                    ),
                    optional replaceNumbers as (
                        type logical meta [
                            Documentation.FieldCaption = "Replace Numbers",
                            Documentation.FieldDescription = "Boolean flag to determine if null numbers should be replaced with 0."
                        ]
                    )                           
                ) as table meta
                    [
                        Documentation.Name = "fxReplaceWithDefaultValues",
                        Documentation.Description = "Replaces nulls and like null values with a default, standard value.",
                        Documentation.Category = "Clean",
                        Documentation.Examples =
                            {
                                [
                                    Description = "",
                                    Code =
                                        "
        fxReplaceWithDefaultValues ( #table (), true, true )
        TextCol, NumberCol
        Hello, 1
        Test, 2,
        Justin, null,
        (blank), 3,
        BLANK, 4
                                        ",
                                    Result =
                                        "
        TextCol, NumberCol
        Hello, 1
        Test, 2,
        Justin, 0,
        Unknown, 3,
        Unknown, 4
                                        "                         
                                ]
                            }
                    ],
            fxReplaceMeta =
                Value.ReplaceType (
                    fxFunction,
                    fxDocumentation
                ) 
        in
            fxReplaceMeta,
    fxRemoveSubstrings =
        let
            Source = (
            inputString as text,
            listToRemove as list
        ) as text =>
            List.Accumulate (
                listToRemove,
                inputString,
                (
                    state,
                    current
                ) =>
                    Text.Replace (
                        state,
                        current,
                        ""
                    )
            )
        in
            Source,
    fxIsNullOrEmpty =
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
            fxReplaceMeta,
    fxBooleanToBinaryString =
        let
            Source = (
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
        in
            Source,
    fxConvertBooleans =
        let
            Source = (
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
        in
            Source,
    fxUnzipList =
        let
            Source = (
            listOfLists as list,
            indexNumber as number
        ) as list =>
            let
                _transformed_list =
                    List.Transform (
                        listOfLists,
                        each _{ indexNumber }
                    )
            in
                _transformed_list
        in
            Source,
    fxIsUpper =
        let
            Source = (
            char as text
        ) as logical =>
            Text.Upper ( char ) = char
        in
            Source,
    fxSplitCamelCaseText =
        let
            fxFunction =
                (
                    textToTransform as text
                ) as text =>
                    let
                        _reverse_text =
                            Text.Reverse ( textToTransform ),
                        _list_of_characters =
                            Text.ToList ( _reverse_text ),
                        _create_reversed_text =
                            List.Accumulate (
                                _list_of_characters,
                                [ 
                                    is_upper_previous = true,
                                    transformed_text = ""
                                ],
                                ( state, current ) => 
                                    [
                                        is_upper_previous =
                                            if
                                                current = " "
                                            then 
                                                true
                                            else
                                                fxIsUpper ( current ),
                                        transformed_text =                   
                                            if
                                                current = " "
                                            then
                                                state[transformed_text]
                                            else if 
                                                Text.End ( state[transformed_text], 1 ) = " "
                                            then 
                                                state[transformed_text] & current
                                            else if 
                                                fxIsUpper ( current ) and 
                                                    not state[is_upper_previous]
                                            then
                                                state[transformed_text] & current & " "
                                            else if  
                                                not fxIsUpper ( current ) and 
                                                    state[is_upper_previous]
                                            then
                                                state[transformed_text] & " " & current
                                            else                        
                                                state[transformed_text] & current
                                    ]
                            )[transformed_text],
                        _restore_text_order =
                            Text.Reverse ( _create_reversed_text ),
                        _trim_text =
                            Text.Trim ( _restore_text_order ),
                        _capitalize_first_word =
                            Text.Upper ( Text.Start ( _trim_text, 1 ) ) &
                                Text.End ( _trim_text, Text.Length ( _trim_text ) - 1 )     
                    in
                        _capitalize_first_word,
            fxDocumentation =
                type function (
                    textToTransform as (
                        type text meta [
                            Documentation.FieldCaption = "Text to Transform",
                            Documentation.FieldDescription = "The text stored in camel or pascal case.",
                            Documentation.SampleValues = 
                                {
                                    "helloWorld",
                                    "ThisIsASampleText"
                                }
                        ]
                    )                     
                ) as text meta
                    [
                        Documentation.Name = "fxSplitCamelCaseText",
                        Documentation.Description = "This function splits a string stored in camel or pascal case into multiple words separated by spaces.",
                        Documentation.Category = "Clean",
                        Documentation.Examples =
                            {
                                [
                                    Description = "",
                                    Code = "fxSplitCamelCaseText ( ""helloWorld"" )",
                                    Result = "Hello World"
                                ],
                                [
                                    Description = "",
                                    Code = "fxSplitCamelCaseText ( ""ThisIsAnExampleString"" )",
                                    Result = "This Is An Example String"
                                ]                        
                            }
                    ],
            fxReplaceMeta =
                Value.ReplaceType (
                    fxFunction,
                    fxDocumentation
                ) 
        in
            fxReplaceMeta,
    fxSplitCamelCaseColumns =
        let
            fxFunction =
                (
                    tableToTransform as table,
                    optional columnNameOverrides as nullable list
                ) as table =>
                    let
                        _source_column_names =
                            Table.ColumnNames ( tableToTransform ),
                        _clean_column_name_overrides =
                            columnNameOverrides ?? {},            
                        _columns_to_override =
                            fxUnzipList (
                                _clean_column_name_overrides,
                                0
                            ),
                        _columns_to_transform =
                            List.RemoveItems (
                                _source_column_names,
                                _columns_to_override
                            ),            
                        _transform_source_column_names =
                            List.Transform (
                                _columns_to_transform,
                                each
                                    {
                                        _,
                                        fxSplitCamelCaseText ( _ )
                                    }
                            ),
                        _combine_lists =
                            List.Combine (
                                {
                                    _clean_column_name_overrides,
                                    _transform_source_column_names
                                }
                            ),
                        _apply_changes =
                            Table.RenameColumns (
                                tableToTransform,
                                _combine_lists
                            )
                    in
                        _apply_changes,
            fxDocumentation =
                type function (
                    tableToTransform as (
                        type table meta [
                            Documentation.FieldCaption = "Table To Transform",
                            Documentation.FieldDescription = "The table whose column names you want to transform.",
                            Documentation.SampleValues = 
                                {
                                    "#table()"
                                }
                        ]
                    ),
                    optional columnNameOverrides as (
                        type nullable list meta [
                            Documentation.FieldCaption = "Column Name Overrides",
                            Documentation.FieldDescription = "A list of column name overrides to be used instead of the default transformation.",
                            Documentation.SampleValues = 
                                {
                                    "{ ""OldColumnName"", ""Override Column Name"", }"
                                }
                        ]
                    )                                 
                ) as text meta
                    [
                        Documentation.Name = "fxSplitCamelCaseColumns",
                        Documentation.Description = "This function splits all camel case column names in a table and converts them to proper case. The function also takes an optional list of alternative column name transformations.",
                        Documentation.Category = "Clean"
                    ],
            fxReplaceMeta =
                Value.ReplaceType (
                    fxFunction,
                    fxDocumentation
                ) 
        in
            fxReplaceMeta,
    fxIsInList =
        let
            fxFunction =
                (
                    inputString as text,
                    optional listToKeep as list,
                    optional listToExclude as list
                ) as logical =>
                    let
                        _checkSubstring = 
                            if
                                listToKeep is null
                            then
                                true
                            else
                                List.AnyTrue (
                                    List.Transform (
                                        listToKeep,
                                        each Text.Contains (
                                                inputString,
                                                _
                                            )
                                    )
                                ),        
                        _checkNotSubstring =
                            if
                                listToExclude is null
                            then
                                true
                            else
                                not List.AnyTrue (
                                        List.Transform (
                                            listToExclude,
                                            each Text.Contains (
                                                    inputString,
                                                    _
                                                )
                                        )
                                    ),
                        _result =
                            _checkSubstring and
                                _checkNotSubstring
                    in
                        _result,
            fxDocumentation =
                type function (
                    inputString as (
                        type text meta [
                            Documentation.FieldCaption = "Input String",
                            Documentation.FieldDescription = "The string you want to search.",
                            Documentation.SampleValues = 
                                {
                                    "Hello my name is Justin."
                                }
                        ]
                    ),
                    optional listToKeep as (
                        type list meta [
                            Documentation.FieldCaption = "List to Keep",
                            Documentation.FieldDescription = "The list of values you want to search for.",
                            Documentation.SampleValues = 
                                {
                                    "{ ""Justin"", ""Bob"", ""Ashish"" }"
                                }
                        ]
                    ),
                    optional listToExclude as (
                        type list meta [
                            Documentation.FieldCaption = "List to Exclude",
                            Documentation.FieldDescription = "The list of values you want to exclude.",
                            Documentation.SampleValues = 
                                {
                                    "{ ""Ron"", ""Ariel"", ""Palavi"" }"
                                }
                        ]
                    )                        
                ) as number meta
                    [
                        Documentation.Name = "fxIsInList",
                        Documentation.Description = "This function returns true if the input string contains any of the substrings in the list to keep and does not contain any of the substrings in the list to exclude.",
                        Documentation.Category = "List",
                        Documentation.Examples =
                            {
                                [
                                    Description = "",
                                    Code = "fxIsInList ( ""Hello my name is Justin Martin"", { ""Justin"" } )",
                                    Result = "true"
                                ],
                                [
                                    Description = "",
                                    Code = "fxIsInList ( ""Hello my name is Justin Martin"", { ""Justin"" }, { ""Martin"" } )",
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
            fxReplaceMeta,
    fxAddNullRecord =
        let
            fxFunction =
                (
                    tableToTransform as table
                ) as table =>
                    let
                        _columnn_names =
                            Table.ColumnNames ( tableToTransform ),
                        _null_record =
                            Record.FromList (
                                List.Repeat (
                                    { null },
                                    List.Count ( _columnn_names )
                                ),
                                _columnn_names
                            ),                 
                        _combine_tables =
                            Table.Combine (
                                {
                                    tableToTransform,
                                    Table.FromRecords (
                                        { _null_record }
                                    )
                                }
                            ),
                        _restore_column_types =
                            fxSetColumnTypesFromModel (
                                _combine_tables,
                                tableToTransform
                            )
                    in
                        _restore_column_types,
            fxDocumentation =
                type function (
                    tableToTransform as (
                        type table meta [
                            Documentation.FieldCaption = "Table to Transform",
                            Documentation.FieldDescription = "The table on which to add a null record.."
                        ]
                    )
                ) as table meta
                    [
                        Documentation.Name = "fxAddNullRecord",
                        Documentation.Description = "This function takes a table and adds a row of null values to the bottom of the table, then restores the column types of the resulting table to match the original table.",
                        Documentation.Category = "Generate"
                    ],
            fxReplaceMeta =
                Value.ReplaceType (
                    fxFunction,
                    fxDocumentation
                ) 
        in
            fxReplaceMeta,
    fxGroupSelfJoin =
        let
            Source = (
            tableToTransform as table,
            columnsToGroupBy as list,
            columnToAggregate as text,
            newColumnName as text,
            groupType as text,
            optional columnToFilterBy as text,
            optional columnToFilterByValue as any    
        ) as table =>
            let
                _clean_group_type = fxCleanAndLowerString ( groupType ),
                _groupingFunction = 
                    if
                        _clean_group_type = "max"
                    then
                        List.Max
                    else if
                        _clean_group_type = "min"
                    then
                        List.Min
                    else if
                        _clean_group_type = "average"
                    then
                        List.Average             
                    else
                        List.Count,
                _create_lookup_table =
                    Table.Group (
                        tableToTransform,
                        columnsToGroupBy,
                        {
                            {
                                newColumnName,
                                each _groupingFunction(
                                    Table.Column (
                                        _,
                                        columnToAggregate
                                    )
                                )
                            }
                        }
                    ),
                _select_rows =
                    if
                        columnToFilterBy is null
                    then
                        _create_lookup_table
                    else
                        Table.SelectRows (
                            _create_lookup_table,
                            each
                                Record.Field (
                                        _,
                                        columnToFilterBy
                                ) = columnToFilterByValue            
                        ),
                _cleaned_columns_to_group_by =
                    List.RemoveItems (
                        columnsToGroupBy,
                        { columnToFilterBy }
                    ),
                _join_tables =
                    Table.NestedJoin (
                        tableToTransform,
                        _cleaned_columns_to_group_by,
                        _select_rows,
                        _cleaned_columns_to_group_by,
                        "tempJoin",
                        JoinKind.LeftOuter
                    ),
                _expand_join =
                    Table.ExpandTableColumn (
                        _join_tables,
                        "tempJoin",
                        { newColumnName },
                        { newColumnName }
                    )
            in
                _expand_join
        in
            Source,
    fxJoinAndExpandTable =
        let
            fxFunction =
                ( 
                    rowIntegritySetting as number,          
                    sourceTable as table,
                    targetTable as table,
                    sourceJoinColumns as list,
                    columnsToExpand as list,
                    optional targetJoinColumns as list,            
                    optional alternateColumnsToExpand as list
                ) as table =>
                    let 
                        _temp_column_name = 
                            "_TempJoin",
                        _columns_to_join_clean = // use if target join columns different than source join columns
                            targetJoinColumns ?? sourceJoinColumns,
                        _columns_to_expand_clean = // use if renaming columns to expand
                            alternateColumnsToExpand ?? columnsToExpand,
                        _target_table_clean = // force row integrity on target table
                            if 
                                rowIntegritySetting = 2
                            then
                                Table.Distinct (
                                    targetTable,
                                    _columns_to_join_clean
                                )
                            else
                                targetTable,
                        _join_tables = // perform join
                            Table.NestedJoin (
                                sourceTable,
                                sourceJoinColumns,
                                _target_table_clean,
                                _columns_to_join_clean,
                                _temp_column_name,
                                JoinKind.LeftOuter
                            ),
                        _expand_join = // expand columns
                            Table.ExpandTableColumn (
                                _join_tables,
                                _temp_column_name,
                                columnsToExpand,
                                _columns_to_expand_clean
                            ),
                        _check_row_integrity = // check if the row counts on the source table are the same after joining the target table
                            if
                                rowIntegritySetting = 1
                            then
                                if
                                    Table.RowCount ( sourceTable ) <> Table.RowCount ( _expand_join )
                                then
                                    error "Left join has generated additional rows" 
                                else
                                    _expand_join
                            else
                                _expand_join
                    in 
                        _check_row_integrity,
            fxDocumentation =
                type function (
                    rowIntegritySetting as (
                        type number meta [
                            Documentation.FieldCaption = "Row Integrity Setting",
                            fxDocumentation.AllowedValues = { 0, 1, 2 },
                            Documentation.FieldDescription =
        "0.  None - Do nothing.
        1. Check Row Integrity - Check to see if the beginning and end row counts are the same in the source table after joining the target table. If not, throw an error.
        2. Force Row Integrity - Force a Table.Distinct on the join columns for the target table."
                        ]
                    ),    
                    sourceTable as (
                        type table meta [
                            Documentation.FieldCaption = "Source Table",
                            Documentation.FieldDescription = "The source table to be joined to."
                        ]
                    ),
                    targetTable as (
                        type table meta [
                            Documentation.FieldCaption = "Target Table",
                            Documentation.FieldDescription = "The target table to join."
                        ]
                    ),            
                    sourceJoinColumns as (
                        type list meta [
                            Documentation.FieldCaption = "Source Join Columns",
                            Documentation.FieldDescription = "The list of column names to join. Must be the same in both tables if targetJoinColumns is null."
                        ]
                    ),
                    columnsToExpand as (
                        type list meta [
                            Documentation.FieldCaption = "Columns To Expand",
                            Documentation.FieldDescription = "The list of column names you want to expand from the target table."
                        ]
                    ),
                    optional targetJoinColumns as (
                        type list meta [
                            Documentation.FieldCaption = "Target Join Columns",
                            Documentation.FieldDescription = "Optional list of column names in the target table to join. Leave null if the column names are the same in the source table."
                        ]
                    ),            
                    optional alternateColumnsToExpand as (
                        type list meta [
                            Documentation.FieldCaption = "Alternate Columns To Expand",
                            Documentation.FieldDescription = "Optional list of column names to expand. Columns expanded in columnsToExpand will be renamed to this list when joined."
                        ]
                    )                                                                      
                ) as table meta
                    [
                        Documentation.Name = "fxJoinAndExpandTable",
                        Documentation.Description = "This function performs a left join on a source and target table and expands the list of provided columns.",
                        Documentation.Category = "Generate"
                    ],
            fxReplaceMeta =
                Value.ReplaceType (
                    fxFunction,
                    fxDocumentation
                ) 
        in
            fxReplaceMeta,
    fxGenerateSurrogateKeyColumn =
        let
            fxFunction =
                (
                    tableToTransform as table,
                    newSKeyColumnName as text,
                    optional columnsToGroupBy as list  
                ) as table =>
                    let
                        _cleaned_skey_column = // Set new surrogate key column name.
                            "_SKey " & newSKeyColumnName,
                        _create_table_group = 
                            if
                                columnsToGroupBy is null
                            then // If columnsToGroupBy is null, a unique surrogate key is generated for each record in the table.  
                                Table.AddIndexColumn (
                                    tableToTransform,
                                    _cleaned_skey_column,
                                    0,
                                    1,
                                    Int64.Type
                                )                
                            else
                                let
                                    _create_distinct_table_group =
                                        Table.AddIndexColumn (
                                            Table.Distinct (
                                                Table.SelectColumns (
                                                    tableToTransform,
                                                    columnsToGroupBy
                                                )
                                            ),
                                            _cleaned_skey_column,
                                            0,
                                            1,
                                            Int64.Type
                                        ),
                                    _join_and_expand_table =
                                        fxJoinAndExpandTable (
                                            1,
                                            tableToTransform,
                                            _create_distinct_table_group,
                                            columnsToGroupBy,
                                            { _cleaned_skey_column }
                                        )
                                in
                                    _join_and_expand_table               
                    in
                        _create_table_group,
            fxDocumentation = 
                type function (
                    tableToTransform as (
                        type table meta [
                            Documentation.FieldCaption = "Table to Transform",
                            Documentation.FieldDescription = "The table on which to generate a new surrogate key column."
                        ]
                    ),
                    newSKeyColumnName as (
                        type text meta [
                            Documentation.FieldCaption = "New SKey Column Name",
                            Documentation.FieldDescription = "The new column name for the surrogate key. The input will be prefixed with ""_SKey"".",
                            Documentation.SampleValues = 
                                {
                                    "Geo",
                                    "Parcel",
                                    "Ticket"
                                }
                        ]
                    ),
                    optional columnsToGroupBy as (
                        type list meta [
                            Documentation.FieldCaption = "Columns To Group By",
                            Documentation.FieldDescription = "The list of columns used to generate the surrogate key. A unique key will be generated for each unique combination of values. If left blank, each row will be considered unique.",
                            Documentation.SampleValues = 
                                {
                                    "null",
                                    "{ ""AZ"", ""Metro"" }"
                                }
                        ]
                    )                        
                ) as table meta
                    [
                        Documentation.Name = "fxGenerateSurrogateKeyColumn",
                        Documentation.Description = "This function takes a table, a new SKey column name, and an optional list of columns to group by, and returns a new table with an added SKey column that assigns a unique index to each row or group of rows.",
                        Documentation.Category = "Generate",
                        Documentation.Examples =
                            {
                                [
                                    Description = "",
                                    Code = "fxGenerateSurrogateKeyColumn ( dimParcel, ""Parcel"", {""Parcel Id""} )",
                                    Result = "dimParcel table with new surrogate key column called ""_SKey Parcel""."
                                ]
                            }
                    ],
            fxReplaceMeta =
                Value.ReplaceType (
                    fxFunction,
                    fxDocumentation
                ) 
        in
            fxReplaceMeta,
    fxCreateMergedColumns =
        let
            Source = (
            tableToTransform as table,
            transformationList as list
        ) =>
            let
                _add_columns =
                    List.Accumulate (
                        transformationList,
                        tableToTransform,
                        (
                            state,
                            current
                        ) => 
                            let
                                _new_column_name =
                                    current { 0 },
                                _column_1 =
                                    current { 1 },
                                _column_2 =
                                    current { 2 },
                                _column_type =
                                    current { 3 },
                                _add_column =
                                    Table.AddColumn (
                                        state,
                                        _new_column_name,
                                        each
                                            Record.FieldOrDefault ( _, _column_1 ) ??
                                                Record.FieldOrDefault ( _, _column_2 ),
                                        _column_type
                                    )
                            in
                                _add_column
                    )
            in
                _add_columns
        in
            Source,
    fxExtractDimensionTable =
        (
            sourceTable as table,
            dimensionColumns as list,
            optional qualityCheck as logical
        ) as table =>
            let
                _default_quality_check =
                    qualityCheck ?? true, 
                _select_columns__dimensions =
                    Table.SelectColumns (
                        sourceTable,
                        dimensionColumns
                    ),
                _remove_duplicates =
                    Table.Distinct ( _select_columns__dimensions ),
                _row_count =
                    Table.RowCount ( _remove_duplicates ),
                _distinct_skey =
                    Table.SelectRows (
                        Table.Schema ( _select_columns__dimensions ),
                        each
                            Text.Start ( [Name], 6 ) = "_SKey "
                    )[Name]{0},
                _distinct_skey_count =
                    List.NonNullCount (
                        List.Distinct (
                            Table.Column (
                                _remove_duplicates,
                                _distinct_skey
                            )
                        )
                    ),
                _quality_check =
                    if
                        _row_count <> _distinct_skey_count and 
                            _default_quality_check
                    then
                        error _distinct_skey & " in the dimension table is not unique." 
                    else
                        _remove_duplicates            
            in
                _quality_check,
    fxCumulativeToIncremental =
        let
            fxFunction =
                (
                    tableToTransform as table,
                    groupByColumns as list,
                    sortByColumn as text,
                    columnsToTransform as list
                ) as table =>
                    let
                        _create_groups = // Group all rows by group by columns
                            Table.Group (
                                tableToTransform,
                                groupByColumns,
                                {
                                    {
                                        "Group",
                                        each
                                            _,
                                        type
                                            table     
                                    }
                                }
                            ),
                        _create_incremental_data = // For each set of grouped rows, create incremental column
                            Table.AddColumn (
                                _create_groups,
                                "Data",
                                each
                                    let
                                        _sort_by_date = // Sort data by sort by column
                                            Table.Sort (
                                                [Group],
                                                { { sortByColumn, Order.Ascending } }
                                            ),
                                        _create_index = // Add index to be used for calculating difference between each cumulative row
                                            Table.AddIndexColumn (
                                                _sort_by_date,
                                                "Index",
                                                0,
                                                1,
                                                Int64.Type
                                            ),
                                        _create_incremental_data_for_each_column = // For each cumulative column, create incremental column
                                            List.Accumulate(
                                                columnsToTransform,
                                                _create_index,
                                                (
                                                    state,
                                                    columnToTransform
                                                ) =>
                                                    let 
                                                        _buffer_data = // Buffer results
                                                            List.Buffer (
                                                                Table.Column(state, columnToTransform)
                                                            ),
                                                        _calculate_incremental_value = // Add column with calculated incremental value
                                                            Table.AddColumn (
                                                                state,
                                                                columnToTransform & " Incremental",
                                                                each
                                                                    if
                                                                        [Index] > 0
                                                                    then                              
                                                                        Record.Field(_, columnToTransform) -
                                                                            _buffer_data { [Index] - 1 }
                                                                    else 
                                                                        Record.Field(_, columnToTransform),
                                                                type
                                                                    number
                                                            )  
                                                        in
                                                            _calculate_incremental_value           
                                            )
                                    in
                                        _create_incremental_data_for_each_column
                            ),
                        _columns_to_extract = // Get list of columns to extract from group
                            List.Combine ( 
                                {
                                    List.RemoveItems (
                                        Table.ColumnNames ( tableToTransform ),
                                        groupByColumns
                                    ),
                                    List.Transform (
                                        columnsToTransform,
                                        each
                                            _ & " Incremental"
                                    )
                                }
                            ),
                        _extract_incremental_data = // Expand columns from group
                            Table.ExpandTableColumn (
                                _create_incremental_data,
                                "Data",
                                _columns_to_extract,
                                _columns_to_extract
                            ),
                        _remove_group_column = // Remove group column
                            Table.RemoveColumns (
                                _extract_incremental_data,
                                { "Group" }
                            ),
                        _restore_column_types = // Set column types
                            fxSetColumnTypesFromModel (
                                _remove_group_column,
                                List.Accumulate(
                                    columnsToTransform,
                                    tableToTransform,
                                    (
                                        state,
                                        column
                                    ) =>                        
                                        Table.AddColumn (
                                            state,
                                            column & " Incremental",
                                            each
                                                null,
                                            type
                                                number
                                        )
                                )
                            )                  
                    in
                        _restore_column_types,
            fxDocumentation =
                type function (
                    tableToTransform as (
                        type table meta [
                            Documentation.FieldCaption = "Table to Transform",
                            Documentation.FieldDescription = "The table containing the cumulative columns to be transformed."
                        ]
                    ),    
                    groupByColumns as (
                        type list meta [
                            Documentation.FieldCaption = "Group By Columns",
                            Documentation.FieldDescription = "The columns that, when taken together, the running total is applied to."
                        ]
                    ),
                    sortByColumn as (
                        type text meta [
                            Documentation.FieldCaption = "Sort by Column",
                            Documentation.FieldDescription = "The time/sort by column for the running total."
                        ]
                    ),            
                    columnsToTransform as (
                        type list meta [
                            Documentation.FieldCaption = "Columns to Transform",
                            Documentation.FieldDescription = "The cumulative columns to be transformed to incremental."
                        ]
                    )                                                                   
                ) as table meta
                    [
                        Documentation.Name = "fxCumulativeToIncremental",
                        Documentation.Description = "This function transforms one or more running total columns into incremental columns.",
                        Documentation.Category = "Generate"
                    ],
            fxReplaceMeta =
                Value.ReplaceType (
                    fxFunction,
                    fxDocumentation
                ) 
        in
            fxReplaceMeta,
    fxGetColumnAttribute =
        let
            fxFunction =
                (
                    sourceTable as table,
                    sourceColumn as text,
                    attribute as text
                ) as any =>
                    let
                        _get_schema =
                            Table.Schema ( sourceTable ),
                        _select_column =
                            Table.SelectRows (
                                _get_schema,
                                each [Name] = sourceColumn
                            ),
                        _select_type =
                            Record.Field (
                                _select_column { 0 },
                                attribute
                            )
                    in
                        _select_type,
            fxDocumentation =
                type function (
                    sourceTable as (
                        type table meta [
                            Documentation.FieldCaption = "Source Table",
                            Documentation.FieldDescription = "The table to analyze.",
                            Documentation.SampleValues = 
                                {
                                    "#table"
                                }
                        ]
                    ),
                    sourceColumn as (
                        type text meta [
                            Documentation.FieldCaption = "Source Column",
                            Documentation.FieldDescription = "The column to analyze on the source table.",
                            Documentation.SampleValues = 
                                {
                                    "Column1"
                                }
                        ]
                    ),
                    attribute as (
                        type text meta [
                            Documentation.FieldCaption = "Attribute",
                            Documentation.FieldDescription = "The attribute about the table you want to analyze.",
                            Documentation.AllowedVAlues = 
                                {
                                    "Name",
                                    "Position",
                                    "TypeName",
                                    "Kind",
                                    "IsNullable",
                                    "NumericPrecisionBase",
                                    "NumericPrecision",
                                    "NumericScale",
                                    "DateTimePrecision",
                                    "MaxLength",
                                    "IsVariableLength",
                                    "NativeTypeName",
                                    "NativeDefaultExpression",
                                    "NativeExpression",
                                    "Description",
                                    "IsWritable",
                                    "FieldCaption"                            
                                },
                            Documentation.SampleValues = 
                                {
                                    "Name",
                                    "Position",
                                    "Kind"
                                }
                        ]
                    )                                               
                ) as any meta
                    [
                        Documentation.Name = "fxGetColumnAttribute",
                        Documentation.Description = "This function returns the value of the specified metadata attribute for the specified column in the source table.",
                        Documentation.Category = "Information",
                        Documentation.Examples =
                            {
                                [
                                    Description = "Return the name of the column.",
                                    Code = "fxGetColumnAttribute ( Geography, ""Metro Name"", ""Name"" )",
                                    Result = """Metro Name"""
                                ],
                                [
                                    Description = "Return the position of the column.",
                                    Code = "fxGetColumnAttribute ( Geography, ""Metro Name"", ""Position"" )",
                                    Result = "6"
                                ],
                                [
                                    Description = "Return the type of the column.",
                                    Code = "fxGetColumnAttribute ( Geography, ""Metro Name"", ""Kind"" )",
                                    Result = """text"""
                                ]                                                                    
                            }
                    ],
            fxReplaceMeta =
                Value.ReplaceType (
                    fxFunction,
                    fxDocumentation
                ) 
        in
            fxReplaceMeta,
    fxHashText =
        let
            Source = (text as text) as text =>
            let
                input = text,
                len = Text.Length(input),
                nChunks = Number.RoundUp((len * 8 + 65) / 512),
                bytes = nChunks * 64,
                output = Text.ToBinary(input, TextEncoding.Ascii),
                /*Define Constants*/
                t = List.Transform({0..32}, each Number.Power(2, _)),
                h0 = {1779033703, 3144134277, 1013904242, 2773480762, 1359893119, 2600822924, 528734635, 1541459225},
                k = {
                    1116352408,
                    1899447441,
                    3049323471,
                    3921009573,
                    961987163,
                    1508970993,
                    2453635748,
                    2870763221,
                    3624381080,
                    310598401,
                    607225278,
                    1426881987,
                    1925078388,
                    2162078206,
                    2614888103,
                    3248222580,
                    3835390401,
                    4022224774,
                    264347078,
                    604807628,
                    770255983,
                    1249150122,
                    1555081692,
                    1996064986,
                    2554220882,
                    2821834349,
                    2952996808,
                    3210313671,
                    3336571891,
                    3584528711,
                    113926993,
                    338241895,
                    666307205,
                    773529912,
                    1294757372,
                    1396182291,
                    1695183700,
                    1986661051,
                    2177026350,
                    2456956037,
                    2730485921,
                    2820302411,
                    3259730800,
                    3345764771,
                    3516065817,
                    3600352804,
                    4094571909,
                    275423344,
                    430227734,
                    506948616,
                    659060556,
                    883997877,
                    958139571,
                    1322822218,
                    1537002063,
                    1747873779,
                    1955562222,
                    2024104815,
                    2227730452,
                    2361852424,
                    2428436474,
                    2756734187,
                    3204031479,
                    3329325298
                },
                /*Define Functions*/
                XOR = Number.BitwiseXor,
                AND = Number.BitwiseAnd,
                MOD32 = (n) => Number.Mod(n, t{32}),
                RROT32 = (n, r) => Number.BitwiseShiftRight(n, r) + t{32 - r} * Number.Mod(n, t{r}),
                s0 = (w) => XOR(XOR(RROT32(w, 7), RROT32(w, 18)), Number.BitwiseShiftRight(w, 3)),
                s1 = (w) => XOR(XOR(RROT32(w, 17), RROT32(w, 19)), Number.BitwiseShiftRight(w, 10)),
                S1 = (w) => XOR(XOR(RROT32(w, 6), RROT32(w, 11)), RROT32(w, 25)),
                S0 = (w) => XOR(XOR(RROT32(w, 2), RROT32(w, 13)), RROT32(w, 22)),
                /*Main compression function*/
                CHUNK = (hash, w) =>
                    let
                        G = List.Generate(
                            () =>
                                [
                                    i = -1,
                                    a = hash{0},
                                    b = hash{1},
                                    c = hash{2},
                                    d = hash{3},
                                    e = hash{4},
                                    f = hash{5},
                                    g = hash{6},
                                    h = hash{7},
                                    temp1 = 0,
                                    temp2 = 0,
                                    ch = 0,
                                    maj = 0
                                ],
                            each [i] < 64,
                            each
                                [
                                    ch = XOR(AND([e], [f]), AND(XOR([e], t{32} - 1), [g])),
                                    temp1 = MOD32([h] + S1([e]) + ch + k{i} + w{i}),
                                    maj = XOR(XOR(AND([a], [b]), AND([a], [c])), AND([b], [c])),
                                    temp2 = MOD32(S0([a]) + maj),
                                    h = [g],
                                    g = [f],
                                    f = [e],
                                    e = MOD32([d] + temp1),
                                    d = [c],
                                    c = [b],
                                    b = [a],
                                    a = MOD32(temp1 + temp2),
                                    i = [i] + 1
                                ]
                            //,
                            //each [s]
                        ),
                        list_abc = Record.ToList(Record.SelectFields(List.Last(G), {"a", "b", "c", "d", "e", "f", "g", "h"}))
                    in
                        list_abc,
                /*Create Message Schedule (w)*/
                list_bytes = {1..bytes},
                list_chunks = List.Transform(list_bytes, each Number.IntegerDivide((_ - 1), 64)),
                list_words = List.Transform(list_bytes, each Number.Mod(Number.IntegerDivide((_ - 1), 4), 16)),
                list_chars = List.Transform(
                    list_bytes,
                    each
                        if _ < len + 1 then
                            Character.ToNumber(Text.At(input, _ - 1))
                        else if _ = len + 1 then
                            128
                        else if _ > bytes - 8 then
                            Number.Mod(Number.BitwiseShiftRight(len * 8, (bytes - _) * 8), 256)
                        else
                            0
                ),
                lists_table = Table.FromColumns(
                    {list_bytes, list_chunks, list_words, list_chars}, {"index", "chunk", "word", "u"}
                ),
                group_words = Table.Buffer(
                    Table.Group(
                        lists_table,
                        {"chunk", "word"},
                        {{"w", each List.Accumulate([u], 0, (state, current) => 256 * state + current), Int32.Type}}
                    )
                ),
                /*Loop through chunks*/
                update_hash = List.Accumulate(
                    {0..nChunks - 1},
                    h0,
                    /*Initial hash*/ (
                        state, current
                    ) =>
                        let
                            w16 = List.Range(group_words[w], 16 * current, 16),
                            w48 = List.Generate(
                                () => [i = 15, w = w16, s = 0],
                                each [i] < 64,
                                each
                                    [
                                        s = MOD32([w]{i - 16} + s0([w]{i - 15}) +[w]{i - 7} + s1([w]{i - 2})),
                                        w = [w] & {s},
                                        i = [i] + 1
                                    ],
                                each [s]
                            ),
                            w64 = w16 & List.Skip(w48),
                            hash = CHUNK(state, w64),
                            endstate = List.Transform({0..7}, each MOD32(state{_} + hash{_}))
                        in
                            endstate
                ),
                /*Convert hash to hex code*/
                hash_bytes = List.Transform(
                    update_hash,
                    (hash) =>
                        List.Transform(
                            {0..3}, (byte) => Number.BitwiseShiftRight(Number.Mod(hash, t{32 - 8 * byte}), 24 - 8 * byte)
                        )
                ),
                hash_hex = List.Transform(hash_bytes, each Binary.ToText(Binary.FromList(_), BinaryEncoding.Hex)),
                concatenate = Text.Combine(hash_hex)
            in
                concatenate
        in
            Source,
    fxGetDataflowItem =
        let
            Source = (
            strWorkspaceName as text,
            strDataflowName as text,
            optional strTableName as nullable text,
            optional blnDevModeOverride as nullable logical          
        ) =>
            let
                _clean_table_name =
                    strTableName ?? strDataflowName,  
                _clean_dev_mode =
                    blnDevModeOverride ?? paramDevMode,                  
                _source =
                    PowerPlatform.Dataflows(),
                _select_workspaces =
                    _source {
                        [ Id = "Workspaces" ]
                    }[Data],
                _select_workspace = 
                    _select_workspaces {
                        [ workspaceName = strWorkspaceName ]
                    }[Data],                                           
                _select_dataflow = 
                    _select_workspace {
                        [ dataflowName = strDataflowName ]
                    }[Data],
                _select_table =
                    _select_dataflow {
                        [
                            entity = _clean_table_name,
                            version = ""
                        ]
                    }[Data],
                _limit_rows =
                    if
                        _clean_dev_mode
                    then 
                        Table.FirstN (
                            _select_table,
                            1
                        )
                    else
                        _select_table
            in
                _limit_rows
        in
            Source,
    fxGetWorkspaceName =
        let
            Source = () as text =>
            let
                _result =
                    "IPA Energy Data - " & paramPipelineStage
            in
                _result
        in
            Source,
    fxJoinGeoMultiGrain =
        let
            Source = (
            tableToTransform as table,
            geoTable as table,
            grain1 as text,
            joinColumns1 as list,
            grain2 as text,
            joinColumns2 as list,
            optional join1RowIntegrity as number,
            optional join2RowIntegrity as number            
        ) as table =>
            let   
                _row_integrity_1_clean =
                    join1RowIntegrity ?? 1,
                _row_integrity_2_clean =
                    join2RowIntegrity ?? 1,   
                _join_table__geography =
                    fxJoinAndExpandTable (
                        _row_integrity_1_clean,
                        tableToTransform,
                        Table.SelectRows (
                            geoTable,
                            each
                                [Geo Type] = grain1
                        ),
                        joinColumns1,
                        { "_SKey Geography" }
                    ),
                _join_table__geography2 =
                    fxJoinAndExpandTable (
                        _row_integrity_2_clean,               
                        _join_table__geography,
                        Table.SelectRows (
                            geoTable,
                            each
                                [Geo Type] = grain2
                        ),
                        joinColumns2,
                        {
                            "_SKey Geography"
                        },
                        null,
                        { "_SKey Geography2" }            
                    ),    
                _clean_geography =
                    Table.ReplaceValue (
                        _join_table__geography2,
                        each
                            [_SKey Geography],
                        each
                            if
                                [_SKey Geography] is null and
                                    [_SKey Geography2] is null
                            then 
                                -1                                   
                            else
                                [_SKey Geography] ?? [_SKey Geography2],
                        Replacer.ReplaceValue,
                        { "_SKey Geography" }
                    ),
                _remove_geo_2 =
                    Table.RemoveColumns (
                        _clean_geography,
                        {
                            "_SKey Geography2"
                        }
                    ),
                _change_type =
                    Table.TransformColumnTypes (
                        _remove_geo_2,
                        {
                            { "_SKey Geography", Int64.Type }
                        }
                    )            
            in
                _change_type
        in
            Source
]
