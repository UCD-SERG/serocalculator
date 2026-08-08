# `ids()` works

    c("P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8", "P9", "P10", 
    "P11", "P12", "P13", "P14", "P15", "P16", "P17", "P18", "P19", 
    "P20", "P21", "P22", "P23", "P24", "P25", "P26", "P27", "P28", 
    "P29", "P30", "P31", "P32", "P33", "P34", "P35", "P36", "P37", 
    "P38", "P39", "P40", "P41", "P42", "P43", "P44", "P45", "P46", 
    "P47", "P48", "P49", "P50", "P51", "P52", "P53", "P54", "P55", 
    "P56", "P57", "P58", "P59", "P60", "P61", "P62", "P63", "P64", 
    "P65", "P66", "P67", "P68", "P69", "P70", "P71", "P72", "P73", 
    "P74", "P75", "P76", "P77", "P78", "P79", "P80", "P81", "P82", 
    "P83", "P84", "P85", "P86", "P87", "P88", "P89", "P90", "P91", 
    "P92", "P93", "P94", "P95", "P96", "P97", "P98", "P99", "P100", 
    "P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8", "P9", "P10", 
    "P11", "P12", "P13", "P14", "P15", "P16", "P17", "P18", "P19", 
    "P20", "P21", "P22", "P23", "P24", "P25", "P26", "P27", "P28", 
    "P29", "P30", "P31", "P32", "P33", "P34", "P35", "P36", "P37", 
    "P38", "P39", "P40", "P41", "P42", "P43", "P44", "P45", "P46", 
    "P47", "P48", "P49", "P50", "P51", "P52", "P53", "P54", "P55", 
    "P56", "P57", "P58", "P59", "P60", "P61", "P62", "P63", "P64", 
    "P65", "P66", "P67", "P68", "P69", "P70", "P71", "P72", "P73", 
    "P74", "P75", "P76", "P77", "P78", "P79", "P80", "P81", "P82", 
    "P83", "P84", "P85", "P86", "P87", "P88", "P89", "P90", "P91", 
    "P92", "P93", "P94", "P95", "P96", "P97", "P98", "P99", "P100"
    )

# `id_varname()` aborts with `.data` pronoun

    Code
      dplyr::filter(sees_pop_data_pk_100, ids(.data) == "P1")
    Condition
      Error in `dplyr::filter()`:
      i In argument: `ids(.data) == "P1"`.
      Caused by error in `ids_varname()`:
      ! can't extract attributes from pronouns

# `ids_varname()` warns when guessing colname

    Code
      ids_varname(xs_data)
    Condition
      Warning:
      No `id_var` attribute found in `object`.
      i Defaulting to 'index_id'.
    Output
      [1] "index_id"

# `ids_varname()` warns when unable to guess colname

    Code
      ids_varname(dplyr::select(xs_data, -index_id))
    Condition
      Error in `ids_varname()`:
      ! No `id_var` attribute found in `object`.

