# `ids()` works

    c("P1", "P3", "P5", "P7", "P9", "P11", "P13", "P15", "P17", "P19", 
    "P21", "P23", "P25", "P27", "P29", "P31", "P33", "P35", "P37", 
    "P39", "P41", "P43", "P45", "P47", "P49", "P51", "P53", "P55", 
    "P57", "P59", "P61", "P63", "P65", "P67", "P69", "P71", "P73", 
    "P75", "P77", "P79", "P81", "P83", "P85", "P87", "P89", "P91", 
    "P93", "P95", "P97", "P99", "P101", "P103", "P105", "P107", "P109", 
    "P111", "P113", "P115", "P117", "P119", "P121", "P123", "P125", 
    "P127", "P129", "P131", "P133", "P135", "P137", "P139", "P141", 
    "P143", "P145", "P147", "P149", "P151", "P153", "P155", "P157", 
    "P159", "P161", "P163", "P165", "P167", "P169", "P171", "P173", 
    "P175", "P177", "P179", "P181", "P183", "P185", "P187", "P189", 
    "P191", "P193", "P195", "P197", "P199", "P2", "P4", "P6", "P8", 
    "P10", "P12", "P14", "P16", "P18", "P20", "P22", "P24", "P26", 
    "P28", "P30", "P32", "P34", "P36", "P38", "P40", "P42", "P44", 
    "P46", "P48", "P50", "P52", "P54", "P56", "P58", "P60", "P62", 
    "P64", "P66", "P68", "P70", "P72", "P74", "P76", "P78", "P80", 
    "P82", "P84", "P86", "P88", "P90", "P92", "P94", "P96", "P98", 
    "P100", "P102", "P104", "P106", "P108", "P110", "P112", "P114", 
    "P116", "P118", "P120", "P122", "P124", "P126", "P128", "P130", 
    "P132", "P134", "P136", "P138", "P140", "P142", "P144", "P146", 
    "P148", "P150", "P152", "P154", "P156", "P158", "P160", "P162", 
    "P164", "P166", "P168", "P170", "P172", "P174", "P176", "P178", 
    "P180", "P182", "P184", "P186", "P188", "P190", "P192", "P194", 
    "P196", "P198", "P200")

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

