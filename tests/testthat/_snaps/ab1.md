# results are consistent

    Code
      do.call(dplyr::mutate(dplyr::rename(dplyr::select(params, -c(antigen_iso, iter)),
      shape = r), t = 10), what = ab1)
    Output
      [1] 63.09

