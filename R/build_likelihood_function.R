build_likelihood_function = function(
    cross_sectional_data,
    longitudinal_parameter_samples,
    noise_params,
    antigen_isos = names(cross_sectional_data))
{

  likelihood_function = function(lambda)
  {

    res = 0

    # add terms, e.g. for other antibodies
    for (cur_antigen in antigen_isos)
    {
      res = res +
        fdev(
          lambda = lambda,
          csdata = cross_sectional_data[[cur_antigen]],
          lnpars = longitudinal_parameter_samples[[cur_antigen]],
          cond = noise_params[[cur_antigen]])
    }

    return(res)

  }

  return(likelihood_function)
}
