bt <- function(y0, y1, t1)
{
  to_return = try(log(y1 / y0) / t1 )
  # if(inherits(to_return, "try-error")) browser()
  return(to_return)
}
