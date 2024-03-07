antigen_decay_curve = function(t, b0, mu_b, y0, mu_y, gamma)
{
  bt_active = b0 * exp(mu_b * t) - (gamma * y0 * (exp(mu_y * t) - exp(mu_b*t)) / (mu_y - mu_b))

  bt = pmax(0, bt_active)
}
