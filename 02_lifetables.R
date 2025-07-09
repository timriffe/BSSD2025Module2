
# function based on table in HMD methods protocol
mx_to_ax <- function(mx, sex, age, nx){
  ax <-
    case_when(
      age == 0 & sex == "male" & mx < 0.02300 ~ 0.14929 - 1.99545 * mx,
      age == 0 & sex == "male" & mx >= 0.02300 & mx < 0.08307 ~ 0.02832 + 3.26021 * mx,
      age == 0 & sex == "male" & mx >= 0.08307 ~ 0.29915,
      age == 0 & sex == "female" & mx < 0.01724 ~ 0.14903 - 2.05527 * mx,
      age == 0 & sex == "female" & mx >= 0.01724 & mx < 0.06891 ~ 0.04667 + 3.88089 * mx,
      age == 0 & sex == "female" & mx >= 0.06891 ~ 0.31411,
      age == max(age) ~ 1 / mx,
      TRUE ~ nx / 2)
  return(ax)
}

# standard qx calculation
calc_qx <- function(mx, ax, age, nx = rep(1,length(mx))){
  qx <- nx * mx / (1 + (nx - ax) * mx)
  qx <- if_else(age == max(age), 1, qx)
  return(qx)
}

# cumprod() makes our synthetic cohort :-),
# if radix = 1, this is the probability of surviving from birth to age x
# radix = 1e5 means you have 1e5 lemmings/sims
calc_lx <- function(qx, radix = 1e5){
  n  <- length(qx)
  lx <- c(1, cumprod(1 - qx))
  lx <- lx[1:n]
  lx <- radix * lx
  return(lx)
}

calc_Lx <- function(lx,ax,dx,nx = rep(1,length(lx))){
  n <- length(lx)
  Lx <- nx * lx - (nx - ax) * dx
  Lx[n] <- lx[n] * ax[n]
  return(Lx)
} 

calc_Tx <- function(Lx){
  Tx <- rev(Lx) |> cumsum() |> rev()
}
# a simple lifetable function 
# (too simplified for actual production use)
# but it's good enough for anything we're doing.

lt_full <- function(data, radix = 1, groups){
   out <-
    data |> 
    mutate(
      # new use nx
      nx = c(diff(age),1),
      ax = mx_to_ax(mx = mx, 
                    sex = groups$sex, 
                    age = age,
                    nx = nx),
      qx = calc_qx(mx = mx, 
                   ax = ax, 
                   age = age,
                   nx = nx),
      lx = calc_lx(qx = qx, radix = 1e5),
      dx = lx * qx,
      Lx = calc_Lx(lx = lx,
                   ax = ax,
                   dx = dx,
                   nx = nx),
      Tx = calc_Tx(Lx),
      ex = Tx / lx) 
  return(out)
}
