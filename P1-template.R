v_p=50 #mph
v_or=0 #mph
a_or_max=3 #ft/s^2
t_sim <- c(1:100)
x_0=2 #mile
sigma_min=20 #seconds
#1mile = 5280 ft ~ 176 m
sigma <- x_0/v_p - v_p/a_or_max