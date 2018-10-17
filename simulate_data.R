# simulate the data set

# generate quadratic function flux = f(wavelength)
u = 3543
g = 4770
r = 6231
i = 7625 
z = 9134 
# from http://skyserver.sdss.org/dr2/en/proj/advanced/color/sdssfilters.asp

# generate sample data points
generate_curve = function(n) {
  i=0
  x = c()
  y = c()
  while(i<n) {
    plot(NA, xlim=c(3000, 10000), ylim=c(0,20), 
         xlab="wavelength in anstrogram",
         ylab="flux")
    usr = par("usr")
    loc = locator(1)
    i = i+1
    x = c(x, loc$x)
    y = c(y, loc$y)
  }
  return(data.frame(x,y))
}


# m : the number of curves
# n : the number of clicks to create a curve
# N : the number of data points to generate around each band 
# simg: the standard deviation of the error
simulate_data = function(m, n, N) {
  # initialize a dataframe
  d = t(as.matrix(rep(0,6)))
  for (i in 1:m) {
    print("input data points")
    points = generate_curve(n)
    quadratic = lm(y~I(x^2)+x+1, data=points)
    coefs = coef(summary(quadratic))
    # intercept
    z0 = coefs[1,1]
    # first order coefficient
    z1 = coefs[3,1]
    # second order coefficeint
    z2 = coefs[2,1]
    x = c(3543, 4770, 6231, 7625, 9134)
    f = c(1.81, 3.73, 4.49, 4.76, 4.81)
    y = z0 + z1*x + z2*(x^2) 
    sigma = 0.05 * y
    for (j in 1:N) {
      y_sim = pmax(y + rnorm(5, 0, sigma), 0)
      y_sim = -2.5 * log10(y_sim/f)
      y_sim = c(y_sim, i)
      d = rbind(d, y_sim)
    }
  }
  d = d[-1,]
  d = as.data.frame(d)
  colnames(d) = c("mag_u", "mag_g", "mag_r", "mag_i", "mag_z", "label")
  
  return(d[-1,])
  
}

sim_data = simulate_data(2, 10, 100)
sim_data = as.data.frame(sim_data)
colnames(sim_data)
plot(sim_data$x, sim_data$y)

matplot(y=t(sim_data[,-6]))

# use kmeans to cluster on sim_data
firsttry = kmeans(sim_data, 2)
clusters = firsttry$cluster

#compate clusters with labels
table(clusters, sim_data$label)

plot(sim_data$mag_u, sim_data$mag_g)





