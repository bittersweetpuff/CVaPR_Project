fit_and_plot <- function(x, y, name, quadratic = F) {
   #task d - plot
   plot(x, y, main = name)


   if(quadratic) {
      #task g - polinominal regression
      fit <- lm(y ~ x + x^2)
   } else {
      #task e - least-squares linear model
      fit <- lm(y ~ x)
   }
   #printing fit
   print(fit)
   #task f - fitting a least-squares line on the scatter plot
      abline(coefficients(fit), lwd = 2, lty = 2,
      col = "red")
}


#setting seed to 1 for consistant results
set.seed(1)

#task a - create a vector x of random values from N(0,1) distribution
x <- rnorm(100, mean = 0, sd = 1)

#task b - create a vector eps of random values from N(0,0.25) distribution
eps <- rnorm(100, mean = 0, sd = 0.25)
eps2 <- rnorm(100, mean = 0, sd = 0.1)
eps3 <- rnorm(100, mean = 0, sd = 0.4)

#task c - create a vector y = -1 + 0.5x + eps
y <- -1 + (0.5 * x) + eps
y2 <- -1 + (0.5 * x) + eps2
y3 <- -1 + (0.5 * x) + eps3


fit_and_plot(x, y, "Original set")
fit_and_plot(x, y, "Original set (polinominal)", quadratic = T)
fit_and_plot(x, y2, "Set with less noise")
fit_and_plot(x, y3, "Set with more noise")