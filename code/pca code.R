# MACS 40800
# Philip Waggoner

# PCA

# load some libraries
library(tidyverse)
library(here)
library(skimr)
library(GGally)
library(tictoc)
library(factoextra)

# read in data
choc_data <- read_csv(here("data", "chocolates.csv"))

# select key features
chocolate <- choc_data[ ,4:14] %>% 
  as_tibble()

chocolate$Type <- as.factor(chocolate$Type)

# take a look at the data
skim(chocolate)

ggpairs(chocolate, mapping = aes(col = Type)) + 
  theme_minimal()

# fit the PCA model
pca_fit <- select(chocolate, -Type) %>%
  scale() %>% 
  prcomp(); summary(pca_fit)

# PC loadings by hand
map_dfc(1:10, ~ pca_fit$rotation[, .] * sqrt(pca_fit$sdev^2)[.]) 

# Or simply extract...
pca_out <- get_pca(pca_fit)
print(pca_out$coord)

# viz: scree
fviz_screeplot(pca_fit, addlabels = TRUE, choice = "variance")

# viz: biplot
fviz_pca_biplot(pca_fit, label = "var")

# viz: loadings
fviz_pca_var(pca_fit)

# customized plot
data_for_plot <- chocolate %>%
  mutate(PC1 = pca_fit$x[, 1], 
         PC2 = pca_fit$x[, 2])

data_for_plot %>% 
  ggplot(aes(PC1, PC2, col = Type)) +
  geom_point() +
  stat_ellipse() +
  labs(title = "PCA Solution",
       subtitle = "Coloring by Chocolate Type",
       x = "PC1",
       y = "PC2") +
  theme_minimal()


# is scaling really *that* big of a deal?
pca_un <- select(chocolate, -Type) %>%
  prcomp(scale = FALSE)

summary(pca_un)

fviz_pca_biplot(pca_un, label = "var")

fviz_screeplot(pca_un, 
               addlabels = TRUE, 
               choice = "variance")

# 


##
## On your own if you're interested: calculating eigen vectors by hand, step by step 

# Suppose we had the following cartesian coordinates for two features, X1 and X2, for our first observation: `(-1, 0.5)`. To find the linear equation for the line, then, we proceed in three steps. 

# 1. Substitute those point values in the Pythagorean theorem ($a^2 + b^2 = c^2$): `((-1)^2) + ((0.5)^2)`. The reason for this is we want to calculate the linear equation for our line/PC. But we don't have an intercept, so we have to do it on the basis of our input feature values alone as discussed in class. Doing this, then, gives a value of = `1.25`

# 2. Take the square root to get c (not c^2): 
sqrt(((-1)^2) + ((0.5)^2)) # which gives `1.118034`.

# 2.1 You can double check your math by writing a simple function:

p <- function(a, b){
  sqrt(a^2 + b^2)
}

p(-1, 0.5)

# 3. Then, divide each coordinate value by `c` (1.11) to allow the normalized value of `c` to equal 1, which is the distance to the origin.

(-1)/1.118034 # a = -0.8944272
(0.5)/1.118034 # b = 0.4472136

# These eigen vectors give the *linear* equation for our line, $-0.8944272 \times X1 + 0.4472136 \times X2$

# To check this, we can do the same thing for some other projected point on this line, where the solution should return the value of the hypotenuse (side c), yet the sign of which will indicate the direction of the line (which, again, is what we are interested in finding when we calculate eigen vectors). 

# Suppose, our second observation is defined by (2, -0.68). Before we start, as a baseline, let's plug in the `X1` and `X2` coordinates from our new set into our formula that we previously defined.

(-0.8944272)*(2) + (0.4472136)*(-0.68) # -2.09296

# Now, we can follow the same steps as before, giving a new set of weights, but still projecting the point onto the same PC.

# 1. Compute $a^2 + b^2$
((2)^2) + ((-0.68)^2) 

# 2. Calculate the hypotenuse, and this time store this in object `c` to make life a little easier. 
p <- function(a, b){
  sqrt(a^2 + b^2)
}

(c <- p(2, -0.68)) # = 2.11

# 3. Divide each coordinate by c to get the "parameter" values/weights for the line equation.
2/c # a
(-0.68)/c # b

# This then gives the new formula: $0.9467727 \times X1 + -0.3219027 \times X2$. 

# Now, we can test it out with our first set of coordinates (-1, 0.5). 

0.9467727*(-1) + (-0.3219027)*0.5 # = -1.11 (which, recall, was the value of c from the first set of points above)

# Great! It worked such that both values from both equations for different projected points on the line indicate the slope of the PC is negative given the two negative values we calculated (-2.10 and -1.11). And these values are the PC scores for these two points, which are projected onto the same PC.

# The take away message here is that we can calculate the slope/direction of the PC by hand (which in this simple case is negative) with the vector of these normalized distances (eigen vectors), as well as the PC scores, using the equation for the PC, all using simple algebra.
