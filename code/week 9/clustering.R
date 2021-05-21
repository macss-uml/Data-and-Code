# Clustering (40800)
# pdwaggoner@uchicago.edu

# Techniques covered: hierarchical, k-means, GMM

# load libraries and set some stuff up
library(tidyverse) # data management / modeling functions
library(patchwork) # mult ggplot
library(ggdendro) # extract dendro data for tidy viz
library(cluster) # fitting clustering algs
library(factoextra) # prettier (tidy) viz options
library(skimr) # for tidy data summaries
library(dbscan) # fitting dbscan alg (if desired)

set.seed(1234)
theme_set(theme_minimal())


#
# Hierarchical clustering

# generate some simulated data with structure
x <- tibble(x1 = rnorm(50) + 3,
            x2 = rnorm(50) - 4,
            y = ifelse(x1 < 3, "1",
                       ifelse(x2 > -4, "2", "3")))

# plot
ggplot(x, aes(x1, x2, color = y)) +
  geom_point() +
  scale_color_brewer(type = "qual", 
                     palette = "Dark2") +
  labs(title = "Simulated data",
       x = expression(X[1]),
       y = expression(X[2])) +
  theme(legend.position = "none")


# compare linkage methods
complete <- hclust(dist(x), 
                   method = "complete")

single <- hclust(dist(x), 
                 method = "single")

average <- hclust(dist(x), 
                  method = "average")

ward <- hclust(dist(x), 
                  method = "ward.D2")

# plot
cl <- ggdendrogram(complete) +
  labs(title = "Complete linkage")

sl <- ggdendrogram(single) +
  labs(title = "Single linkage")

al <- ggdendrogram(average) +
  labs(title = "Average linkage")

wl <- ggdendrogram(average) +
  labs(title = "Ward's linkage")


library(patchwork)

cl + sl /
  al + wl


#
# A little deeper exploration focusing on HAC with *complete* linkage for tree cutting

# fit HAC with complete linkage 
hc_complete <- hclust(dist(x), 
                      method = "complete")

# plot as before
ggdendrogram(hc_complete)

# Recall, that to generate clusters, we make a horizontal cut somewhere on the dendrogram, severing the tree into multiple subtrees. 

# The height of the cut will dictate how many clusters are formed. Note: we are using height here (nidicating a height in measure distance). But you could instead cut for a specific number of clusters by specifying k instead.

# First, try cutting the tree at a height of 4 splits the dendrogram into "subtrees"
h <- 4

# extract dendro data
hcdata <- dendro_data(hc_complete)

hclabs <- label(hcdata) %>%
  left_join(tibble(label = as.factor(seq.int(nrow(x))),
                   cl = as.factor(cutree(hc_complete, h = h))))

# plot dendrogram
ggdendrogram(hc_complete, labels = FALSE) +
  geom_text(data = hclabs,
            aes(label = label, x = x, y = 0, color = cl),
            vjust = .5, hjust = 1, angle = 90) +
  geom_hline(yintercept = h, linetype = 2) +
  scale_color_brewer(type = "qual", palette = "Dark2", guide = FALSE) +
  theme(axis.text.x = element_blank(),
        legend.position = "none")

# How many do you get? (I got 3 clusters)

# now try for h = 3
h <- 3

# extract dendro data
hcdata <- dendro_data(hc_complete)

hclabs <- label(hcdata) %>%
  left_join(tibble(label = as.factor(seq.int(nrow(x))),
                   cl = as.factor(cutree(hc_complete, h = h))))

# plot dendrogram
ggdendrogram(hc_complete, labels = FALSE) +
  geom_text(data = hclabs,
            aes(label = label, x = x, y = 0, color = cl),
            vjust = .5, hjust = 1, angle = 90) +
  geom_hline(yintercept = h, linetype = 2) +
  scale_color_brewer(type = "qual", palette = "Dark2", guide = FALSE) +
  theme(axis.text.x = element_blank(),
        legend.position = "none")


# Now how many did you get cutting the tree a bit lower? (I got 4 now).



#
# k-means clustering

# Recall, k-means will give you however many clusters you search for, whether these are meaningful or not, e.g...

# create some data
# consider removing the previous "x" simulated data from the environment to avoid the warning message using (uncomment the following line): 
# rm(x) # Note though, it will still work if you don't remove x; x is simply redefined

x <- list(
  `1` = MASS::mvrnorm(n = 300, c(-4,10), matrix(c(1.5,1,1,1.5),2)),
  `2` = MASS::mvrnorm(n = 300, c(5,7), matrix(c(1,2,2,6),2)),
  `3` = MASS::mvrnorm(n = 300, c(-1,1), matrix(c(4,0,0,4),2)),
  `4` = MASS::mvrnorm(n = 300, c(10,-10), matrix(c(4,0,0,4),2)),
  `5` = MASS::mvrnorm(n = 300, c(3,-3), matrix(c(4,0,0,4),2))
) %>%
  map_df(as_tibble, .id = "cluster") %>%
  rename(x = V1,
         y = V2)

# estimate k clusters
x_out <- x %>%
  select(-cluster) %>%
  mutate(k2 = kmeans(x, 2, nstart = 20)$cluster,
         k3 = kmeans(x, 3, nstart = 20)$cluster,
         k4 = kmeans(x, 4, nstart = 20)$cluster,
         k5 = kmeans(x, 5, nstart = 20)$cluster,
         k6 = kmeans(x, 6, nstart = 20)$cluster,
         k7 = kmeans(x, 7, nstart = 20)$cluster)

# plot clusters
x_out %>%
  gather(K, pred, k2:k7) %>%
  mutate(K = parse_number(K),
         pred = factor(pred)) %>%
  ggplot(aes(x, y, color = pred)) +
  facet_wrap(~ K, labeller = label_both) +
  geom_point() +
  scale_color_brewer(type = "qual", palette = "Dark2") +
  theme(legend.position = "none")


# Now, re-run multiple versions with different set of random initial centroids; colors don't matter here, rather the actual point assignments are what matters along with changes to WSS. This is basically like starting the algorithm with a different random seed each time.

# First with k = 5
kmean.out <- rerun(6, kmeans(x %>%
                               select(-cluster), 
                             5, 
                             nstart = 1))

withinss <- rep(map_chr(kmean.out, ~ .$tot.withinss), each = nrow(x))

kmean.out %>%
  map_df(~ enframe(.$cluster, name = NULL), .id = "id") %>%
  bind_cols(bind_rows(x, x, x, x, x, x)) %>%
  mutate(withinss = str_c("Within SS = ", withinss),
         id = str_c("Attempt #", id),
         value = factor(value)) %>%
  ggplot(aes(x, y, color = value)) +
  facet_wrap(~ id + withinss, ncol = 3, 
             labeller = label_wrap_gen(multi_line = TRUE)) +
  geom_point() +
  scale_color_brewer(type = "qual", palette = "Dark2", guide = FALSE) +
  labs(title = "Convergence of k-means cluster algorithm",
       x = expression(X[1]),
       y = expression(X[2])) +
  theme(legend.position = "none")

# not much change? 

# now, with k = 3
kmean.out <- rerun(6, kmeans(x %>%
                               select(-cluster), 
                             3, 
                             nstart = 1))

withinss <- rep(map_chr(kmean.out, ~ .$tot.withinss), each = nrow(x))

kmean.out %>%
  map_df(~ enframe(.$cluster, name = NULL), .id = "id") %>%
  bind_cols(bind_rows(x, x, x, x, x, x)) %>%
  mutate(withinss = str_c("Within SS = ", withinss),
         id = str_c("Attempt #", id),
         value = factor(value)) %>%
  ggplot(aes(x, y, color = value)) +
  facet_wrap(~ id + withinss, ncol = 3, labeller = label_wrap_gen(multi_line = TRUE)) +
  geom_point() +
  scale_color_brewer(type = "qual", palette = "Dark2", guide = FALSE) +
  labs(title = "Convergence of k-means cluster algorithm",
       x = expression(X[1]),
       y = expression(X[2])) +
  theme(legend.position = "none")

# Now a bit more; but still not a ton. Though note the poor job of separating the clearly different clusters to the upper part of the plot. Thus, I'd recommend at a minimum k = 4, or possibly 5. 


# Another look at k-means with real data
df <- USArrests %>%
  na.omit %>%
  scale()

head(df)

# inspect reduction in within-cluster variation (sums of squares) over a range of values for k
wss <- function(k) {
  kmeans(df, k, nstart = 10)$tot.withinss
}

tibble(
  k = 1:10
) %>%
  mutate(wss = map_dbl(k, wss)) %>%
  ggplot(aes(k, wss)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 1:10) +
  labs(title = "USArrests",
       x = "Number of clusters K",
       y = "Total within-clusters sum of squares")


# Now inspect k-means quality using average silhouette width

# RECALL: The silhouette value is a measure of how similar an object is to its own cluster (cohesion) compared to other clusters (separation). Silhouette values ranges from −1 to +1, where a high value indicates that the object is well matched to its own cluster and poorly matched to neighboring clusters. If most objects have a high value, then the clustering configuration is "appropriate." If many points have a low or negative value, then the clustering configuration may have too many or too few clusters. Thus, it's useful to use silhouette width in conjunction with other common internal validation metrics like WSS as we previously explored.

# First define a helper function, avg_sil(), to compute average silhouette value for k clusters; makes life a bit faster and easier

avg_sil <- function(k) {
  km.res <- kmeans(df, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(df))
  mean(ss[, 3])
}

tibble(
  k = 2:10
) %>%
  mutate(avg_sil = map_dbl(k, avg_sil)) %>%
  ggplot(aes(k, avg_sil)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 2:10) +
  labs(title = "US Arrests Data",
       x = "Number of clusters K",
       y = "Average silhouette value")


# 
# GMM

# load libraries needed for this section
library(tidyverse) # data management
library(mixtools) # fitting GMM
library(plotmm) # tidy viz of GMM
library(amerika) # color palettes

pres <- read_csv("Data/2012_DVS.csv") %>% 
  mutate(State = X1,
         DVS = dem_vs) %>% 
  select(-c(X1, dem_vs))

head(pres)

# Take a look at the density
ggplot(pres, aes(x = DVS)) +
  geom_density() + 
  xlim(min(pres$DVS) - 10, 
       max(pres$DVS) + 10) +
  theme_minimal() +
  labs(x = "Democratic Vote Share")

# best guess at component means (fig from lecture)
ggplot(pres, aes(x = DVS)) +
  geom_density() + 
  xlim(min(pres$DVS) - 10, 
       max(pres$DVS) + 10) +
  theme_minimal() +
  labs(x = "Democratic Vote Share") +
  geom_vline(xintercept = 41, 
             col = amerika_palettes$Republican[3]) + 
  geom_vline(xintercept = 53, 
             col = amerika_palettes$Democrat[3])

# Start by fitting a two component (cluster) gmm to try and recover and then separate these components from each other. Recall, our assumption is these are parties mixed together. Stick with this assumption throughout the exercise, though it could be wrong as this is an unsupervised exercise. But it will nonetheless help us make sense of the output.

set.seed(7355) 

# fit the gmm
gmm1 <- normalmixEM(pres$DVS, k = 2) 

# viz the overlaid component curves
ggplot(data.frame(x = gmm1$x)) +
  geom_histogram(aes(x, ..density..), alpha = 0.4, fill = "darkgray") +
  stat_function(geom = "line", fun = plot_mix_comps_normal,
                args = list(gmm1$mu[1], gmm1$sigma[1], lam = gmm1$lambda[1]),
                colour = amerika_palettes$Republican[3]) +
  stat_function(geom = "line", fun = plot_mix_comps_normal,
                args = list(gmm1$mu[2], gmm1$sigma[2], lam = gmm1$lambda[2]),
                colour = amerika_palettes$Democrat[3]) +
  xlab("Democratic Vote Shares") +
  ylab("Density") + 
  theme_minimal()

# Interestingly, though the US has two major parties, the gmm doesn't seem to fit too well. Perhaps there is an outlying state out there to the right of the bulk of the distribution. Let's explore with k = 3...

# next attempt

set.seed(7355)

# fit
gmm2 <- normalmixEM(pres$DVS, k = 3)

# viz
ggplot(data.frame(x = gmm2$x)) +
  geom_histogram(aes(x, ..density..), alpha = 0.4, fill = "darkgray") +
  stat_function(geom = "line", fun = plot_mix_comps_normal,
                args = list(gmm2$mu[1], gmm2$sigma[1], lam = gmm2$lambda[1]),
                colour = amerika_palettes$Republican[3]) +
  stat_function(geom = "line", fun = plot_mix_comps_normal,
                args = list(gmm2$mu[2], gmm2$sigma[2], lam = gmm2$lambda[2]),
                colour = amerika_palettes$Democrat[3]) +
  stat_function(geom = "line", fun = plot_mix_comps_normal,
                args = list(gmm2$mu[3], gmm2$sigma[3], lam = gmm2$lambda[3]),
                colour = "black") +
  xlab("Democratic Vote Shares") +
  ylab("Density") + 
  theme_minimal()

# Yeah, that state/obs to the far right definitely seems to be requiring it's component curve, given the flat blue curve when k = 3. 

# Perhaps we can search for and omit it to check out suspicions. Let's try.

# Searching for (potentially) problematic observation, given poor fit of GMM
which(pres$DVS > 80) # it's observation 17, which is Washington D.C.

# now we can try again without outlier, by creating a new data frame minus DC
pres2 <- pres[-c(17), ]

# quickly compare to make sure it worked
{
  withDC <- head(pres$DVS, 20)
withoutDC <- head(pres2$DVS, 20)
head(data.frame(cbind(withDC, withoutDC)), 18)
} # DC is gone; onward!


# Now try the gmm with k = 2 again, but this time with no DC to see if that was the pattern we just explored. 
set.seed(1)

# pull out DVS to make things a bit easier
dvs.nodc <- pres2$DVS

# fit
gmm.nodc <- normalmixEM(dvs.nodc, k = 2)

# viz
ggplot(data.frame(x = gmm.nodc$x)) +
  geom_histogram(aes(x, ..density..), alpha = 0.4, fill = "darkgray", bins = 20) +
  stat_function(geom = "line", fun = plot_mix_comps_normal,
                args = list(gmm.nodc$mu[1], gmm.nodc$sigma[1], lam = gmm.nodc$lambda[1]),
                colour = amerika_palettes$Republican[3]) +
  stat_function(geom = "line", fun = plot_mix_comps_normal,
                args = list(gmm.nodc$mu[2], gmm.nodc$sigma[2], lam = gmm.nodc$lambda[2]),
                colour = amerika_palettes$Democrat[3]) +
  xlab("Democratic Vote Shares") +
  ylab("Density") + 
  theme_minimal()

# Definitely DC is an outlier. For *demo purposes only*, let's take a look at a full GMM exercise, but when dropping DC. In practice, you probably shouldn't just drop outliers. But it's OK for now; we are learning. 


# Now, with our gmm fit, let's explore from a variety of angles, numeric and visual.


# First, you can call specific values from the output. 
# means for each component/cluster
gmm.nodc$mu

# sd's for each component/cluster
gmm.nodc$sigma

# mixing weights for each component/cluster
gmm.nodc$lambda


## Next, let's explore component densities to start to make our way toward imputing substantive meaning in these patterns. 

# Start with a table of posterior probabilities of states belonging to each cluster
posterior <- data.frame(cbind(gmm.nodc$x, gmm.nodc$posterior))
rownames(posterior) <- pres2$State
round(head(posterior, 10), 3) # note: v1 is the original vote share (DVS), and comp.1 is the probability of belonging to component 1, and similarly for component 2 via comp.2

# Now, how many states were clustered in each component?
posterior$component <- ifelse(posterior$comp.1 > 0.3, 1, 2)
table(posterior$component) 

# Finally, let's plot a histogram of these counts to see if the gmm predicted specific states correctly (in comparison to the true election outcome). Note: we still aren't saying anything explicit about party. We will just plot the results of the gmm. Then, we can take a look at the true outcomes to compare. 

ggplot(posterior, aes(x = V1)) + 
  geom_histogram(aes(fill = factor(component)), alpha = 0.4, stat ="bin", binwidth = 3) +
  labs(x = "Democratic Vote Share",
       y = "Count of States",
       title = "Gaussian Mixture Model") +
  scale_fill_manual(values=c(amerika_palettes$Republican[3], 
                             amerika_palettes$Democrat[3]),
                    name="Component",
                    breaks=c("1", "2"),
                    labels=c("1", "2")) +
  geom_vline(xintercept = 50, linetype="solid", 
             color = "black", size=1.2) +
  theme_minimal()

# Indeed, the gmm does a great (perfect) job of "capturing reality," where the predicted component assignments mirror the true election results. Take a look and compare: <https://en.wikipedia.org/wiki/2012_United_States_presidential_election#Results_by_state>

# For fun on your own, try this process but with k-means and see if you get different or similar results. 

##
## EXTRA CODE (if you're interested): Using the DBSCAN algorithm to explore state legislative professionalism

# libraries needed for this section
library(tidyverse) # data management / misc
library(skimr) # for tidy data summaries
library(dbscan) # fitting dbscan alg (if desired)
library(factoextra) # prettier (tidy) viz options

# load professionalism data
load("Data/legprof-components.v1.0.RData")

# First, set up scaled df
st <- x %>% 
  filter(sessid == "2009/10") %>% 
  select(-c(fips, stateabv, sessid, mds1, mds2, year)) %>%
  na.omit(st); skim(st)

st_scale <- data.frame(scale(st[,2:5]))

states <- st$state # save state names for plot

# read as matrix for dbscan fit
st_scale <- data.frame(scale(st[,2:5])) %>% 
  rename(`Total Length` = t_slength,
         `Regular Length` = slength,
         `Salary` = salary_real,
         `Expenditures` = expend) %>% 
  as.matrix()


# first, determine the optimal epsilon value

# Epsilon is the maximum distance between two points, which informs clustering. The kNNdistplot() function plots the average distance between every point and its nearest neighbors, and are arranged in ascending order. Though a bit ambiguous, we are looking for an "elbow"/"knee" or a sharp change in average distance, suggesting distance between points is growing, thus signaling the possibility of outliers, which informs the threshold for the neighborhood size.

kNNdistplot(st_scale, 
            k = 4) # number of nearest neighbors; try altering this to see differences
abline(h = 1.2, # looks like around 1.2
       col = "red")

# run the algorithm
dbscan_fit <- dbscan(st_scale, 
                     eps = 1.2, 
                     minPts = 4)

# Visualize all features: visualize across all raw values
pairs(st_scale, 
      col = ifelse(dbscan_fit$cluster == 1, "#F8766D", "#00BFC4"), 
      pch = 19)

# note: blue points/states are treated as outliers/noise - this is at the heart of the difference in the DBSCAN approach to clustering

# Visualize cluster assignment
rownames(st_scale) <- st$state

fviz_cluster(dbscan_fit, st_scale, 
             repel = TRUE,
             show.clust.cent = FALSE,
             outlier.color = "#00BFC4", 
             labelsize = 7,
             pointsize = 1.5, 
             main = "Cluster Assignments from DBSCAN Algorithm") +
  theme_minimal()


# Interesingly, states with the most "professional" legislatures are treated as "noise"/outliers, compared to all other state legislatures, which are clustered together. This is different from how other clustering algorithms would treat these observations, which would be as their own cluster instead of leaving them unclustered as in DBSCAN. Take a look at the examples in my book (the reading for today) to explore these other algorithms with these state legislatures data to compare.
