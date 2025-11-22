forLoopRoots <- function(n){
  roots <- rep(NA, n)
  for (i in 1:n) {
    roots[i] <- sqrt(i)
  }
  return(roots)
}

head(forLoopRoots(1000))

SapplyRoots <- function(n){
  roots <- sapply(1:n, sqrt)
  return(roots)
}

head(SapplyRoots(10000))

library(microbenchmark)

microbenchmark(forLoopRoots(10000), SapplyRoots(10000))

# Vectorization
VectorizedRoots <- function(n){
  sqrt(1:n)
}

head(VectorizedRoots(10000))

microbenchmark(forLoopRoots(10000), SapplyRoots(10000), VectorizedRoots(10000))

set.seed(1)
vec <- sample(letters, 100, replace = TRUE)
vec

# Repeated elements: nested loop
method1 <- function(x){
  # first element cannot be a duplicate
  out <- sapply(2:length(x), function(i){
    matches <- sapply(1:(i-1), function(i2){
      x[i2] == x[i]
    })
    return(any(matches))
  })
  out <- c(FALSE, out) #because we start from the second one
  return(out)
}

vec[1:11]

method1(vec)[1:11]


method2 <- function(x){
  # first element cannot be a duplicate
  out <- sapply(2:length(x), function(i){
    return(any(x[1:(i-1)] == x[i]))
  })
  out <- c(FALSE, out)
  return(out)
}

method2(vec)[1:11]

method3 <- function(x){
  duplicated(x)
}

method3(vec)[1:11]

microbenchmark(method1(vec), method2(vec), method3(vec))

# 1. CREATING A CLUSTER
library(parallel)
cl <- makeCluster(4)    
x  <- 20

# 2. PREPARING THE CLUSTER
clusterSetRNGStream(cl, 123) # Equivalent to `set.seed(123)` (not necessary for this example)
clusterExport(cl, "x")

# 3. DO YOUR CALL
clusterEvalQ(cl, {
  paste0("Hello from process #", Sys.getpid(), ". I see x and it is equal to ", x)
})

# 4. STOP THE CLUSTER
stopCluster(cl)

# mclappy functions: output is a list not a vector! no need to mention stopCluster
# 1. SETUP
library(parallel)
x  <- 20

# 2. DO YOUR CALL
mclapply(1:4, function(i){
  paste0("Hello from process #", Sys.getpid(), ". I see x and it is equal to ", x)
}, mc.cores = 4)

## Parallel Regression
ans <- apply(
  X = X,
  MARGIN = 2,
  FUN = function(x) coef(lm(y ~ x))
)

ans[,1:5]

# `parapply`
library(parallel)
cl <- makeCluster(4L)
clusterExport(cl, "y")
ans <- parApply(
  cl = cl,
  X = X,
  MARGIN = 2,
  FUN = function(x) coef(lm(y ~ x))
)

ans[,1:5]


source("~/Downloads/sars-cov2.R", echo=FALSE)
# source("img/sars-cov2.R", echo = FALSE)

# Looking at some constants
probs_sick # Sick individual's probabilities
probs_susc

# Location of who wears the facemask. This step is only for plotting
wears <- which(one$wears, arr.ind = TRUE) - 1
wears <- wears/(one$nr) * (1 + 1/one$nr)

set.seed(7123)
one <- simulate_covid(
  
)

# Initializing the animation
fig   <- magick::image_device(600, 600, res = 96/2, pointsize = 24)
for (i in 1:one$current_step) {
  
  # Plot
  image(
    one$temporal[,,i], col=c("gray", "tomato", "steelblue","black"),
    main = paste("Time", i - 1L, "of", one$nsteps),
    zlim = c(1,4)
  )
  points(wears, col="white", pch=20, cex=1.5)
  legend(
    "topright",
    col = c("gray", "tomato", "steelblue","black", "black"),
    legend = c(names(codes), "wears a mask"),
    pch = c(rep(15, 4), 21)
  )
}

# Finalizing plot and writing the animation
# dev.off()
# animation <- magick::image_animate(fig, fps = 2)
# magick::image_write(animation, "covid1.gif")