
partab <- function() {
  x <- rbinom(5, 1, 0.5)
  return(sum(x))
}

results <- replicate(100, {
  sum_sheers <- partab()
  if (sum_sheers == 4) { 
    return("مساوی 4")
  } else if (sum_sheers >= 3) {
    return("بزرگ‌تر یا مساوی 3")
  } else if (sum_sheers == 0) {
    return("مساوی 0")
  } else {
    return("بازنده")
  }
})

num_greater_than_4 <- sum(results == "مساوی 4")
num_greater_equal_3 <- sum(results == "بزرگ‌تر یا مساوی 3")
num_equal_0 <- sum(results == "مساوی 0")

prob_greater_than_4 <- num_greater_than_4 / 100
prob_greater_equal_3 <- num_greater_equal_3 / 100
prob_equal_0 <- num_equal_0 / 100

cat("احتمال مساوی 4:", prob_greater_than_4, "\n")
cat("احتمال بزرگ‌تر یا مساوی 3:", prob_greater_equal_3, "\n")
cat("احتمال مساوی 0:", prob_equal_0, "\n")

#سوال 2/////////////////////////////////////
func_estimate <- function() {
  Y <- mean(sample(1:6,2, replace = TRUE))
  return(Y)
}
res <- replicate(1000, func_estimate())
winA <- 0
winB <- 0
winC <- 0
for (i in res) {
  if (i < 5) {
    winA <- winA + 1
  }
  if (i == 1) {
    winB <- winB + 1
  }
  if (i <= 2) {
    winC <- winC + 1
  }
}
cat(" A:", winA / 1000, '\n', "B:", winB / 1000, '\n', "C:", winC / 1000, '\n')
#سوال 3//////////////////////////////////////////
bag <- c(rep(1, 5), rep(2, 7), rep(3, 12))
take1 <- function(){
  x <- sample(bag, 2, replace = T)
  if(x[1]==x[2] && x[1] == 1){
    return(1)
  }
  return(0)
}
win <- replicate(1000,take1(),simplify = TRUE)
sum(win)/1000
take2 <- function(){
  x <- sample(bag, 3, replace = T)
  for(i in 1:3){
    for(j in 1:3){
      if(i!=j){
        return(0)
      }
    }
  }
  return(1)
}
win <- replicate(1000,take2(),simplify = TRUE)
sum(win)/1000
