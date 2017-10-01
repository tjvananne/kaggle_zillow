

# https://stackoverflow.com/questions/4560459/all-levels-of-a-factor-in-a-model-matrix-in-r

testFrame <- data.frame(First=sample(1:10, 20, replace=T),
                        Second=sample(1:20, 20, replace=T), Third=sample(1:10, 20, replace=T),
                        Fourth=rep(c("Alice","Bob","Charlie","David"), 5),
                        Fifth=rep(c("Edward","Frank","Georgia","Hank","Isaac"),4))

testFrame


lapply(testFrame[,4:5], contrasts, contrasts = FALSE)

model.matrix(~ ., data=testFrame, 
             contrasts.arg = lapply(testFrame[,4:5], contrasts, contrasts=FALSE))


model.matrix(~ ., data=testFrame, 
             contrasts.arg = lapply(testFrame[,sapply(testFrame, is.factor)], contrasts, contrasts=FALSE))
sapply(joined, class)

