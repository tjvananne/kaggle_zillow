


## capture messages and errors to a file.
# zz <- file("all.Rout", open="wt")
zz <- file("all_msg.txt", open="wt")
sink(zz, type="message")

try(log("a"))

## reset message sink and close the file connection
sink(type="message")
close(zz)

## Display the log file
readLines("all.Rout")
# [1] "Error in log(\"a\") : Non-numeric argument to mathematical function"
