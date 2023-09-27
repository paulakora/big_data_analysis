.profile$foo <- function(data, indices, cor.type){
  dt <- data[indices,]
  c(
    cor(dt[,1], dt[,2], method = cor.type),
    median(dt[,1]),
    median(dt[,2])
  )
}