load("inst/extdata/rl_reg1_raw.RData")
identity.rl_reg1 <- seq_along(identity)
for(i in seq_along(identity)) {
  find_rownum <- which(originals_dup$id == identity[i])
  if(length(find_rownum > 0)) identity.rl_reg1[i] <- originals_dup[find_rownum, ]$rownum
}
rl_reg1 <- data[, -10]
save(rl_reg1, file = "data/rl_reg1.RData")
save(identity.rl_reg1, file = "data/identity.rl_reg1.RData")

load("inst/extdata/rl_reg2_raw.RData")
identity.rl_reg2 <- seq_along(identity)
for(i in seq_along(identity)) {
  find_rownum <- which(originals_dup$id == identity[i])
  if(length(find_rownum > 0)) identity.rl_reg2[i] <- originals_dup[find_rownum, ]$rownum
}
rl_reg2 <- data[, -10]
save(rl_reg2, file = "data/rl_reg2.RData")
save(identity.rl_reg2, file = "data/identity.rl_reg2.RData")

load("inst/extdata/rl_reg5_raw.RData")
identity.rl_reg5 <- seq_along(identity)
for(i in seq_along(identity)) {
  find_rownum <- which(originals_dup$id == identity[i])
  if(length(find_rownum > 0)) identity.rl_reg5[i] <- originals_dup[find_rownum, ]$rownum
}
rl_reg5 <- data[, -10]
save(rl_reg5, file = "data/rl_reg5.RData")
save(identity.rl_reg5, file = "data/identity.rl_reg5.RData")
