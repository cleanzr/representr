load("inst/extdata/rl_reg1_raw.RData")
identity.rl_reg1 <- seq_along(identity)
identity.rl_reg1[setdiff(identity.rl_reg1, originals_idx)] <- merge(data.frame(id = identity[setdiff(identity.rl_reg1, originals_idx)]), originals_dup, by = "id", all.x = TRUE)$rownum
rl_reg1 <- data[, -10]
save(rl_reg1, file = "data/rl_reg1.RData")
save(identity.rl_reg1, file = "data/identity.rl_reg1.RData")

load("inst/extdata/rl_reg2_raw.RData")
identity.rl_reg2 <- seq_along(identity)
identity.rl_reg2[setdiff(identity.rl_reg2, originals_idx)] <- merge(data.frame(id = identity[setdiff(identity.rl_reg2, originals_idx)]), originals_dup, by = "id", all.x = TRUE)$rownum
rl_reg2 <- data[, -10]
save(rl_reg2, file = "data/rl_reg2.RData")
save(identity.rl_reg2, file = "data/identity.rl_reg2.RData")

load("inst/extdata/rl_reg5_raw.RData")
identity.rl_reg5 <- seq_along(identity)
identity.rl_reg5[setdiff(identity.rl_reg2, originals_idx)] <- merge(data.frame(id = identity[setdiff(identity.rl_reg5, originals_idx)]), originals_dup, by = "id", all.x = TRUE)$rownum
rl_reg5 <- data[, -10]
save(rl_reg5, file = "data/rl_reg5.RData")
save(identity.rl_reg5, file = "data/identity.rl_reg5.RData")
