library(igraph)
library(openxlsx)

setwd("C:/Users/ThinkPad/Desktop/复杂网络")

edge_data <- read.xlsx("C:/Users/ThinkPad/Desktop/复杂网络/数据/全球大豆贸易-2024.xlsx", sheet=1, colNames=TRUE)
country_names <- unique(c(as.character(edge_data[,1]), as.character(edge_data[,2])))

data <- read.xlsx("C:/Users/ThinkPad/Desktop/复杂网络/数据/全球大豆贸易-矩阵-2024.xlsx", sheet=1, colNames=FALSE, rowNames=FALSE)
d <- as.matrix(data)
g <- graph_from_adjacency_matrix(d, mode="max", weighted=TRUE, diag=FALSE)

if(length(country_names) >= vcount(g)) {
  V(g)$name <- country_names[1:vcount(g)]
} else {
  V(g)$name <- paste("Country", 1:vcount(g))
}

cat("网络信息:\n")
cat("节点数:", vcount(g), "\n")
cat("边数:", ecount(g), "\n")
cat("平均度:", mean(degree(g)), "\n\n")

beta <- 0.3
gamma <- 0.1
t_max <- 50
seeds_num <- 3

run_sir <- function(g, seeds, beta=0.3, gamma=0.1, t_max=50) {
  N <- vcount(g)
  node_status <- rep(0, N)
  node_status[seeds] <- 1
  
  S_count <- rep(0, t_max)
  I_count <- rep(0, t_max)
  R_count <- rep(0, t_max)
  
  for(t in 1:t_max) {
    new_infected <- c()
    new_recovered <- c()
    
    current_infected <- which(node_status == 1)
    
    for(i in current_infected) {
      neighbors <- neighbors(g, i)
      susceptible_neighbors <- neighbors[node_status[neighbors] == 0]
      
      if(length(susceptible_neighbors) > 0) {
        infection_attempt <- runif(length(susceptible_neighbors)) < beta
        newly_infected <- susceptible_neighbors[infection_attempt]
        new_infected <- c(new_infected, newly_infected)
      }
      
      if(runif(1) < gamma) {
        new_recovered <- c(new_recovered, i)
      }
    }
    
    new_infected <- unique(new_infected)
    new_recovered <- unique(new_recovered)
    
    node_status[new_infected] <- 1
    node_status[new_recovered] <- 2
    
    S_count[t] <- sum(node_status == 0)
    I_count[t] <- sum(node_status == 1)
    R_count[t] <- sum(node_status == 2)
    
    if(I_count[t] == 0) {
      S_count <- S_count[1:t]
      I_count <- I_count[1:t]
      R_count <- R_count[1:t]
      break
    }
  }
  
  return(list(S=S_count, I=I_count, R=R_count))
}

cat("=== 实验1：随机选取节点初始感染（随机免疫对照组）===\n")
set.seed(123)
random_seeds <- sample(1:vcount(g), seeds_num)
cat("初始感染节点:", V(g)$name[random_seeds], "\n")
random_result <- run_sir(g, random_seeds, beta, gamma, t_max)

cat("\n随机感染传播结果:\n")
cat("传播峰值感染节点数:", max(random_result$I), "\n")
cat("最终感染节点占比:", (sum(random_result$R) + tail(random_result$I, 1))/vcount(g), "\n")

cat("\n=== 实验2：核心节点初始感染（巴西、美国、中国作为初始感染源）===\n")
strength_out <- strength(g, mode="out")
top_exporters <- names(sort(strength_out, decreasing=TRUE)[1:3])
cat("核心出口节点:", top_exporters, "\n")

core_seeds <- which(V(g)$name %in% top_exporters)
if(length(core_seeds) < seeds_num) {
  core_seeds <- c(core_seeds, sample(setdiff(1:vcount(g), core_seeds), seeds_num - length(core_seeds)))
}
core_seeds <- core_seeds[1:seeds_num]
cat("实际初始感染节点:", V(g)$name[core_seeds], "\n")

core_result <- run_sir(g, core_seeds, beta, gamma, t_max)

cat("\n核心节点感染传播结果:\n")
cat("传播峰值感染节点数:", max(core_result$I), "\n")
cat("最终感染节点占比:", (sum(core_result$R) + tail(core_result$I, 1))/vcount(g), "\n")

cat("\n=== SIR传播动力学模拟对比总结 ===\n")
cat("随机感染峰值:", max(random_result$I), "| 核心节点感染峰值:", max(core_result$I), "\n")
cat("随机感染最终占比:", round((sum(random_result$R) + tail(random_result$I, 1))/vcount(g), 4), 
    "| 核心节点感染最终占比:", round((sum(core_result$R) + tail(core_result$I, 1))/vcount(g), 4), "\n")

par(mfrow = c(1,2))
times_random <- 1:length(random_result$S)
plot(times_random, random_result$S, type = "l", col = "blue", lwd = 2, 
     xlab = "时间", ylab = "数量", ylim = c(0, vcount(g)),
     main = "随机初始感染 SIR传播曲线")
lines(times_random, random_result$I, col = "red", lwd = 2)
lines(times_random, random_result$R, col = "green", lwd = 2)
legend("right", legend = c("易感者(S)", "感染者(I)", "恢复者(R)"),
       col = c("blue", "red", "green"), lty = 1, lwd = 2, cex=0.7)

times_core <- 1:length(core_result$S)
plot(times_core, core_result$S, type = "l", col = "blue", lwd = 2, 
     xlab = "时间", ylab = "数量", ylim = c(0, vcount(g)),
     main = "核心节点初始感染 SIR传播曲线")
lines(times_core, core_result$I, col = "red", lwd = 2)
lines(times_core, core_result$R, col = "green", lwd = 2)
legend("right", legend = c("易感者(S)", "感染者(I)", "恢复者(R)"),
       col = c("blue", "red", "green"), lty = 1, lwd = 2, cex=0.7)

par(mfrow = c(1,1))

avg_degree <- mean(degree(g))
R0_random <- beta * avg_degree / gamma
cat("\n基本再生数 R0 =", round(R0_random, 2), "\n")

result_df <- data.frame(
  实验类型 = c("随机感染", "核心节点感染"),
  峰值感染数 = c(max(random_result$I), max(core_result$I)),
  最终感染占比 = c(
    (sum(random_result$R) + tail(random_result$I, 1))/vcount(g),
    (sum(core_result$R) + tail(core_result$I, 1))/vcount(g)
  ),
  传播时长 = c(length(random_result$S), length(core_result$S))
)

write.csv(result_df, "SIR传播模拟结果.csv", row.names=FALSE, fileEncoding="UTF-8")
cat("\nSIR传播模拟结果已保存到：SIR传播模拟结果.csv\n")