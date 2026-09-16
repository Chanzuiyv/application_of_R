library(igraph)
library(plyr)
library(stringr)
library(openxlsx)

setwd("C:/Users/ThinkPad/Desktop/复杂网络")

edge_data <- read.xlsx("全球大豆贸易-2024.xlsx", sheet=1, colNames=TRUE)
country_names <- unique(c(as.character(edge_data[,1]), as.character(edge_data[,2])))

data <- read.xlsx("全球大豆贸易-矩阵-2024.xlsx", sheet=1, colNames=FALSE, rowNames=FALSE)
d <- as.matrix(data)

g <- graph_from_adjacency_matrix(d, mode="max", weighted=TRUE, diag=FALSE)

if(length(country_names) >= vcount(g)) {
  V(g)$name <- country_names[1:vcount(g)]
} else {
  V(g)$name <- paste("Country", 1:vcount(g))
}

n <- vcount(g)
m <- ecount(g)
l <- mean_distance(g, weights = NA)
density <- edge_density(g, loops = FALSE)
c <- transitivity(g)
avg_degree <- mean(degree(g))

cat("=== 网络基础拓扑特征 ===\n")
cat("网络节点总数（参与贸易的国家/地区）：", n, "个\n")
cat("贸易边总数：", m, "条\n")
cat("网络平均路径长度：", round(l, 4), "\n")
cat("网络密度：", round(density, 6), "\n")
cat("全局聚类系数：", round(c, 4), "\n")
cat("平均度：", round(avg_degree, 2), "\n\n")

degree_total <- degree(g, normalized=FALSE)
degree_in <- degree(g, mode="in", normalized=FALSE)
degree_out <- degree(g, mode="out", normalized=FALSE)
strength_total <- strength(g)
strength_in <- strength(g, mode = "in")
strength_out <- strength(g, mode = "out")
betweenness <- betweenness(g, normalized = TRUE)
closeness <- closeness(g, mode="all", normalized = FALSE)
eigenvector <- eigen_centrality(g, scale = FALSE)$vector

closeness <- closeness / max(closeness)

degree_out_rank <- sort(degree_out, decreasing = TRUE)[1:5]
cat("=== 出度排名（出口贸易伙伴数量）Top5 ===\n")
for(i in 1:min(5, length(degree_out_rank))) {
  idx <- which(degree_out == degree_out_rank[i])[1]
  cat("第", i, "名：", V(g)$name[idx], "（出度：", degree_out_rank[i], "）\n")
}
cat("\n")

degree_in_rank <- sort(degree_in, decreasing = TRUE)[1:5]
cat("=== 入度排名（进口贸易伙伴数量）Top5 ===\n")
for(i in 1:min(5, length(degree_in_rank))) {
  idx <- which(degree_in == degree_in_rank[i])[1]
  cat("第", i, "名：", V(g)$name[idx], "（入度：", degree_in_rank[i], "）\n")
}
cat("\n")

strength_out_rank <- sort(strength_out, decreasing = TRUE)[1:5]
cat("=== 加权出度排名（出口贸易额规模）Top5 ===\n")
for(i in 1:min(5, length(strength_out_rank))) {
  idx <- which(strength_out == strength_out_rank[i])[1]
  cat("第", i, "名：", V(g)$name[idx], "（加权出度：", round(strength_out_rank[i], 2), "）\n")
}
cat("\n")

betweenness_rank <- sort(betweenness, decreasing = TRUE)[1:5]
cat("=== 介数中心性排名（贸易中转枢纽控制力）Top5 ===\n")
for(i in 1:min(5, length(betweenness_rank))) {
  idx <- which(betweenness == betweenness_rank[i])[1]
  cat("第", i, "名：", V(g)$name[idx], "（介数：", round(betweenness_rank[i], 4), "）\n")
}
cat("\n")

closeness_rank <- sort(closeness, decreasing = TRUE)[1:5]
cat("=== 接近中心性排名 Top5 ===\n")
for(i in 1:min(5, length(closeness_rank))) {
  idx <- which(closeness == closeness_rank[i])[1]
  cat("第", i, "名：", V(g)$name[idx], "（接近中心性：", round(closeness_rank[i], 4), "）\n")
}
cat("\n")

centrality_df <- data.frame(
  国家 = V(g)$name,
  出度 = degree_out,
  入度 = degree_in,
  加权出度 = strength_out,
  加权入度 = strength_in,
  介数中心性 = betweenness,
  接近中心性 = closeness,
  特征向量中心性 = eigenvector
)

write.csv(centrality_df, "网络中心性分析结果.csv", row.names=FALSE, fileEncoding="UTF-8")
cat("中心性分析结果已保存到：网络中心性分析结果.csv\n")

g