library(igraph)
library(ggplot2)
library(dplyr)
library(tidyr)
library(openxlsx)

setwd("C:/Users/ThinkPad/Desktop/复杂网络")

generate_network <- function(file_path = "全球大豆贸易-矩阵-2024.xlsx") {
  edge_data <- read.xlsx("全球大豆贸易-2024.xlsx", sheet=1, colNames=TRUE)
  country_names <- unique(c(as.character(edge_data[,1]), as.character(edge_data[,2])))
  
  data <- read.xlsx(file_path, sheet=1, colNames=FALSE, rowNames=FALSE)
  d <- as.matrix(data)
  d[d > 0] <- 1
  g <- graph_from_adjacency_matrix(d, mode="max", weighted=TRUE, diag=FALSE)
  
  if(length(country_names) >= vcount(g)) {
    V(g)$name <- country_names[1:vcount(g)]
  } else {
    V(g)$name <- paste("Country", 1:vcount(g))
  }
  
  return(g)
}

assess_robustness <- function(g, removal_order, steps = 20) {
  n_nodes <- vcount(g)
  step_size <- ceiling(n_nodes / steps)
  
  results <- data.frame(
    step = 0:steps,
    removed_nodes = NA,
    lcc_size = NA,
    lcc_ratio = NA
  )
  
  lcc_initial <- max(components(g)$csize)
  results[1, "removed_nodes"] <- 0
  results[1, "lcc_size"] <- lcc_initial
  results[1, "lcc_ratio"] <- 1.0
  
  for (step in 1:steps) {
    nodes_to_remove <- removal_order[1:min(step * step_size, n_nodes)]
    
    g_temp <- delete_vertices(g, nodes_to_remove)
    
    if(vcount(g_temp) == 0) {
      lcc_size <- 0
    } else {
      comp <- components(g_temp)
      lcc_size <- max(comp$csize)
    }
    
    results[step + 1, "removed_nodes"] <- length(nodes_to_remove)
    results[step + 1, "lcc_size"] <- lcc_size
    results[step + 1, "lcc_ratio"] <- lcc_size / lcc_initial
  }
  
  return(results)
}

generate_attack_strategies <- function(g) {
  n_nodes <- vcount(g)
  
  random_order <- sample(1:n_nodes, n_nodes)
  
  degree_order <- order(degree(g), decreasing = TRUE)
  
  betweenness_order <- order(betweenness(g), decreasing = TRUE)
  
  closeness_order <- order(closeness(g), decreasing = TRUE)
  
  eigen_order <- order(eigen_centrality(g)$vector, decreasing = TRUE)
  
  return(list(
    random = random_order,
    degree = degree_order,
    betweenness = betweenness_order,
    closeness = closeness_order,
    eigenvector = eigen_order
  ))
}

analyze_network_robustness <- function(g, steps = 20) {
  attack_strategies <- generate_attack_strategies(g)
  
  all_results <- list()
  
  for (strategy_name in names(attack_strategies)) {
    cat("正在评估策略:", strategy_name, "\n")
    
    results <- assess_robustness(
      g = g,
      removal_order = attack_strategies[[strategy_name]],
      steps = steps
    )
    results$strategy <- strategy_name
    all_results[[strategy_name]] <- results
  }
  
  combined_results <- do.call(rbind, all_results)
  rownames(combined_results) <- NULL
  
  return(list(
    detailed_results = combined_results,
    attack_orders = attack_strategies
  ))
}

plot_robustness_curves <- function(results_df) {
  ggplot(results_df, aes(x = removed_nodes / max(results_df$removed_nodes), 
                         y = lcc_ratio, 
                         color = strategy, 
                         group = strategy)) +
    geom_line(size = 1.2) +
    geom_point(size = 2) +
    labs(
      title = "网络鲁棒性分析：不同攻击策略下最大连通子图变化",
      x = "移除节点比例",
      y = "最大连通子图相对大小",
      color = "攻击策略"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "right",
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.title = element_text(size = 12)
    ) +
    scale_color_brewer(palette = "Set1") +
    scale_x_continuous(labels = scales::percent) +
    ylim(0, 1)
}

identify_critical_nodes <- function(g, top_n = 10) {
  centrality_metrics <- data.frame(
    node = V(g)$name,
    degree = degree(g),
    betweenness = betweenness(g),
    closeness = closeness(g),
    eigenvector = eigen_centrality(g)$vector
  )
  
  centrality_metrics[, -1] <- apply(centrality_metrics[, -1], 2, 
                                    function(x) x / max(x))
  
  weights <- c(0.3, 0.3, 0.2, 0.2)
  centrality_metrics$combined_score <- as.matrix(centrality_metrics[, 2:5]) %*% weights
  
  critical_nodes <- centrality_metrics %>%
    arrange(desc(combined_score)) %>%
    head(top_n)
  
  return(list(
    critical_nodes = critical_nodes,
    all_metrics = centrality_metrics
  ))
}

calculate_robustness_metrics <- function(results_df) {
  robustness_metrics <- results_df %>%
    group_by(strategy) %>%
    summarize(
      R50 = lcc_ratio[which.min(abs(removed_nodes - 
                                      max(removed_nodes)/2))],
      R20 = lcc_ratio[which.min(abs(removed_nodes - 
                                      max(removed_nodes)*0.2))],
      R80 = lcc_ratio[which.min(abs(removed_nodes - 
                                      max(removed_nodes)*0.8))],
      AUC = sum(diff(c(0, removed_nodes/max(removed_nodes))) * 
                  (lcc_ratio[-1] + lcc_ratio[-length(lcc_ratio)])/2),
      .groups = "drop"
    )
  
  return(robustness_metrics)
}

main_example <- function() {
  set.seed(123)
  
  cat("构建网络...\n")
  g <- generate_network()
  
  cat("网络节点数:", vcount(g), "\n")
  cat("网络边数:", ecount(g), "\n")
  cat("网络直径:", diameter(g), "\n")
  cat("平均路径长度:", average.path.length(g), "\n")
  cat("聚类系数:", transitivity(g), "\n")
  
  cat("\n进行鲁棒性分析...\n")
  analysis_results <- analyze_network_robustness(g, steps = 20)
  
  cat("\n生成鲁棒性曲线...\n")
  p <- plot_robustness_curves(analysis_results$detailed_results)
  print(p)
  
  cat("\n识别关键节点...\n")
  critical_nodes <- identify_critical_nodes(g, top_n = 10)
  print(critical_nodes$critical_nodes)
  
  robustness_summary <- calculate_robustness_metrics(
    analysis_results$detailed_results
  )
  cat("\n鲁棒性综合评价指标:\n")
  print(robustness_summary)
  
  cat("\n=== 报告关键数据提取 ===\n")
  random_r50 <- robustness_summary$R50[robustness_summary$strategy == "random"]
  degree_r50 <- robustness_summary$R50[robustness_summary$strategy == "degree"]
  cat("随机故障策略 R50 (移除50%节点时最大连通子图相对大小):", round(random_r50, 4), "\n")
  cat("蓄意攻击策略 R50 (按度值从高到低移除节点):", round(degree_r50, 4), "\n")
  
  write.csv(robustness_summary, "鲁棒性分析结果.csv", row.names=FALSE, fileEncoding="UTF-8")
  write.csv(critical_nodes$critical_nodes, "关键节点识别结果.csv", row.names=FALSE, fileEncoding="UTF-8")
  write.csv(analysis_results$detailed_results, "鲁棒性详细数据.csv", row.names=FALSE, fileEncoding="UTF-8")
  
  cat("\n分析结果已保存到CSV文件\n")
  
  par(mfrow = c(2, 3))
  
  plot(g, 
       vertex.size = 5, 
       vertex.label = NA,
       main = "原始网络",
       layout = layout_with_fr)
  
  strategies <- c("random", "degree", "betweenness", 
                  "closeness", "eigenvector")
  
  for (strategy in strategies) {
    nodes_to_remove <- analysis_results$attack_orders[[strategy]][1:ceiling(vcount(g)*0.2)]
    g_attacked <- delete_vertices(g, nodes_to_remove)
    
    plot(g_attacked, 
         vertex.size = 5, 
         vertex.label = NA,
         main = paste(strategy, "攻击后"),
         layout = layout_with_fr)
  }
  par(mfrow = c(1, 1))
  
  return(list(
    graph = g,
    analysis = analysis_results,
    critical_nodes = critical_nodes,
    robustness_metrics = robustness_summary
  ))
}

if (TRUE) {
  example_results <- main_example()
}