# =====================================================
# 一键导出所有节点中心性指标到Excel（修正版）
# =====================================================

# 加载包
library(igraph)
library(openxlsx)

# 【⚠️ 唯一需要你修改的地方】
# 把下面这一行的路径，改成你电脑上“全球大豆贸易-矩阵-2024.xlsx”的实际位置
matrix_path <- "C:/Users/ThinkPad/Desktop/复杂网络/数据/全球大豆贸易-矩阵-2024.xlsx"

# 【⚠️ 新增：国家名称对照表的路径】
# 改成你电脑上“全球大豆贸易-2024.xlsx”的实际位置（这个文件有Sheet3）
country_path <- "C:/Users/ThinkPad/Desktop/复杂网络/数据/全球大豆贸易-2024.xlsx"

# =====================================================
# 1. 读取矩阵数据并构建网络
# =====================================================
data <- read.xlsx(matrix_path, sheet = 1, colNames = FALSE, rowNames = FALSE)
d <- as.matrix(data)
g <- graph_from_adjacency_matrix(d, weighted = TRUE)

# 读取国家名称对照表（从另一个文件的 Sheet3 读取）
country_names <- read.xlsx(country_path, sheet = 3, colNames = FALSE)
country_labels <- country_names[, 2]  # 第二列是国家英文名

# =====================================================
# 2. 计算所有节点指标
# =====================================================

# 基本信息
n_nodes <- vcount(g)
n_edges <- ecount(g)

# 度中心性
deg <- degree(g, mode = "all")
deg_in <- degree(g, mode = "in")
deg_out <- degree(g, mode = "out")

# 加权度
str_all <- strength(g, mode = "all")
str_in <- strength(g, mode = "in")
str_out <- strength(g, mode = "out")

# 介数中心性（归一化）
bet <- betweenness(g, normalized = TRUE)

# 接近中心性（出接近）
clo <- closeness(g, mode = "out", normalized = TRUE)

# 聚类系数（局部）
clus <- transitivity(g, type = "local", isolates = "zero")

# 特征向量中心度
eig <- eigen_centrality(g, scale = TRUE)$vector

# =====================================================
# 3. 合并成一张总表
# =====================================================

result_table <- data.frame(
  编号 = 1:n_nodes,
  国家 = country_labels[1:n_nodes],
  度 = round(deg, 2),
  入度 = round(deg_in, 2),
  出度 = round(deg_out, 2),
  加权度 = round(str_all, 0),
  加权入度 = round(str_in, 0),
  加权出度 = round(str_out, 0),
  介数中心性 = round(bet, 6),
  接近中心性 = round(clo, 6),
  聚类系数 = round(clus, 6),
  特征向量中心度 = round(eig, 6)
)

# 按介数中心性从大到小排序
result_table_sorted <- result_table[order(-result_table$介数中心性), ]

# =====================================================
# 4. 导出到Excel
# =====================================================

output_path <- "C:/Users/ThinkPad/Desktop/复杂网络/数据/节点中心性完整指标表.xlsx"
write.xlsx(result_table_sorted, output_path, rowNames = FALSE)

# 打印完成信息
cat("✅ 导出成功！文件已保存到：", output_path, "\n")
cat("共导出", nrow(result_table), "个国家的数据。\n")

# =====================================================
# 5. 在控制台打印Top10（方便直接复制）
# =====================================================
cat("\n========== Top 10 介数中心性 ==========\n")
print(result_table_sorted[1:10, c("国家", "介数中心性", "度", "加权度")])