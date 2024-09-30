y2 <- function(y1) {
  if(y1 <= 1 && y1 >= -1) { -y1 }
  else y1
}

y1_values <- seq(-5, 5, by=0.01) # 从-3到3，步长为0.1

# 计算对应的输出值
y2_values <- sapply(y1_values, y2) # 使用sapply应用y2函数

# 绘图
plot(y1_values, y2_values, type='l', col='blue', 
     xlab='y1', ylab='y2', main='Function y2 Plot')
special_points_y1 <- c(-1, 1)
special_points_y2 <- c(-1,1)

# 在特定点添加空心点
points(special_points_y1, special_points_y2, pch=21, col='blue', bg='white')
