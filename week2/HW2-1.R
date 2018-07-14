library(ggplot2)
mpg
# 汽車廠牌的統計
ggplot(data = mpg, aes(x = manufacturer )) + geom_bar(fill= "lightblue", colour=("black"))

# 引擎排氣量的統計
ggplot(data = mpg, aes(x = displ)) +
  geom_histogram(binwidth=0.5)

# 各車種的市區/高速油耗表現
ggplot(data = mpg, aes(x = cty, y= hwy , color = class)) +
  geom_point()

# 車種與排氣量的統計
ggplot(mpg, aes(x=class, y = displ)) + 
  geom_boxplot()