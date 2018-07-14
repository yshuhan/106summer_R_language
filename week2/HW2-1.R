library(ggplot2)
mpg
# �T���t�P���έp
ggplot(data = mpg, aes(x = manufacturer )) + geom_bar(fill= "lightblue", colour=("black"))

# �����Ʈ�q���έp
ggplot(data = mpg, aes(x = displ)) +
  geom_histogram(binwidth=0.5)

# �U���ت�����/���t�o�Ӫ��{
ggplot(data = mpg, aes(x = cty, y= hwy , color = class)) +
  geom_point()

# ���ػP�Ʈ�q���έp
ggplot(mpg, aes(x=class, y = displ)) + 
  geom_boxplot()