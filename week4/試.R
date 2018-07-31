dta<-read.table(file="TIMSS2011TW.txt",header = TRUE)

summary(dta)


library(ggplot2)

# coord_flip()橫軸縱軸交換
ggplot(data = dta, aes(x = gender, y = math)) +
  geom_boxplot() + coord_flip() +
  labs( y = 'math', x = 'gender', 
        title = 'Mathematical Score Box')


# 計算信賴區間(μ- Ζα/2 σ , μ+ Ζα/2 σ)

with(dta, 
     tapply(math, gender,
            function(x) 
              c(mean(x) + c(-2, 2) * sd(x)/sqrt(length(x))))) 

t.test(math ~ gender, data = dta)

t.test(math ~ gender, data = dta, var.equal = TRUE)
library(Hmisc)

# 先把父母教育欄位內各個水準順序定下來(order of factors)

parental.education <- factor(parental.education, 
                                 levels = c('elementary school',
                                            'junior high school',
                                            'high school',
                                            'college', 
                                            'university above'))

# 看不同父母教育程度下的數學分數平均數

tapply(dta$math, parental.education, mean)


# 不同父母教育程度下的數學分數平均數，加上信賴區間
ggplot(data = dta, 
       aes(x = parental.education, y = math)) +
  stat_summary(fun.data = 'mean_cl_boot', size = 1) +
  scale_y_continuous(breaks = seq(500, 660, by = 20)) +
  geom_hline(yintercept = mean(dta$math) , 
             linetype = 'dotted') +
  labs(x = '父母教育', y = '數學平均分數') +
  coord_flip()
