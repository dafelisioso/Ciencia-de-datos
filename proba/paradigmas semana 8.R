library(ggplot2)
library(scales)
library(RColorBrewer)
str(mpg)
head(mpg,10)
ggplot(mpg) +
  geom_bar(aes(x=class),fill='blue') +xlab('tipo de auto')+
  ylab('total')


ggplot(mpg,aes(x=displ))+
  geom_histogram(aes(y=..density..),binwidth=1,fill='blue')+
  geom_line(stat = 'density',col='gray50',size=1.5)+
  xlab('desplazamiento del motor') +
  ylab('densidad')



ggplot(mpg,aes(x=displ))+
  geom_histogram(aes(y=..density..),binwidth = 1,fill='blue')+
  geom_line(stat='density',col='gray50',size=1.5,bw='SJ')+
  xlab('motor')+
  ylab('desplazamiento del motor')



#grafica cajas
ggplot(data=mpg,aes(class,cty))+
  geom_boxplot(fill='aliceblue')+
  labs(title='box plot',x='tipos de auto',y='rendimiento')

#grafica violín

ggplot(data=mpg,aes(class,cty ))+
  geom_violin(fill='aliceblue') +
  labs(title='violin plot',x='tipos de auto',y='rendimiento')
  
library(tidyverse)
mpg_summary <- mpg %>%
  group_by(class) %>%
  summarize(mean_cty=mean(cty),
            sd_cty=sd(cty),
            n_cty=n(),
            se=sd_cty/sqrt(n_cty),
            x_max=mean_cty +se,
            x_min =mean_cty-se
  )
mpg_summary$class <- reorder(mpg_summary$class,mpg_summary$mean_cty)

ggplot(mpg_summary,aes(mean_cty))
