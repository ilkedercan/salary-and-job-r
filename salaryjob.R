**Veri setine erişmek için [data-job-salary](https://www.kaggle.com/datasets/milanvaddoriya/data-science-job-salary) sitesini ziyaret edebilirsiniz.**
  
  # 1.Kütüphanelerin yüklenmesi

library(ggplot2)
library(tidyverse)
library(lubridate)
library(readxl)
library(funModeling)
library(gridExtra)
library(magrittr)
library(scales)
library(plotrix)
library(RColorBrewer)
library(readr)
library(maps)
library(highcharter)
library(dplyr)
library(tidyverse)
library(magrittr)
library(DataExplorer)
library(maps)
library(plotly)
library(DT)
library(tidytext)
library(gridExtra)
library(readxl)
library(ggplot2)
library(dplyr)
library(plotly)
library(tidyr)
library(d3Tree)
# 2. Verisetini yükleme

getwd()

setwd("/home/ilke/Downloads")

df<- read.csv("datascience_salaries (1).csv",sep=",", header=TRUE,stringsAsFactors = FALSE)

# 3. Keşifci Veri Analizi(Veriseti genel yapısı hakkında bilgi edinme)
glimpse(df)
summary(df)
plot_intro(df)
## 3.1  veri ön işleme/ temizleme

df[df==""] <- NA #Eksik gözlem olmadığını görmüştük. ancak Na/Null girilmemiş ama boş gözlem varsa NA ekleyerek tekrar kontrol sağlanır.

sum(is.na(df))  #veride eksik gözlem yok.

df$X <- NULL   #gereksiz satır silme

#%99unun maaşı USD türünden girili. Euro ve diğer olanları çıkardım.
df <-df %>%
  filter(salary_currency == "USD")


## 3.2 Genel istatistik/grafik
### 3.1.1 Sürekli Değişkenlerin Özet Bazı İstatistikleri
profiling_num(df)
### 3.1.2 Genel Histogram
plot_num(df)  #genel histogram
#veri setiyle ilgili genel bir önizleme için
d3tree(list(root = df2tree(rootname ='title', 
                           struct = as.data.frame(df)), 
            layout = 'collapse'))
ggplot(df,aes(job_title, fill=experience_level))+
  geom_bar()+
  coord_polar(theta = "y")

title_count<- df %>% group_by(job_title) %>% tally() %>% arrange(n, decreasing=T)
title_count

#mesleklerde intern dağılımı nasıl?
#Big data'da intern olarak çalışan yok
ggplot(data = df) + 
  geom_bar(mapping = aes(x = job_title, fill = job_type), position = "dodge") + scale_fill_manual(values = c("blue","red"))+
  theme_bw()

#mesleklere göre deneyimleri görebileceğimiz grafik
ggplot(data = df) + 
  geom_bar(mapping = aes(x = job_title, fill = experience_level), position = "dodge") + scale_fill_manual(values = c("yellow", "red", "blue","magenta"))+
  theme_bw() 

#verisetine çalışma yeri olarak remote/no remote ifade edecek şekilde remote=0, no remote=1 olacak şekilde yeni bir sütuna eklendi.
ds <- df %>% mutate(remote = ifelse(grepl("remote",tolower(location) ), "1", "0"))

#131 kişi remote olarak çalışmaktadır.
remote_counts <- count(ds, remote)
remote_counts

write.csv(ds, file = "/home/ilke/Downloads/ödev.csv", row.names = FALSE)
#remote eklenmiş halini kaydetme

# çalışanların ½11'i remote olarak çalışmaktadır.
data <- c(131, 1026)
data_percent <- prop.table(data) * 100
library(ggplot2)
data_df <- data.frame(remote = c("Remote", "No remote"), count = c(131, 1026))

ggplot(data = data_df, aes(x = "", y = count, fill = remote)) + 
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start = 0) + 
  ggtitle("Location Pie Chart") +
  scale_fill_manual(values = c("#00BFC4", "#F8766D")) +
  theme_void() + 
  labs(fill = "location", title = "Location Pie Chart") +
  geom_text(aes(label = paste0(round(data_percent), "%")), position = position_stack(vjust = 0.5))

#en çok çalışılan lokasyonlara bakalım
top_10_locations <- df %>% group_by(location) %>% 
  summarise(count=n()) %>% 
  arrange(desc(count)) %>% 
  top_n(10)
top_10_locations

ggplot(data = top_10_locations, aes(x = location, y = count, colour = location)) +
  geom_bar(stat = "identity") +
  ggtitle("Lokasyon ve Count") +
  xlab("Lokasyon") +
  ylab("Count")

#Çalışma tipine göre sıklıklar ve maaş istatistikleri
salary_by_jobtype <- df %>% group_by(job_type) %>% 
  summarise(count= n(),
            min_salary=min(salary),
            max_salary=max(salary),
            mean_salary=mean(salary)) %>% 
  arrange(desc(mean_salary)) %>% 
  mutate(rank = rank(-mean_salary))
salary_by_jobtype


#mesleğe göre meslek sıklıkları ve maaş istatistikleri
salary_by_job <- df %>% group_by(job_title) %>% 
  summarise(count= n(),
            min_salary=min(salary),
            max_salary=max(salary),
            mean_salary=mean(salary),
            median_salary=median(salary)) %>% 
  arrange(desc(mean_salary)) %>% 
  mutate(rank = rank(-mean_salary))
salary_by_job

#deneyim seviyesine göre maaş istatistikleri
salary_by_exp <- df %>% group_by(experience_level) %>% summarise(count= n(),
                                                                 min_salary=min(salary),
                                                                 max_salary=max(salary),
                                                                 mean_salary=mean(salary)) %>% 
  arrange(desc(mean_salary)) %>% 
  mutate(rank = rank(-mean_salary))
salary_by_exp

#meslek ve çalışma tiplerine göre maaş ortalamaları
a <- df %>% 
  select(job_title, salary, job_type) %>%
  group_by(job_title, job_type) %>%
  summarise(avg_income = mean(salary))
a

#remote çalışma durumuna göre maaş istatistikleri
salary_by_remote <- ds %>% group_by(remote) %>% 
  summarise(count= n(),            
            min_salary=min(salary),
            max_salary=max(salary),
            mean_salary=mean(salary)) %>% 
  arrange(desc(mean_salary)) %>% 
  mutate(rank = rank(-mean_salary))
salary_by_remote

#meslek ve deneyimlere göre maaşın saçılım grafiği
saçılım<- plot_ly(ds , x= ~experience_level , y = ~salary , z= ~job_title , color = ~job_title , text = ~salary ) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = "LEVEL"),
                          yaxis= list(title = "SALARY"),
                          zaxis = list (title = "TİTLE"))
  )
saçılım

p1 <- plot_ly(ds, y = ~salary, color = ~experience_level, colors = c('#F8766D', '#00BA38','#619CFF'), legendgroup = ~job_title) %>% 
  add_boxplot(x = ~job_title)

p1

#mesleklere göre remote/no remeto maaş
ggplot(ds, aes(x = remote, y = salary, fill =job_title, colour = job_title)) + 
  geom_boxplot(outlier.colour = NA) + xlab("remote/no remote") + ylab("salary") 

### Veri bilimcisi rollerine ilişkin 1171 gözlem vardı. Veriler temiz ve tüm gözlemler kaydedildi. EUR ve GBP cinsinden kaydedilen birkaç maaş, veri setinden çıkarılarak 1157 gözleme düşürüldü

Çoğu, kendilerini veri analisti veya veri bilimcisi olarak tanımlıyor. 
Yaklaşık 4 kişiden 1'i makine öğrenimi rolündeyken, çok azı makine öğrenimi operasyonları rolünde. 

Rollerin yaklaşık %3'ü stajyerler içindi. Veri setindeki gözlemlerin büyük çoğunluğu Full Time çalışan gözlemlerden oluşmaktadır.

Yanıt verenlerin çoğu (%62) üst düzey pozisyonlarda çalışıyordu ve önemsiz bir %1'i yöneticiydi. 

Rollerin %10'u giriş seviyesindeyken, rollerin dörtte biri orta düzey taşıyıcı veri bilimcileri içindi.
                       
                       Konumları remote/ remote olmama üzerine sınıflandırarak remote= 0, remote olmayan=1 olarak yeni sütuna eklendi.
                       Konumların çoğu Amerika Birleşik Devletleri'ne (New York, San-Francisco) dağılmıştı, ardından Birleşik Krallık (Çoğunlukla Londra'da) ve iyi bir sayı da Avrupa'da (Paris, Berlin, Barselona). 
Bu rollerin yaklaşık %11'i uzaktı.


Bir Veri Bilimcisinin Ortalama Maaşı 65.000 USD civarındadır ve medyan 63.000 USD'ye çok yakındır. Bazıları 30.000 USD kadar düşük, birkaçı ise 200.000 USD'den fazla kazanıyor. 
En Yüksek Kazananlar, Makine öğrenimi operasyonları, Makine öğrenimi ve Veri Bilimcisi ile Yönetici veya üst düzey rollerde bulunanlardır. 

Uzaktan çalışanlar, uzak olmayan çalışanlardan biraz daha yüksek kazanma eğilimindedir.