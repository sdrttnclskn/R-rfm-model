##-----------------------kutuphane yuklenmesi---------------------------------------------------------
install.packages("rfm")
library(rfm)
install.packages("knitr")
library(knitr)
install.packages("kableExtra")
library(kableExtra)
install.packages("magrittr")
library(magrittr)
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)
install.packages("tidyverse")
library(tidyverse)
install.packages("DT")
library(DT)
install.packages("grDevices")
library(grDevices)
install.packages("arules")
library(arules)
install.packages("RColorBrewer")
library(RColorBrewer)
#####
options(knitr.table.format = 'html')
options(tibble.width = Inf)

## ------------------------tarih -> date, musterino -> faktor donusumu.-------------------------------------------------------
ceza_model <- ceza_model %>% mutate(TARIH=as.Date(TARIH), MUSTERINO = as.factor(MUSTERINO))

#--------------------------referans tarih -> refday----------------------------------------------------------------------------
refday <- max(ceza_model$TARIH)

## ---------------------------------------------acezatutar tablosu olusumu.-----------------------------------------------------
acezatutar_model <- data.frame(avans$MUSTERINO,avans$TARIH,(avans$AVANSGECIKMELIODENENTUTAR*0.7)+(avans$AVANSTEMERRUTLUODENENTUTAR*1.4))
colnames(acezatutar_model) <- c("musterino","tarih","cezatutar")

##-----------------sonislemtarih,islemsayisi(frequency),toplamcezatutar(monetary) , gecengunsayisi(recency) hesaplama.------------
tb_gecengunsayisi <- acezatutar_model %>% group_by(musterino) %>% summarise(gecengunsayisi= as.numeric(refday)-as.numeric(max(tarih)))
tb_sonislemtarih <- acezatutar_model %>% group_by(musterino) %>% summarise(sonislemtarih=max(tarih))
tb_islemsayisi <- acezatutar_model %>% group_by(musterino) %>% summarise(islemsayisi = n())
tb_toplamcezatutar <- acezatutar_model %>% group_by(musterino) %>% summarise(toplamcezatutar = sum(cezatutar))


##---------------------------------------------acezatutar_model_musteri tablosu olusturma.---------------------
acezatutar_model_musteri <- data.frame(tb_toplamcezatutar$musterino,tb_toplamcezatutar$toplamcezatutar,tb_sonislemtarih$sonislemtarih,
                                       tb_islemsayisi$islemsayisi,tb_gecengunsayisi$gecengunsayisi)
colnames(acezatutar_model_musteri) <-c("musterino","toplamcezatutar","sonislemtarih","islemsayisi","gecengunsayisi")


##-----------------------------2.Metod, tek hesaplama rfm------------------------------------------------------------

cezaRFM <- ceza_model %>% group_by(MUSTERINO) %>% 
  summarise(gecengunsayisi=as.numeric(refday)-as.numeric(max(TARIH)),sonislemtarih=max(TARIH),
            islemsayisi=n(), toplamcezatutar = sum(AVANSCEZATUTARI)) 

acezatutar_model_musteri <- data.frame(cezaRFM$MUSTERINO,cezaRFM$toplamcezatutar,cezaRFM$sonislemtarih,cezaRFM$islemsayisi,cezaRFM$gecengunsayisi)

##---------------------------------------------RFM Score, RFM Table hesaplamaları-----------------------------
acezatutar_rfm_result <- rfm_table_customer(acezatutar_model_musteri,musterino,
                                            islemsayisi,gecengunsayisi,toplamcezatutar,refday)
acezatutar_rfm_result

##----------------------------------------------Kmeans tablos olusum-----------------------------------------
acezatutar_rfm_table<- acezatutar_rfm_result$rfm
##toplamtutar_rfm_table$amount <- log(toplamtutar_rfm_table$amount) --> miktar dusuk gerek yok.


##-----------------------------------------------Heat Map-----------------------------------------------------
rfm_heatmap(acezatutar_rfm_result)

##----------------------------------------------Bar Chart-----------------------------------------------------
rfm_bar_chart(acezatutar_rfm_result)

##----------------------------------------------Histogram-----------------------------------------------------
rfm_histograms(acezatutar_rfm_result)

##---------------------------------------------Customers by Orders---------------------------------------------
rfm_order_dist(acezatutar_rfm_result)

##---------------------------------------------Scatter Plots----------------------------------------------------
rfm_rm_plot(acezatutar_rfm_result)
rfm_fm_plot(acezatutar_rfm_result)
rfm_rf_plot(acezatutar_rfm_result)


###--------------------------------------------Segments---------------------------------------------------------

segment <- c(
  "Çok Aşırı Riskli", "Çok Riskli", "Riskli",
  "Potansiyel Riskli", "Az Riskli", "Ortadan Çok Riskli",
  "Orta Riskli", "Ortadan Az Riskli", "Azalan Risk", "Çok Az Riskli",
  "Risksiz"
)

description <- c(
  "Yakın zamanlarda cezaya düşer, sık sık cezaya düşer ve ceza tutarı çok",
  "Ceza tutarı yüksek miktarlarda. Güncel ve sıklıkta işlem olur.",
  "Güncel, ceza tutarı iyi bir miktar, bir kereden fazla cezaya düştü.",
  "Daha yakın zamanda cezaya düşmüş, ancak sık sık değil",
  "Son zamanlarda cezaya düşmüş, ama ceza miktarı fazla değil",
  "Guncellik, sıklık ve ceza miktarıları ortalama skorların üstünde",
  "Guncellik, sıklık ve ceza miktarı ortalama değerlerin altında",
  "Ceza miktarı çok fakat uzun zaman önce, sıklıkla cezaya düştü",
  "Büyük miktarlar da cezaya düştü ve sık sık, ama uzun zaman önce",
  "Düşük miktarda cezaya düşmüş, düşük frekanslı, uzun zaman önce",
  "Sıklık, güncellik ve ceza miktarı çok düşük"
)

recency <- c("4 - 5", "2 - 5", "3 - 5", "4 - 5", "3 - 4", "2 - 3", "2 - 3", "<= 2", "<= 1", "1 - 2", "<= 2")
frequency <- c("4 - 5", "3 - 5", "1 - 3", "<= 1", "<= 1", "2 - 3", "<= 2", "2 - 5", "4 - 5", "1 - 2", "<= 2")
monetary <- c("4 - 5", "3 - 5", "1 - 3", "<= 1", "<= 1", "2 - 3", "<= 2", "2 - 5", "4 - 5", "1 - 2", "<= 2")

segments <- tibble(
  Segment = segment, Description = description, R = recency, 'F'= frequency, M = monetary
)

segments %>%
  kable() %>%
  kable_styling(full_width = TRUE, font_size = 12)



##------------------------------------------Segmented Customer Data--------------------------------------------------

acezatutar_rfm_segments <- acezatutar_rfm_result %>%
  use_series(rfm) %>%
  mutate(
    segment = case_when(
      (recency_score %>% between(4,5)) & (frequency_score %>% between(4,5)) & 
        (monetary_score %>% between(4,5)) ~ "Çok Aşırı Riskli",
      (recency_score %>% between(2, 5)) & (frequency_score %>% between(3, 5)) &
        (monetary_score %>% between(3, 5)) ~ "Çok Riskli",
      (recency_score %>% between(3, 5)) & (frequency_score %>% between(1, 3)) &
        (monetary_score %>% between(1, 3)) ~ "Riskli",
      (recency_score %>% between(4, 5)) & (frequency_score == 1) &
        (monetary_score == 1) ~ "Potansiyel Riskli",
      (recency_score %>% between(3, 4)) & (frequency_score == 1) &
        (monetary_score == 1) ~ "Az Riskli",
      (recency_score %>% between(2, 3)) & (frequency_score %>% between(2, 3)) &
        (monetary_score %>% between(2, 3)) ~ "Ortadan Çok Riskli",
      (recency_score %>% between(2, 3)) & (frequency_score <= 2) &
        (monetary_score <= 2) ~ "Orta Riskli",
      (recency_score <= 2) & (frequency_score %>% between(2, 5)) &
        (monetary_score %>% between(2, 5)) ~ "Ortadan Az Riskli",
      (recency_score == 1) & (frequency_score %>% between(4, 5)) &
        (monetary_score %>% between(4, 5)) ~ "Azalan Risk",
      (recency_score %>% between(1, 2)) & (frequency_score %>% between(1, 2)) &
        (monetary_score %>% between(1, 2)) ~ "Çok Az Riskli",
      (recency_score <= 2) & (frequency_score <= 2) &
        (monetary_score <= 2) ~ "Risksiz",
      TRUE ~ "Diğerleri"
    )
  ) %>%
  select(
    customer_id,segment, rfm_score, transaction_count, recency_days, amount
  )

#-----------------------------------------use datatable-----------------------------------------------------
acezatutar_rfm_datatable<-acezatutar_rfm_segments %>%
  datatable(
    filter = "top",
    options = list(pageLength = 5, autoWidth = TRUE),
    colnames = c(
      "MusteriNo", "Segment","RFM","İşlem Sayısı", "Gecen Gün Sayısı", "Toplam Ceza Tutarı"
    )
  )


##---------------------------------------RFM Segment Size-----------------------------------------------------
acezatutar_segment_size <- acezatutar_rfm_segments %>%
  count(segment) %>%
  arrange(desc(n)) %>%
  rename(Segment = segment, Count = n)

## ---------------------------avg_recency, fig.align='center', fig.height=5, fig.width=6----------------------
data_avg_recency <- 
  acezatutar_rfm_segments %>%
  group_by(segment) %>%
  select(segment, recency_days) %>%
  summarize(median(recency_days)) %>%
  rename(segment = segment, avg_recency = `median(recency_days)`) %>%
  arrange(avg_recency) 

n_fill <- nrow(data_avg_recency)

ggplot(data_avg_recency, aes(segment, avg_recency)) +
  geom_bar(stat = "identity", fill = brewer.pal(n = n_fill, name = "Set1")) +
  xlab("Segment") + ylab("Median Recency") +
  ggtitle("Median Recency by Segment") +
  coord_flip() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

## -------------------------avg_frequency, fig.align='center', fig.height=5, fig.width=6-------------------------
data_avg_frequency <- 
  acezatutar_rfm_segments %>%
  group_by(segment) %>%
  select(segment, transaction_count) %>%
  summarize(median(transaction_count)) %>%
  rename(segment = segment, avg_frequency = `median(transaction_count)`) %>%
  arrange(avg_frequency) 

n_fill <- nrow(data_avg_frequency)

ggplot(data_avg_frequency, aes(segment, avg_frequency)) +
  geom_bar(stat = "identity", fill = brewer.pal(n = n_fill, name = "Set1")) +
  xlab("Segment") + ylab("Median Frequency") +
  ggtitle("Median Frequency by Segment") +
  coord_flip() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )


## ----------------------------avg_monetary, fig.align='center', fig.height=5, fig.width=6--------------------
data_avg_monetary <- 
  acezatutar_rfm_segments %>%
  group_by(segment) %>%
  select(segment, amount) %>%
  summarize(median(amount)) %>%
  rename(segment = segment, avg_monetary = `median(amount)`) %>%
  arrange(avg_monetary) 

n_fill <- nrow(data_avg_monetary)

ggplot(data_avg_monetary, aes(segment, avg_monetary)) +
  geom_bar(stat = "identity", fill = brewer.pal(n = n_fill, name = "Set1")) +
  xlab("Segment") + ylab("Median Monetary Value") +
  ggtitle("Median Monetary Value by Segment") +
  coord_flip() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )


#---------------------------------------session kayedilmesi-------------------------------------------------
save.image('/Users/sdrttnclskn/Desktop/makale/r_kodlar/acezatutar_model.rdata')
