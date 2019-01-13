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
avans <- avans %>% mutate(TARIH=as.Date(TARIH), MUSTERINO = as.factor(MUSTERINO))


## ---------------------------------------------acezatutar tablosu olusumu.-----------------------------------------------------
atoplamtutar_model <- data.frame(avans$MUSTERINO,avans$TARIH,avans$AVANSTMRTSUZODENENTUTAR)
colnames(atoplamtutar_model) <- c("musterino","tarih","toplamtutar")

#--------------------------referans tarih -> refday----------------------------------------------------------------------------
refday <- max(atoplamtutar_model$tarih)

##-----------------sonislemtarih,islemsayisi(frequency),toplamcezatutar(monetary) , gecengunsayisi(recency) hesaplama.------------
tb_gecengunsayisi <- atoplamtutar_model %>% group_by(musterino) %>% summarise(gecengunsayisi= as.numeric(refday)-as.numeric(max(tarih)))
tb_sonislemtarih <- atoplamtutar_model %>% group_by(musterino) %>% summarise(sonislemtarih=max(tarih))
tb_islemsayisi <- atoplamtutar_model %>% group_by(musterino) %>% summarise(islemsayisi = n())
tb_toplamtutar <- atoplamtutar_model %>% group_by(musterino) %>% summarise(toplamtutar = sum(toplamtutar))

##---------------------------------------------acezatutar_model_musteri tablosu olusturma.---------------------
atoplamtutar_model_musteri <- data.frame(tb_toplamtutar$musterino,tb_toplamtutar$toplamtutar,tb_sonislemtarih$sonislemtarih,
                                       tb_islemsayisi$islemsayisi,tb_gecengunsayisi$gecengunsayisi)
colnames(atoplamtutar_model_musteri) <-c("musterino","toplamtutar","sonislemtarih","islemsayisi","gecengunsayisi")

##---------------------------------------------RFM Score, RFM Table hesaplamaları-----------------------------
atoplamtutar_rfm_result <- rfm_table_customer(atoplamtutar_model_musteri,musterino,
                                            islemsayisi,gecengunsayisi,toplamtutar,refday)
atoplamtutar_rfm_result

##-----------------------------------------------Heat Map-----------------------------------------------------
rfm_heatmap(atoplamtutar_rfm_result)

##----------------------------------------------Bar Chart-----------------------------------------------------
rfm_bar_chart(atoplamtutar_rfm_result)

##----------------------------------------------Histogram-----------------------------------------------------
rfm_histograms(atoplamtutar_rfm_result)

##---------------------------------------------Customers by Orders---------------------------------------------
rfm_order_dist(atoplamtutar_rfm_result)

##---------------------------------------------Scatter Plots----------------------------------------------------
rfm_rm_plot(atoplamtutar_rfm_result)
rfm_fm_plot(atoplamtutar_rfm_result)
rfm_rf_plot(atoplamtutar_rfm_result)


###--------------------------------------------Segments---------------------------------------------------------

segment <- c(
  "Şampiyonlar", "Sadık müşteriler", "Potansiyel Sadık",
  "Yeni Müşteriler", "Umut verici", "Dikkat Gerekli",
  "Uyumak Üzere", "Riskli", "Kaybedilmek İstenmeyen Müşteri", "Kayıp"
   
)

description <- c(
  "Güncel, sık satın alan ve en çok toplam tutarda işlem yapan",
  "İyi tutarlarda işlem yapar. Promosyonlara duyarlı",
  "Güncel müşteriler, iyi tutarlarda işlem yaptı, bir kereden fazla işlem yaptı",
  "Yakın zamanlarda işem yapmış, ancak sık sık değil",
  "Son zamanalı işlem yapmış ama düşük miktarlar",
  "Güncellik, sıklık ve parasal değerleri ortalma değerlerin üstünde(Sınırda)",
  "Güncellik, sıklık ve parasal değerleri ortalma değerlerin altında",
  "İyi tutarlarda işlemler yaptı, sık sık ama uzun zaman önce işlem yaptı.",
  "Büyük tutarlarda işlemler yaptı ve sık sık, ama uzun zaman önce işlem yaptı",
  "Çok düşük tutarlarda işlemler yaptı, çok düşük işlem sayısı, uzun zaman önce işlem yaptı"
 
)

recency <- c("4 - 5", "2 - 5", "3 - 5", "4 - 5", "3 - 4", "2 - 3", "2 - 3", "<= 2", "<= 1", "1 - 2")
frequency <- c("4 - 5", "3 - 5", "1 - 3", "<= 1", "<= 1", "2 - 3", "<= 2", "2 - 5", "4 - 5", "1 - 2")
monetary <- c("4 - 5", "3 - 5", "1 - 3", "<= 1", "<= 1", "2 - 3", "<= 2", "2 - 5", "4 - 5", "1 - 2")

segments <- tibble(
  Segment = segment, Description = description, R = recency, 'F'= frequency, M = monetary
)

segments %>%
  kable() %>%
  kable_styling(full_width = TRUE, font_size = 12)


##------------------------------------------Segmented Customer Data--------------------------------------------------

toplamtutar_rfm_segments <- toplamtutar_rfm_result %>%
  use_series(rfm) %>%
  mutate(
    segment = case_when(
      (recency_score %>% between(4,5)) & (frequency_score %>% between(4,5)) & 
        (monetary_score %>% between(4,5)) ~ "Şampiyonlar",
      (recency_score %>% between(2, 5)) & (frequency_score %>% between(3, 5)) &
        (monetary_score %>% between(3, 5)) ~ "Sadık müşteriler",
      (recency_score %>% between(3, 5)) & (frequency_score %>% between(1, 3)) &
        (monetary_score %>% between(1, 3)) ~ "Potansiyel Sadık",
      (recency_score %>% between(4, 5)) & (frequency_score == 1) &
        (monetary_score == 1) ~ "Yeni Müşteriler",
      (recency_score %>% between(3, 4)) & (frequency_score == 1) &
        (monetary_score == 1) ~ "Umut veric",
      (recency_score %>% between(2, 3)) & (frequency_score %>% between(2, 3)) &
        (monetary_score %>% between(2, 3)) ~ "Dikkat Gerekli",
      (recency_score %>% between(2, 3)) & (frequency_score <= 2) &
        (monetary_score <= 2) ~ "Uyumak Üzere",
      (recency_score <= 2) & (frequency_score %>% between(2, 5)) &
        (monetary_score %>% between(2, 5)) ~ "Riskli",
      (recency_score == 1) & (frequency_score %>% between(4, 5)) &
        (monetary_score %>% between(4, 5)) ~ "Kaybedilmek İstenmeyen Müşteri",
      (recency_score %>% between(1, 2)) & (frequency_score %>% between(1, 2)) &
        (monetary_score %>% between(1, 2)) ~ "Kayıp",
      TRUE ~ "Diğerleri"
    )
  ) %>%
  select(
    customer_id,segment, rfm_score, transaction_count, recency_days, amount
  )

#-----------------------------------------use datatable-----------------------------------------------------
toplamtutar_rfm_datatable<-toplamtutar_rfm_segments %>%
  datatable(
    filter = "top",
    options = list(pageLength = 5, autoWidth = TRUE),
    colnames = c(
      "MusteriNo", "Segment","RFM","İşlem Sayısı", "Gecen Gün Sayısı", "Toplam Tutarı"
    )
  )


##---------------------------------------RFM Segment Size-----------------------------------------------------
atoplamtutar_segment_size <- atoplamtutar_rfm_segments %>%
  count(segment) %>%
  arrange(desc(n)) %>%
  rename(Segment = segment, Count = n)

## ---------------------------avg_recency, fig.align='center', fig.height=5, fig.width=6----------------------
data_avg_recency <- 
  atoplamtutar_rfm_segments %>%
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
  atoplamtutar_rfm_segments %>%
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
  atoplamtutar_rfm_segments %>%
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
save.image('/Users/sdrttnclskn/Desktop/makale/r_kodlar/atoplamtutar_model.rdata')
