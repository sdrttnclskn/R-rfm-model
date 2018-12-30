##kutuphane yuklenmesi
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
install.packages("RColorBrewer")
library(RColorBrewer)
#####
options(knitr.table.format = 'html')
options(tibble.width = Inf)

## tarih -> date, musterino -> faktor donusumu.
avans <- avans %>% mutate(TARIH=as.Date(TARIH), MUSTERINO = as.factor(MUSTERINO))

#referans tarih -> refday
refday <- max(acezatutar_model$tarih)

## acezatutar tablosu olusumu.
acezatutar_model <- data.frame(avans$MUSTERINO,avans$TARIH,(avans$AVANSGECIKMELIODENENTUTAR*0.7)+(avans$AVANSTEMERRUTLUODENENTUTAR*1.4))
colnames(acezatutar_model) <- c("musterino","tarih","cezatutar")

##sonislemtarih,islemsayisi(frequency),toplamcezatutar(monetary) , gecengunsayisi(recency) hesaplama.
tb_gecengunsayisi <- acezatutar_model %>% group_by(musterino) %>% summarise(gecengunsayisi= as.numeric(refday)-as.numeric(max(tarih)))
tb_sonislemtarih <- acezatutar_model %>% group_by(musterino) %>% summarise(sonislemtarih=max(tarih))
tb_islemsayisi <- acezatutar_model %>% group_by(musterino) %>% summarise(islemsayisi = n())
tb_toplamcezatutar <- acezatutar_model %>% group_by(musterino) %>% summarise(toplamcezatutar = sum(cezatutar))

##acezatutar_model_musteri tablosu olusturma.
acezatutar_model_musteri <- data.frame(tb_toplamcezatutar$musterino,tb_toplamcezatutar$toplamcezatutar,tb_sonislemtarih$sonislemtarih,
                                       tb_islemsayisi$islemsayisi,tb_gecengunsayisi$gecengunsayisi)
colnames(acezatutar_model_musteri) <-c("musterino","toplamcezatutar","sonislemtarih","islemsayisi","gecengunsayisi")

##RFM Score, RFM Table hesaplamaları
acezatutar_rfm_result <- rfm_table_customer(acezatutar_model_musteri,musterino,
                                            islemsayisi,gecengunsayisi,toplamcezatutar,refday)
acezatutar_rfm_result

##Heat Map
rfm_heatmap(acezatutar_rfm_result)

##Bar Chart
rfm_bar_chart(acezatutar_rfm_result)

##Histogram
rfm_histograms(acezatutar_rfm_result)

##Customers by Orders
rfm_order_dist(acezatutar_rfm_result)

##Scatter Plots
rfm_rm_plot(acezatutar_rfm_result)
rfm_fm_plot(acezatutar_rfm_result)
rfm_rf_plot(acezatutar_rfm_result)


###Segments

segment <- c(
  "Çok Aşırı Riskli", "Çok Riskli", "Riskli",
  "Potansiyel Riskli", "Az Riskli", "Orta Riskli",
  "Ortadan az Riskli", "Ortadan çok Riskli", "Azalan Risk", "Çok az Riskli",
  "Risksiz"
)

description <- c(
  "Son zamanlarda ceza düşer, sık ceza düşer ve ceza tutarı çok",
  "Ceza tutarı iyi miktarlarda. Güncel zamanlar ve sıklıkta işlem olur.",
  "Güncel zamanlar, ceza tutarı iyi bir miktar, bir kereden fazla cezaya düştü.",
  "Daha yakın zamanda cezaya düşmüş, ancak sık sık değil",
  "Son zamanlarda cezaya düşmüş, ama ceza miktarı fazla değil",
  "Ortalamanın üstünde, guncelik, sıklık ve ceza miktarı değerlerin üstünde",
  "Ortalama değerin altında, sıklık ve ceza miktarı değerlerin altında",
  "Ceza miktarı çok, sık sık ama uzun zaman önce cezaya düştü",
  "Büyük miktarlar da cezaya düştü ve sık sık, ama uzun zaman önce",
  "Düşük miktarda cezaya düşmüş, düşük frekanslı, uzun zaman önce",
  "En düşük ceza miktarı, sıklık ve güncelik"
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



##Segmented Customer Data

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
        (monetary_score %>% between(2, 3)) ~ "Orta Riskli",
      (recency_score %>% between(2, 3)) & (frequency_score <= 2) &
        (monetary_score <= 2) ~ "Ortadan az Riskli",
      (recency_score <= 2) & (frequency_score %>% between(2, 5)) &
        (monetary_score %>% between(2, 5)) ~ "Ortadan çok Riskli",
      (recency_score == 1) & (frequency_score %>% between(4, 5)) &
        (monetary_score %>% between(4, 5)) ~ "Azalan Risk",
      (recency_score %>% between(1, 2)) & (frequency_score %>% between(1, 2)) &
        (monetary_score %>% between(1, 2)) ~ "Çok az Riskli",
      (recency_score <= 2) & (frequency_score <= 2) &
        (monetary_score <= 2) ~ "Risksiz",
      TRUE ~ "Diğerleri"
    )
  ) %>%
  select(
    customer_id,segment, rfm_score, transaction_count, recency_days, amount
  )

# use datatable
acezatutar_rfm_datatable<-acezatutar_rfm_segments %>%
  datatable(
    filter = "top",
    options = list(pageLength = 5, autoWidth = TRUE),
    colnames = c(
      "MusteriNo", "Segment","RFM","İşlem Sayısı", "Gecen Gün Sayısı", "Toplam Ceza Tutarı"
    )
  )


##Segment Size
acezatutar_segment_size <- acezatutar_rfm_segments %>%
  count(segment) %>%
  arrange(desc(n)) %>%
  rename(Segment = segment, Count = n)

#session kayedilmesi
save.image('/Users/sdrttnclskn/Desktop/makale/r_kodlar/acezatutar_model.rdata')
