#RFM skorlama
getwd()
#colnames(avansdosyası2) <-c("id"....) vektör oluşturma func. Kolonlara isim verir.

#id grop, factor formatına getirlemli. tarih ise -> date

#x <-as.factor(as.character(id)), musteriid donusumu.

#dataset$id <- as.factor(as.character(id)), kolon type değişti.

# dataset$tarih <- as.Date(as.character(tarih),"%Y%m%d"")

#tarih  date formatına dönüştürldü. ve musteri nı factor
#including Libraries
install.packages("data.table")
library(data.table)
install.packages("tidyr")
library(tidyr)
install.packages("knitr")
library(knitr)
install.packages("dplyr")
library(dplyr)


avans = avans %>% mutate(TARIH= as.Date(TARIH,'%m/%d/%Y'),MUSTERINO = as.factor(MUSTERINO))

#----recency hesaplama------
refday <- max(dataset$tarih)

as.numeric(refday-dataser$tarih)

rfm_recency <- avans %>% group_by(MUSTERINO) %>% summarise(Recency = as.numeric(refDay)-as.numeric(max(avans$TARIH)))

#----frenguency hesaplama-----
count = n()
avans_frenquncy <- avans %>% group_by(MUSTERINO) %>% summarise(frequency = n())

#-----monetary hesaplama------
avans_monetary <- avans %>% group_by(MUSTERINO) %>% summarise(Monetary = sum(AVANSTMRTSUZODENENTUTAR))

# sıfır olan tüm kolonların silinmesi
tem_avans <- tem_avans[, which(colSums(tem_avans) != 0)]


#######hazır package kullanımı ve RFM score hesaplama.##############

install.packages("didrooRFM")
library(didrooRFM)

#func için data hazırlama
avansData <-data.frame(avans$X__1,avans$MUSTERINO,avans$TARIH,avans$AVANSGECIKMELIODENENTUTAR)
# func call
avansRFM = findRFM(avansData, recencyWeight = 4,frequencyWeight = 4,monetoryWeight = 4)



#session kayedilmesi
save.image('/Users/sdrttnclskn/Desktop/makale/r_kodlar/rfm.rdata')