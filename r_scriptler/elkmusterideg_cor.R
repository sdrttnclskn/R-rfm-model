#*****korelasyon hesaplanması**********
load(file.choose())
avansdosyası2 <- avansdosyası

#null verilerin temizlenmesi
nasiz_veriler<-avansdosyası2[complete.cases(avansdosyası2),]

#korelasyon hesaplama
cor_avans<-cor(nasiz_veriler)
install.packages("corrplot")
library(corrplot)
#png("file.png")
#1.metod
corrplot(cor_avans, type = "upper", tl.pos = "td",
         method = "circle", tl.cex = 0.5, tl.col = 'black', diag = FALSE)

#2.metod
corrplot(cor_avans, method = "circle",tl.cex = 0.5, tl.col = 'black')

#func zayıf cor elenmesi.
cor.mtest <- function(mat, conf.level = 0.95) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  diag(lowCI.mat) <- diag(uppCI.mat) <- 1
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], conf.level = conf.level)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      lowCI.mat[i, j] <- lowCI.mat[j, i] <- tmp$conf.int[1]
      uppCI.mat[i, j] <- uppCI.mat[j, i] <- tmp$conf.int[2]
    }
  }
  return(list(p.mat, lowCI.mat, uppCI.mat))
}

res <- cor.mtest(nasiz_veriler, 0.95)
corrplot(cor_avans, method = "circle", order = "hclust", p.mat = res[[1]], sig.level = 0.05,tl.cex = 0.5, addrect = 2, tl.col="black", tl.srt=45)

#******Regresyon Analizi Hesaplamaso*********
#Regresyon da iki ya da daha çok değişken arasındaki ilişkiyi ölçmek için kullanılan bir analizdir.
#Biri bağımlı biri bağımsız olan iki değişkenin fonksiyonel ifadesidir.

install.packages("ggplot2")
library(ggplot2)
# sınıfı numerik olan musteri sütununu factor sınıfına dönüştürelim.

reg_avans$MUSTERINO <- as.factor(as.character(reg_avans$MUSTERINO))

ggplot(reg_avans, aes(x=AVANSTMRTSUZODENENTUTAR, y=AVANSNETBORCTUTAR)) +
  +     geom_point(size=4, color="blue", shape=20)


#Regresyon doğrusu eklemek
ggplot(reg_avans, aes(x=AVANSTMRTSUZODENENTUTAR, y=AVANSNETBORCTUTAR)) +
  geom_point()+
  geom_smooth(method=lm)

# Güven aralığı kaldırıldığında
ggplot(reg_avans, aes(x=AVANSTMRTSUZODENENTUTAR, y=AVANSNETBORCTUTAR)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE)

# Loess metodu
ggplot(reg_avans, aes(x=AVANSTMRTSUZODENENTUTAR, y=AVANSNETBORCTUTAR)) +
  geom_point()+
  geom_smooth()


#session kayedilmesi
save.image('/Users/sdrttnclskn/Desktop/makale/avansdosyası2.rdata')

#kolon silme
library(readxl)
elkmusterideg <- read_excel("Desktop/makale/dataset/elkmusterideg.xlsx", 
                              +     col_types = c("date", "numeric", "blank", 
                                                  +         "blank", "blank", "blank", "blank", 
                                                  +         "blank", "blank", "blank", "blank", 
                                                  +         "blank", "blank", "blank", "blank", 
                                                  +         "blank", "numeric", "numeric", "numeric", 
                                                  +         "numeric", "numeric", "numeric", 
                                                  +         "numeric", "numeric", "numeric", 
                                                  +         "numeric", "numeric", "numeric", 
                                                  +         "numeric", "blank", "blank", "blank", 
                                                  +         "blank", "blank", "blank", "blank", 
                                                  +         "blank"))

