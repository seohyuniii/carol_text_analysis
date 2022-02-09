#파일 불러오기
period1 <- vector()
period2 <- vector()
period3 <- vector()

for (i in list.files(path='./코퍼스언어학_기말과제', pattern='[.]txt$'))
{
file <- scan(file=paste('./코퍼스언어학_기말과제', i, sep='/'), what='', quote=NULL, encoding='UTF-8')
file <- gsub('[()]', '', tolower(file))
file <- gsub('^[[:punct:]]+|[[:punct:]]+$', '', file)
file <- file[nchar(file)>0]
if (i == 'period1.txt') {period1 <- c(period1, file)}
else if (i == 'period2.txt') {period2 <- c(period2, file)}
else {period3 <- c(period3, file)}
}

## 빈도분석
#데이터프레임 만들기
period1.Freq <- data.frame(sort(table(period1), decreasing=T))
period2.Freq <- data.frame(sort(table(period2), decreasing=T))
period3.Freq <- data.frame(sort(table(period3), decreasing=T))


#워드클라우드 만들기
library(wordcloud)
wordcloud(period1.Freq$period1, period1.Freq$Freq, scale=c(4, 0.9), min.freq=2, max.words=250,
	    random.order=F, rot.per=0.4, colors=brewer.pal(11, 'RdYlGn'))
wordcloud(period2.Freq$period2, period2.Freq$Freq, scale=c(4, 0.9), min.freq=2, max.words=250,
	    random.order=F, rot.per=0.4, colors=brewer.pal(11, 'RdYlGn'))
wordcloud(period3.Freq$period3, period3.Freq$Freq, scale=c(4, 0.8), min.freq=2, max.words=250,
	    random.order=F, rot.per=0.4, colors=brewer.pal(11, 'RdYlGn'))

## 연어 분석
#1단계 - 공기어 추출 / 용례(concordance)
index <- grep('christmas', period3)
span <- unlist(lapply(index,
		   function(i){c((i-4):(i-1), (i+1):(i+4))}))
span <- span[span>0 & span<=length(period3)]
crc <- period3[span]

#2단계 - 공기빈도 데이터프레임
Freq.span <- sort(table(crc), decreasing=T)
Freq.all <- table(period3)
Freq.co <- data.frame(W1=vector(), W2=vector(), W1W2=vector(), N=vector())
Freq.co <- data.frame(t(sapply(names(Freq.span),
					 function(x){c(length(index), Freq.all[x], Freq.span[x], length(period3))})))
colnames(Freq.co) <- c('W1', 'W2', 'W1W2', 'N')

#3단계 - 연어 계산
collocates <- data.frame(Freq.co,
				 t.score=(Freq.co$W1W2 - ((Freq.co$W1*Freq.co$W2)/ Freq.co$N)) / sqrt(Freq.co$W1W2),
				 MI=log2((Freq.co$W1W2*Freq.co$N) / (Freq.co$W1*Freq.co$W2)))

#t score 정렬
t.score.sort <- collocates[order(collocates$t.score, decreasing=T), ]

#MI 정렬
MI.sort <- collocates[order(collocates$MI, decreasing=T), ]
MI.sort.2 <- MI.sort[MI.sort$W1W2>=2, ]
head(MI.sort.2, 15)


## 키워드 분석
# TDM 만들기 
DF <- lapply(list.files(path='./코퍼스언어학_기말과제', pattern='[.]txt$'),
		 function(x){scan(file=paste("./코퍼스언어학_기말과제", x, sep="/"), what="char", quote=NULL, encoding='UTF-8')})
DF <- lapply(DF, function(x){gsub('[()]', '', tolower(x))})
DF <- lapply(DF, function(x){gsub('^[[:punct:]]+|[[:punct:]]+$', '', x)})
DF <- lapply(DF, function(x){data.frame(table(x[nchar(x)>0]))})
TDM <- Reduce(function(x, y){merge(x, y, by='Var1', all=T)}, DF)
colnames(TDM) <- c('words', unlist(lapply(list.files(path='./코퍼스언어학_기말과제', pattern='[.]txt$'),
							function(x){substring(gsub('[.].+$', '', x), 1)})))

# TDM 가공 - 행 명을 words로 & NA 치환
TDM <- data.frame(row.names=TDM$words, TDM[2:length(TDM)])
TDM[is.na(TDM)] <- 0
TDM['RowSums'] <- rowSums(TDM)
TDM <- TDM[order(TDM$RowSums, decreasing=T), ]
colSums(TDM)

# comparison cloud
library(wordcloud)
comparison.cloud(TDM[c(1,2,3)],random.order=FALSE, scale=c(1.5, 0.5), rot.per=.4, max.words=350,
		     colors=brewer.pal(8, 'Dark2'), title.size=0.8)

# 카이스퀘어 잔차를 이용한 키워드 분석
CHI <- chisq.test(TDM[c(1,2,3)])$residuals
CHI <- as.data.frame(CHI)
head(CHI[order(CHI$period3, decreasing=T), ], 20)


## 주성분 분석
stop <- readLines('./데이터 파일./13_EnglishStopwords.txt')
NEW <- TDM[!(rownames(TDM) %in% stop), ]

library(AMR)
PCA <- prcomp(scale(NEW[1:20, -length(NEW)]))
ggplot_pca(PCA, labels=rownames(NEW[1:20, -length(NEW)]))
ggplot_pca(prcomp(scale(t(NEW[1:20, -length(NEW)]))), labels=rownames(t(NEW[1:20, -length(NEW)])))


## 대응 분석
library(ca)
plot(ca(NEW[1:20, -length(NEW)]), arrows=c(1,0))
plot(ca(t(NEW[1:20, -length(NEW)])), arrows=c(1,0))


## intersect
intersect_3 <- intersect(intersect(period1, period2), period3)
NEW[rownames(NEW) %in% intersect_3, ]

setdiff_3 <- setdiff(setdiff(period3, period2), period1)
NEW[rownames(NEW) %in% setdiff_3, ]