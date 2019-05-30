library(gtrendsR)
library(data.table)
library(quantmod)
library(ggplot2)

rm(list = ls())

get_gtrend = function(news, word){
  n = length(news$Date)
  dt = as.numeric(news$Date[n] - news$Date[1])
  intervals = ceiling(dt/180)
  trends = c()
  for (i in 1:intervals){
    trends = rbind(trends, gtrends(keyword = c(paste('buy', word), paste('sell', word)),
                                   geo = 'US',time = paste(news$Date[1]-30 + (i-1)*180,
                                                           news$Date[1]-31 + i*180), onlyInterest = T)$interest_over_time)
  }
  trends = trends[order(trends$keyword),]
  trends$date = as.Date(trends$date)
  trends$hits = as.numeric(trends$hits)
  trends[is.na(trends)] = 0
  trends = as.data.table(trends)
  trends_cast = dcast(trends[,c(1,2,5)], date ~ keyword, value.var = 'hits')
  trends_cast[, lag7dayhits_buy := Reduce('+', shift(trends_cast[,2], c(2:7)))/6]
  trends_cast[, lag7dayhits_sell := Reduce('+', shift(trends_cast[,3], c(2:7)))/6]
  return(trends_cast)
}
#############
# suppose at t we have a headline news
# try to find the relationship between
# at t-7 the interest of "buy stock"
# at t-7 the interest of "sell stock"
# at t the interest of "buy stock"
# at t the interest of "sell stock"
# and the return of 1 week puchasing stock at t
#############
dowj = read.csv('dj.csv')
dowj = dowj[1:30,]

getSymbols(as.vector(dowj[,2]),src='yahoo')
djlist = list(MMM,  AXP,  AAPL, CAT,  CVX,  CSCO, DWDP, XOM,  INTC, IBM,  JNJ,  JPM,
              MCD,  MRK,  MSFT, NKE,  PFE,  BA,   KO,   GS,   HD,   PG,   TRV,  DIS,  UTX,
              UNH,  VZ,   V,    WBA,  WMT )
for (i in 1:30){
  djlist[[i]] = as.data.table(djlist[[i]])
  djlist[[i]][, shift1dayprice := shift(djlist[[i]][,7],-1)]
  djlist[[i]][, shift1weekprice := shift(djlist[[i]][,7],-5)]
  djlist[[i]][, ret1day := djlist[[i]][,8]/djlist[[i]][,7]-1]
  djlist[[i]][, ret1week := djlist[[i]][,9]/djlist[[i]][,7]-1]
}


# news = as.data.table(read.csv('abcnews-date-text.csv'))
# news$publish_date = as.Date(as.character(news$publish_date), '%Y%m%d')
news2 = as.data.table(read.csv('headlines_sample_1.csv', stringsAsFactors = F))[,c(2,4)]
colnames(news2) = c('Date', 'News')
news2$Date = as.Date(as.character(news2$Date), '%Y%m%d')
news2 = news2[order(news2$Date),]

# temp = grep('facebook', news$headline_text, value = T)
# fbnews = news[headline_text %in% temp,]
# fbnews = fbnews[publish_date >= FB$index[1],]
# fbnews = fbnews[!duplicated(fbnews$publish_date),]

words = as.vector(dowj[,1])
merged_news_return = list()
for (i in 1:30){
  temp = grep(words[i], news2$News, value = T)
  stocknews = news2[News %in% temp,]
  if (length(stocknews$Date) > 0){
    trends = get_gtrend(stocknews, words[i])
    news_trend_merge = merge(stocknews, trends, by.x = 'Date', by.y = 'date', all.x = T)
    merged = merge(news_trend_merge, djlist[[i]], by.x = 'Date', by.y = 'index')
    merged = merged[, c(1,3,4,5,6,15,16)]
    merged_news_return[[i]] = merged
  }
}


out = rbindlist(merged_news_return)
out[is.na(out)] = 0
colnames(out)[2:3] = c('attimebuy', 'attimesell')
out$delta_buy = out$attimebuy- out$lag7dayhits_buy
out$delta_sell = out$attimesell- out$lag7dayhits_sell
out$diff_delta = out$delta_buy-out$delta_sell

insample = out[Date < '2017-01-01',]
outsample = out[Date >= '2017-01-01',]
reg1 = lm(data = out, ret1day ~  attimebuy + attimesell + delta_buy + delta_sell)
summary(reg1)

reg2 = lm(data = out, ret1week ~  attimebuy + attimesell + delta_buy + delta_sell)
summary(reg2)

reg3 = lm(data = insample, ret1day ~  attimebuy + attimesell + delta_buy + delta_sell)
summary(reg3)

out$predict_ret1day = predict(reg1)

outsample$predict_ret1day = predict(reg3, outsample)

strat1 = out[,c(1,6,11)]
strat1 = strat1[order(strat1$Date),]
strat1 = as.data.table(strat1)
strat1[predict_ret1day >= 0, buy_or_sell := 1]
strat1[predict_ret1day < 0, buy_or_sell := -1]

#outsample strat
strat2 = outsample[,c(1,6,11)]
strat2 = strat2[order(strat2$Date),]
strat2 = as.data.table(strat2)
strat2[predict_ret1day >= 0, buy_or_sell := 1]
strat2[predict_ret1day < 0, buy_or_sell := -1]

getSymbols('DJI',src='yahoo')
#dowjone = as.data.table(read.csv('DJIA_table.csv'))
dowjone = as.data.table(DJI)
colnames(dowjone)[c(1,7)] = c('Date', 'Adj.Close')
dowjone = dowjone[order(dowjone$Date),]
dowjone$Date = as.Date(dowjone$Date)
dowjone1 = dowjone[Date >= '2013-01-11' & Date <= '2017-06-14',]
dowjone1$holding_ret = dowjone1$Adj.Close/dowjone1$Adj.Close[1]
dowjone2 = dowjone[Date >= '2017-01-01' & Date <= '2017-06-14',]
dowjone2$holding_ret = dowjone2$Adj.Close/dowjone2$Adj.Close[1]
strat_dj_merged = merge(dowjone1, strat1, by.x = 'Date', by.y = 'Date', all.x = T)
strat_dj_merged[, strat_ret := ret1day * buy_or_sell + 1]
strat_dj_merged[is.na(strat_ret), strat_ret := 1]
strat_dj_merged[, cum_ret := cumprod(strat_ret)]

strat_dj_merged2 = merge(dowjone2, strat2, by.x = 'Date', by.y = 'Date', all.x = T)
strat_dj_merged2[, strat_ret := ret1day * buy_or_sell + 1]
strat_dj_merged2[is.na(strat_ret), strat_ret := 1]
strat_dj_merged2[, cum_ret := cumprod(strat_ret)]

dates = sort(unique(strat_dj_merged$Date))
n = length(dates)
bnh_ret = rep(0, n)
my_strat_ret = rep(0,n)
for (i in 1:n){
  temp = strat_dj_merged[Date == dates[i],]
  bnh_ret[i] = temp$holding_ret[1]
  my_strat_ret[i] = mean(temp$strat_ret)
}
outcome = data.table(dates)
outcome = cbind(outcome, bnh_ret, my_strat_ret)
outcome[, cum_ret := cumprod(my_strat_ret)]

ggplot(data = outcome, aes(x = dates)) + geom_line(aes(y = bnh_ret, col = 'buy and hold')) +
  geom_line(aes(y = cum_ret, col = 'google trend strategy'))

dates = sort(unique(strat_dj_merged2$Date))
n = length(dates)
bnh_ret = rep(0, n)
my_strat_ret = rep(0,n)
for (i in 1:n){
  temp = strat_dj_merged2[Date == dates[i],]
  bnh_ret[i] = temp$holding_ret[1]
  my_strat_ret[i] = mean(temp$strat_ret)
}
outcome = data.table(dates)
outcome = cbind(outcome, bnh_ret, my_strat_ret)
outcome[, cum_ret := cumprod(my_strat_ret)]

ggplot(data = outcome, aes(x = dates)) + geom_line(aes(y = bnh_ret, col = 'buy and hold')) +
  geom_line(aes(y = cum_ret, col = 'google trend strategy')) + ggtitle('out of sample result')

