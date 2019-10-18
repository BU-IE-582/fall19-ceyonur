install.packages("data.table")
library(data.table)

# TASK 1.1
matches <- fread("/home/ceyonur/git/CMPE/fall19-ceyonur/data/homework1/matches.csv", select=c("match_id", "league_id","match_status","match_hometeam_score", "match_awayteam_score"))
epl_matches <- matches[league_id == 148 & match_status == "Finished"]
h1 <- hist(epl_matches$match_hometeam_score, breaks = seq(0, max(epl_matches$match_hometeam_score),1),
            xlab="Home Score(goals)", ylab="Number of Games", col = "red", main = "Home Score Histogram") # a)
xfit<-seq(min(epl_matches$match_hometeam_score),max(epl_matches$match_hometeam_score),length=40)
yfit<-dnorm(xfit,mean=mean(epl_matches$match_hometeam_score),sd=sd(epl_matches$match_hometeam_score))
yfit <- yfit*diff(h1$mids[1:2])*length(epl_matches$match_hometeam_score)
lines(xfit, yfit, col="blue", lwd=2)

h2 <- hist(epl_matches$match_awayteam_score, breaks = seq(0, max(epl_matches$match_awayteam_score),1),
            xlab="Away Score(goals)", ylab="Number of Games", col = "red", main = "Away Score Histogram") # b)
xfit<-seq(min(epl_matches$match_awayteam_score),max(epl_matches$match_awayteam_score),length=40)
yfit<-dnorm(xfit,mean=mean(epl_matches$match_awayteam_score),sd=sd(epl_matches$match_awayteam_score))
yfit <- yfit*diff(h2$mids[1:2])*length(epl_matches$match_awayteam_score)
lines(xfit, yfit, col="blue", lwd=2)

epl_diff <- epl_matches$match_hometeam_score - epl_matches$match_awayteam_score
h3 <- hist(epl_diff, breaks = seq(min(epl_diff), max(epl_diff),1),
            xlab="Home Goals - Away Goals", ylab="Number of Games", col = "red", main = "Home - Away Score Histogram") # c)

xfit<-seq(min(epl_diff),max(epl_diff),length=40)
yfit<-dnorm(xfit,mean=mean(epl_diff),sd=sd(epl_diff))
yfit <- yfit*diff(h3$mids[1:2])*length(epl_diff)
lines(xfit, yfit, col="blue", lwd=2)

bets <- fread("/home/ceyonur/git/CMPE/fall19-ceyonur/data/homework1/bets.csv")
epl_matches <- epl_matches[, is_draw := as.numeric(match_awayteam_score == match_hometeam_score)]
bets <- bets[variable %in% c("odd_1", "odd_x", "odd_2" )]
merged_data <- merge(epl_matches, bets, by='match_id')
bookmaker_data_count <- bets[, .N, by = c("odd_bookmakers")]
bookmaker_data_count <- bookmaker_data_count[order(-N)]
bookmaker_names <- bookmaker_data_count[1:4]$odd_bookmakers

bookmaker_draw_analysis <- function(bookmaker_name, data) {
  bookmaker_bets <- data[odd_bookmakers == bookmaker_name]
  bookmaker_bets <- bookmaker_bets[, c("p_win", "p_draw", "p_lose") := list(1/value[variable == 'odd_1'], 1/value[variable == 'odd_x'], 1/value[variable == 'odd_2']),
                    by = list(match_id, odd_bookmakers, variable)]
  bookmaker_bets <- aggregate(bookmaker_bets[,c("is_draw", "p_win","p_draw","p_lose")], by=list(bookmaker_bets$match_id, bookmaker_bets$odd_bookmakers),
                          FUN = function (x) first(na.omit(x)))
  bookmaker_bets <- data.table(bookmaker_bets)
  names(bookmaker_bets)[1] <- "match_id"
  names(bookmaker_bets)[2] <- "odd_bookmakers"
  # TASK 2.2
  bookmaker_bets[, p_win_imp:=p_win / (p_win + p_draw + p_lose)]
  bookmaker_bets[, p_draw_imp:=p_draw / (p_win + p_draw + p_lose)]
  bookmaker_bets[, p_lose_imp:=p_lose / (p_win + p_draw + p_lose)]
  bookmaker_bets[, p_win_lose_imp := p_win_imp - p_lose_imp]
}

bookmaker_bin_table <- function(bookmaker_bets){
  # TASK 2.3
  cutpoints <- seq(min(bookmaker_bets$p_win_lose_imp) - 0.06, max(bookmaker_bets$p_win_lose_imp) + 0.06, 0.05)
  bookmaker_bets[,w_l_cut:=cut(p_win_lose_imp, cutpoints)]
  bookmaker_bin_table <- bookmaker_bets[,list(emprical_over=mean(is_draw), probabilistic_over=mean(p_draw_imp),draw_count=sum(is_draw), total_count=.N),by=list(w_l_cut)]
  bookmaker_bin_table <- bookmaker_bin_table[order(w_l_cut)]
}

# 1st
first_bookmaker_name <- bookmaker_names[1]
# Task 2.1-2
first_bets <- bookmaker_draw_analysis(first_bookmaker_name, merged_data)
first_bin_table <- bookmaker_bin_table(first_bets)
# Task 2.3
plot(first_bets$p_win_imp - first_bets$p_lose_imp, first_bets$p_draw_imp,cex=0.5,col='red',xlab="P(WIN_IMP) - P(LOSE_IMP)", ylab="P(DRAW_IMP)", main = first_bookmaker_name)
# Task 2.4
plot(first_bin_table$w_l_cut, first_bin_table$emprical_over,ylab="P(DRAW)",xlab="bin", main = first_bookmaker_name)
lines(lowess(first_bin_table$w_l_cut, first_bin_table$emprical_over), col="black", lwd=1)
lines(lowess(first_bin_table$w_l_cut, first_bin_table$probabilistic_over), col="red", lwd=1)
legend("topright", legend=c("Calculated Probability", "Real Probability"), col=c("black","red"), lty=1:1, cex=0.8)

# 2nd
second_bookmaker_name <- bookmaker_names[2]
# Task 2.1-2
second_bets <- bookmaker_draw_analysis(second_bookmaker_name, merged_data)
second_bin_table <- bookmaker_bin_table(second_bets)
# Task 2.3
plot(second_bets$p_win_imp - second_bets$p_lose_imp, second_bets$p_draw_imp,cex=0.5,col='red',xlab="P(WIN_IMP) - P(LOSE_IMP)", ylab="P(DRAW_IMP)", main = second_bookmaker_name)
# Task 2.4
plot(second_bin_table$w_l_cut, second_bin_table$emprical_over,ylab="P(DRAW)",xlab="bin", main = second_bookmaker_name)
lines(lowess(second_bin_table$w_l_cut, second_bin_table$emprical_over), col="black", lwd=1)
lines(lowess(second_bin_table$w_l_cut, second_bin_table$probabilistic_over), col="red", lwd=1)
legend("topright", legend=c("Calculated Probability", "Real Probability"), col=c("black","red"), lty=1:1, cex=0.8)

# 3rd
third_bookmaker_name <- bookmaker_names[3]
# Task 2.1-2
third_bets <- bookmaker_draw_analysis(third_bookmaker_name, merged_data)
third_bin_table <- bookmaker_bin_table(third_bets)
# Task 2.3
plot(third_bets$p_win_imp - third_bets$p_lose_imp, third_bets$p_draw_imp,cex=0.5,col='red',xlab="P(WIN_IMP) - P(LOSE_IMP)", ylab="P(DRAW_IMP)", main = third_bookmaker_name)
# Task 2.4
plot(third_bin_table$w_l_cut, third_bin_table$emprical_over,ylab="P(DRAW)",xlab="bin", main = third_bookmaker_name)
lines(lowess(third_bin_table$w_l_cut, third_bin_table$emprical_over), col="black", lwd=1)
lines(lowess(third_bin_table$w_l_cut, third_bin_table$probabilistic_over), col="red", lwd=1)
legend("topright", legend=c("Calculated Probability", "Real Probability"), col=c("black","red"), lty=1:1, cex=0.8)

# 4th
fourth_bookmaker_name <- bookmaker_names[4]
# Task 2.1-2
fourth_bets <- bookmaker_draw_analysis(fourth_bookmaker_name, merged_data)
fourth_bin_table <- bookmaker_bin_table(fourth_bets)
# Task 2.3
plot(fourth_bets$p_win_imp - fourth_bets$p_lose_imp, fourth_bets$p_draw_imp,cex=0.5,col='red',xlab="P(WIN_IMP) - P(LOSE_IMP)", ylab="P(DRAW_IMP)", main = fourth_bookmaker_name)
# Task 2.4
plot(fourth_bin_table$w_l_cut, fourth_bin_table$emprical_over,ylab="P(DRAW)",xlab="bin", main = fourth_bookmaker_name)
lines(lowess(fourth_bin_table$w_l_cut, fourth_bin_table$emprical_over), col="black", lwd=1)
lines(lowess(fourth_bin_table$w_l_cut, fourth_bin_table$probabilistic_over), col="red", lwd=1)
legend("topright", legend=c("Calculated Probability", "Real Probability"), col=c("black","red"), lty=1:1, cex=0.8)

# Task 3
goals <- fread("/home/ceyonur/git/CMPE/fall19-ceyonur/data/homework1/goals.csv")
goals <- goals[time > 90]
goals <- goals[,c("home_score", "away_score") := tstrsplit(score, '-', type.convert = TRUE)]
goals <- goals[, match_hometeam_endtime_score := home_score - (as.integer(home_scorer != ""))]
goals <- goals[, match_awayteam_endtime_score := away_score - (as.integer(away_scorer != ""))]
goals <- unique(goals, by="match_id")
merged <- merge(epl_matches, goals[, c("match_id", "match_hometeam_endtime_score", "match_awayteam_endtime_score")], all.x= TRUE)
merged <- merged[, match_hometeam_endtime_score := ifelse(is.na(match_hometeam_endtime_score),match_hometeam_score, match_hometeam_endtime_score)]
merged <- merged[, match_awayteam_endtime_score := ifelse(is.na(match_awayteam_endtime_score),match_awayteam_score, match_awayteam_endtime_score)]
merged <- merged[, match_score_status := match_hometeam_score - match_awayteam_score]
merged <- merged[, match_score_status := ifelse(match_score_status == 0, 0, ifelse(match_score_status > 0, 1, -1))]
merged <- merged[, match_endtime_score_status := match_hometeam_endtime_score - match_awayteam_endtime_score]
merged <- merged[, match_endtime_score_status := ifelse(match_endtime_score_status == 0, 0, ifelse(match_endtime_score_status > 0, 1, -1))]
excluded_endtime_matches <- merged[match_endtime_score_status != match_score_status]
bookings <- fread("/home/ceyonur/git/CMPE/fall19-ceyonur/data/homework1/booking.csv")
excluded_bookings <- bookings[card == "red card" & time <= 15]
excluded_match_ids <- unique(c(excluded_endtime_matches$match_id, excluded_bookings$match_id))
'%!in%' <- function(x,y)!('%in%'(x,y))
excluded_merged_data = merged_data[match_id %!in% excluded_match_ids]

# 1st
first_excluded_bookmaker_name <- bookmaker_names[1]
# Task 2.1-2
first_excluded_bets <- bookmaker_draw_analysis(first_excluded_bookmaker_name, excluded_merged_data)
first_excluded_bin_table <- bookmaker_bin_table(first_excluded_bets)
# Task 2.3
plot(first_excluded_bets$p_win_imp - first_excluded_bets$p_lose_imp, first_excluded_bets$p_draw_imp,cex=0.5,col='red',xlab="P(WIN_IMP) - P(LOSE_IMP)", ylab="P(DRAW_IMP)", main = first_excluded_bookmaker_name)
# Task 2.4
plot(first_excluded_bin_table$w_l_cut, first_excluded_bin_table$emprical_over,ylab="P(DRAW)",xlab="bin", main = first_excluded_bookmaker_name)
lines(lowess(first_excluded_bin_table$w_l_cut, first_excluded_bin_table$emprical_over), col="black", lwd=1)
lines(lowess(first_excluded_bin_table$w_l_cut, first_excluded_bin_table$probabilistic_over), col="red", lwd=1)
legend("topright", legend=c("Calculated Probability", "Real Probability"), col=c("black","red"), lty=1:1, cex=0.8)

# 2nd
second_excluded_bookmaker_name <- bookmaker_names[2]
# Task 2.1-2
second_excluded_bets <- bookmaker_draw_analysis(second_excluded_bookmaker_name, excluded_merged_data)
second_excluded_bin_table <- bookmaker_bin_table(second_excluded_bets)
# Task 2.3
plot(second_excluded_bets$p_win_imp - second_excluded_bets$p_lose_imp, second_excluded_bets$p_draw_imp,cex=0.5,col='red',xlab="P(WIN_IMP) - P(LOSE_IMP)", ylab="P(DRAW_IMP)", main = second_excluded_bookmaker_name)
# Task 2.4
plot(second_excluded_bin_table$w_l_cut, second_excluded_bin_table$emprical_over,ylab="P(DRAW)",xlab="bin", main = second_excluded_bookmaker_name)
lines(lowess(second_excluded_bin_table$w_l_cut, second_excluded_bin_table$emprical_over), col="black", lwd=1)
lines(lowess(second_excluded_bin_table$w_l_cut, second_excluded_bin_table$probabilistic_over), col="red", lwd=1)
legend("topright", legend=c("Calculated Probability", "Real Probability"), col=c("black","red"), lty=1:1, cex=0.8)

# 3rd
third_excluded_bookmaker_name <- bookmaker_names[3]
# Task 2.1-2
third_excluded_bets <- bookmaker_draw_analysis(third_excluded_bookmaker_name, excluded_merged_data)
third_excluded_bin_table <- bookmaker_bin_table(third_excluded_bets)
# Task 2.3
plot(third_excluded_bets$p_win_imp - third_excluded_bets$p_lose_imp, third_excluded_bets$p_draw_imp,cex=0.5,col='red',xlab="P(WIN_IMP) - P(LOSE_IMP)", ylab="P(DRAW_IMP)", main = third_excluded_bookmaker_name)
# Task 2.4
plot(third_excluded_bin_table$w_l_cut, third_excluded_bin_table$emprical_over,ylab="P(DRAW)",xlab="bin", main = third_excluded_bookmaker_name)
lines(lowess(third_excluded_bin_table$w_l_cut, third_excluded_bin_table$emprical_over), col="black", lwd=1)
lines(lowess(third_excluded_bin_table$w_l_cut, third_excluded_bin_table$probabilistic_over), col="red", lwd=1)
legend("topright", legend=c("Calculated Probability", "Real Probability"), col=c("black","red"), lty=1:1, cex=0.8)

# 4th
fourth_excluded_bookmaker_name <- bookmaker_names[4]
# Task 2.1-2
fourth_excluded_bets <- bookmaker_draw_analysis(fourth_excluded_bookmaker_name, excluded_merged_data)
fourth_excluded_bin_table <- bookmaker_bin_table(fourth_excluded_bets)
# Task 2.3
plot(fourth_excluded_bets$p_win_imp - fourth_excluded_bets$p_lose_imp, fourth_excluded_bets$p_draw_imp,cex=0.5,col='red',xlab="P(WIN_IMP) - P(LOSE_IMP)", ylab="P(DRAW_IMP)", main = fourth_excluded_bookmaker_name)
# Task 2.4
plot(fourth_excluded_bin_table$w_l_cut, fourth_excluded_bin_table$emprical_over,ylab="P(DRAW)",xlab="bin", main = fourth_excluded_bookmaker_name)
lines(lowess(fourth_excluded_bin_table$w_l_cut, fourth_excluded_bin_table$emprical_over), col="black", lwd=1)
lines(lowess(fourth_excluded_bin_table$w_l_cut, fourth_excluded_bin_table$probabilistic_over), col="red", lwd=1)
legend("topright", legend=c("Calculated Probability", "Real Probability"), col=c("black","red"), lty=1:1, cex=0.8)
