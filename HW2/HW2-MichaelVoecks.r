#Import Chess Scores
chessRatings <- read.table("./ChessRatingComparison.csv", sep=",", header=TRUE)
p = length(chessRatings)

#Pairwise plots against USCF regular rating
myvars <- c("USCF.Regular.Rating", "Chess.com.Live.Standard.Rating", "Chess.com.Live.Blitz.Rating", "Chess.com.Turn.Based.Standard", "USCF.Quick.Rating", "FIDE.Regular.Rating")
stripped <- chessRatings[myvars]
pairs(stripped)

#Model y = B0 + B1*X + e
chessRatings.lm <- lm(USCF.Regular.Rating~Chess.com.Live.Standard.Rating, data=chessRatings)
summary(chessRatings.lm)
confint(chessRatings.lm, level=0.95)

#Diagnostics
par(mfrow=c(2,2))
plot(chessRatings.lm)

#Remove outliers, new plot
chessRatings.lm.out <- lm(USCF.Regular.Rating~Chess.com.Live.Standard.Rating, data=chessRatings[-339,])
summary(chessRatings.lm.out)
confint(chessRatings.lm.out, level=0.95)

#Predict USCF ratings with Chess.com 1600 score (confidence and predict intervals)
predict.lm(chessRatings.lm.out, newdata=data.frame(Chess.com.Live.Standard.Rating=c(1600)), interval="confidence")
predict.lm(chessRatings.lm.out, newdata=data.frame(Chess.com.Live.Standard.Rating=c(1600)), interval="predict")

#Compute anova on all scores
cregular <- chessRatings.lm
cblitz <- lm(USCF.Regular.Rating~Chess.com.Live.Blitz.Rating, data=chessRatings)
cbullet <- lm(USCF.Regular.Rating~Chess.com.Live.Bullet.Rating, data=chessRatings)
cturnbased <- lm(USCF.Regular.Rating~Chess.com.Turn.Based.Standard, data=chessRatings)
uquick <- lm(USCF.Regular.Rating~USCF.Quick.Rating, data=chessRatings)
fregular <- lm(USCF.Regular.Rating~FIDE.Regular.Rating, data=chessRatings)

anova(cregular)
anova(cblitz)
anova(cbullet)
anova(cturnbased)
anova(uquick)
anova(fregular)
