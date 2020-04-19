dat <- read.csv("data/Ethio_COVID19_data.csv", header = TRUE)
dat <- dat[,c(4, 2)]
colnames(dat) <- c("day", "cases")
dat$day <- dat$day-1
dat$cuml <- cumsum(dat$cases)


# The following lines fit a simple smoothing regression (LOESS), which requires careful interpretation
ls <-loess(cases~day, data=dat,control=loess.control(surface="direct"))
day.pred <- seq(0, 60, 1)
pred <- predict(ls, newdata = data.frame(day=day.pred), se=TRUE)

png("day38.png", width = 18, height = 12, units = "cm", res = 300)
par(col="gray", mar=c(4, 4, 0.1, 0.1))
plot(dat$day, dat$cases, xlim = c(0, 60), ylim = c(0, 20), pch=20, las=1,
     ylab="# new cases", xlab="time since first case", col="deepskyblue2",
     main="")
abline(v=seq(0, 60, 2), h=seq(0, 20, 2), col="gray95", lty=3)
text(0, 18, labels = "COVID19 trend @Ethiopia.", pos = 4, col="black", cex=1.25)

lines(day.pred, pred$fit, col=2, lwd=2)
lines(day.pred, pred$fit+pred$se.fit, lty=2, col="orange", lwd=1.5)
lines(day.pred, sapply(pred$fit-pred$se.fit, function(x) max(0, x)), lty=2, col="orange", lwd=1.5)
abline(v=37, lty=3, col="black")
text(36, 18, labels = "today (19/2020)", pos = 2, col="black", srt=90, offset = 0.01, cex=0.75)
dev.off()
