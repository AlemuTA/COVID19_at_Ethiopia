day=42
date="23/4/2020"


# ----------- Ethiopia  ---------------------------
dat <- read.csv("data/Ethio_COVID19_data.csv", header = TRUE)
dat <- dat[,c(3, 1)]
colnames(dat) <- c("day", "cases")
dat$day <- dat$day-1
dat$cuml <- cumsum(dat$cases)

ls <-loess(cases~day, data=dat,control=loess.control(surface="direct"))
day.pred <- seq(0, 50, 1)
pred <- predict(ls, newdata = data.frame(day=day.pred),
                se=TRUE)

png(paste0("Ethio_day", day,".png"), width = 18, height = 12, units = "cm", res = 300)
par(col="gray", mar=c(4, 4, 0.1, 0.1))
plot(dat$day, dat$cases, xlim = c(0, 50), ylim = c(0, 20), pch=20, las=1,
     ylab="# new cases", xlab="time since the first case (days)", col="deepskyblue2",
     main="")
abline(v=seq(0, 60, 2), h=seq(0, 20, 2), col="gray95", lty=3)
text(0, 18, labels = "COVID19 trend @Ethiopia.", pos = 4, col="black", cex=1.25)

lines(day.pred, sapply(pred$fit, function(x) max(0, x)), col=2, lwd=2)
lines(day.pred, pred$fit+pred$se.fit, lty=2, col="orange", lwd=1.5)
lines(day.pred, sapply(pred$fit-pred$se.fit, function(x) max(0, x)), lty=2, col="orange", lwd=1.5)
abline(v=day-1, lty=3, col="black")
text(day-2, 18, labels = paste0("today (", date,")"), pos = 2, col="black", srt=90, offset = 0.01, cex=0.75)
dev.off()



# ----------- Kenya ---------------------------
dat <- read.csv("data/Kenya_COVID19_data.csv", header = TRUE)
dat <- dat[,c(3, 1)]
colnames(dat) <- c("day", "cases")
dat$day <- dat$day-1
dat$cuml <- cumsum(dat$cases)

ls <-loess(cases~day, data=dat,control=loess.control(surface="direct"))
day.pred <- seq(0, 50, 1)
pred <- predict(ls, newdata = data.frame(day=day.pred),
                se=TRUE)

png(paste0("Kenya_day", day,".png"), width = 18, height = 12, units = "cm", res = 300)
par(col="gray", mar=c(4, 4, 0.1, 0.1))
plot(dat$day, dat$cases, xlim = c(0, 50), ylim = c(0, 35), pch=20, las=1,
     ylab="# new cases", xlab="time since the first case (days)", col="deepskyblue2",
     main="")
abline(v=seq(0, 60, 2), h=seq(0, 20, 2), col="gray95", lty=3)
text(0, 34, labels = "COVID19 trend @Kenya.", pos = 4, col="black", cex=1.25)

lines(day.pred, pred$fit, col=2, lwd=2)
lines(day.pred, pred$fit+pred$se.fit, lty=2, col="orange", lwd=1.5)
lines(day.pred, sapply(pred$fit-pred$se.fit, function(x) max(0, x)), lty=2, col="orange", lwd=1.5)
abline(v=day-1, lty=3, col="black")
text(day-2, 32, labels = paste0("today (", date,")"), pos = 2, col="black", srt=90, offset = 0.01, cex=0.75)
dev.off()



# ----------- Uganda ---------------------------
dat <- read.csv("data/Uganda_COVID19_data.csv", header = TRUE)
dat <- dat[,c(3, 2)]
colnames(dat) <- c("day", "cases")
dat$day <- dat$day-1
dat$cuml <- cumsum(dat$cases)

ls <-loess(cases~day, data=dat,control=loess.control(surface="direct"))
day.pred <- seq(0, 50, 1)
pred <- predict(ls, newdata = data.frame(day=day.pred),
                se=TRUE)

png(paste0("Uganda_day", day,".png"), width = 18, height = 12, units = "cm", res = 300)
par(col="gray", mar=c(4, 4, 0.1, 0.1))
plot(dat$day, dat$cases, xlim = c(0, 50), ylim = c(0, 20), pch=20, las=1,
     ylab="# new cases", xlab="time since the first case (days)", col="deepskyblue2",
     main="")
abline(v=seq(0, 60, 2), h=seq(0, 20, 2), col="gray95", lty=3)
text(0, 18, labels = "COVID19 trend @Uganda.", pos = 4, col="black", cex=1.25)

lines(day.pred, pred$fit, col=2, lwd=2)
lines(day.pred, pred$fit+pred$se.fit, lty=2, col="orange", lwd=1.5)
lines(day.pred, sapply(pred$fit-pred$se.fit, function(x) max(0, x)), lty=2, col="orange", lwd=1.5)
abline(v=day-1, lty=3, col="black")
text(day-2, 18, labels = paste0("today (", date,")"), pos = 2, col="black", srt=90, offset = 0.01, cex=0.75)
dev.off()



# ----------- Rwanda ---------------------------
dat <- read.csv("data/Rwand_COVID19_data.csv", header = TRUE)
dat <- dat[,c(3, 2)]
colnames(dat) <- c("day", "cases")
dat$day <- dat$day-1
dat$cuml <- cumsum(dat$cases)

ls <-loess(cases~day, data=dat,control=loess.control(surface="direct"))
day.pred <- seq(0, 50, 1)
pred <- predict(ls, newdata = data.frame(day=day.pred),
                se=TRUE)

png(paste0("Rwanda_day", day,".png"), width = 18, height = 12, units = "cm", res = 300)
par(col="gray", mar=c(4, 4, 0.1, 0.1))
plot(dat$day, dat$cases, xlim = c(0, 50), ylim = c(0, 20), pch=20, las=1,
     ylab="# new cases", xlab="time since the first case (days)", col="deepskyblue2",
     main="")
abline(v=seq(0, 60, 2), h=seq(0, 20, 2), col="gray95", lty=3)
text(0, 18, labels = "COVID19 trend @Rwanda.", pos = 4, col="black", cex=1.25)

lines(day.pred, sapply(pred$fit, function(x) max(0, x)), col=2, lwd=2)
lines(day.pred, pred$fit+pred$se.fit, lty=2, col="orange", lwd=1.5)
lines(day.pred, sapply(pred$fit-pred$se.fit, function(x) max(0, x)), lty=2, col="orange", lwd=1.5)
abline(v=day-1, lty=3, col="black")
text(day-2, 18, labels = paste0("today (", date,")"), pos = 2, col="black", srt=90, offset = 0.01, cex=0.75)
dev.off()



# ----------- Djibouti ---------------------------
dat <- read.csv("data/Djibuti_COVID19_data.csv", header = TRUE)
dat <- dat[,c(3, 2)]
colnames(dat) <- c("day", "cases")
dat$day <- dat$day-1
dat$cuml <- cumsum(dat$cases)

ls <-loess(cases~day, data=dat,control=loess.control(surface="direct"))
day.pred <- seq(0, 50, 1)
pred <- predict(ls, newdata = data.frame(day=day.pred),
                se=TRUE)

png(paste0("Djibouti_day", day,".png"), width = 18, height = 12, units = "cm", res = 300)
par(col="gray", mar=c(4, 4, 0.1, 0.1))
plot(dat$day, dat$cases, xlim = c(0, 50), ylim = c(0, 160), pch=20, las=1,
     ylab="# new cases", xlab="time since the first case (days)", col="deepskyblue2",
     main="")
abline(v=seq(0, 60, 2), h=seq(0, 20, 2), col="gray95", lty=3)
text(0, 145, labels = "COVID19 trend @Djibouti.", pos = 4, col="black", cex=1.25)

lines(day.pred, pred$fit, col=2, lwd=2)
lines(day.pred, pred$fit+pred$se.fit, lty=2, col="orange", lwd=1.5)
lines(day.pred, sapply(pred$fit-pred$se.fit, function(x) max(0, x)), lty=2, col="orange", lwd=1.5)
abline(v=day-1, lty=3, col="black")
text(day-2, 150, labels = paste0("today (", date,")"), 
     pos = 2, col="black", srt=90, offset = 0.01, cex=0.75)
dev.off()


# ----------- Tanzania ---------------------------
dat <- read.csv("data/Tanzania_COVID19_data.csv", header = TRUE)
dat <- dat[,c(3, 2)]
colnames(dat) <- c("day", "cases")
dat$day <- dat$day-1
dat$cuml <- cumsum(dat$cases)

ls <-loess(cases~day, data=dat,control=loess.control(surface="direct"))
day.pred <- seq(0, 50, 1)
pred <- predict(ls, newdata = data.frame(day=day.pred),
                se=TRUE)

png(paste0("Tanzania_day", day,".png"), width = 18, height = 12, units = "cm", res = 300)
par(col="gray", mar=c(4, 4, 0.1, 0.1))
plot(dat$day, dat$cases, xlim = c(0, 50), ylim = c(0, 100), pch=20, las=1,
     ylab="# new cases", xlab="time since the first case", col="deepskyblue2",
     main="")
abline(v=seq(0, 60, 2), h=seq(0, 20, 2), col="gray95", lty=3)
text(0, 95, labels = "COVID19 trend @Tanzania.", pos = 4, col="black", cex=1.25)

lines(day.pred, pred$fit, col=2, lwd=2)
lines(day.pred, pred$fit+pred$se.fit, lty=2, col="orange", lwd=1.5)
lines(day.pred, sapply(pred$fit-pred$se.fit, function(x) max(0, x)), lty=2, col="orange", lwd=1.5)
abline(v=day-1, lty=3, col="black")
text(day-2, 95, labels = paste0("today (", date,")"), pos = 2, col="black", srt=90, offset = 0.01, cex=0.75)
dev.off()


# ----------- Somalia ---------------------------
dat <- read.csv("data/Somalia_COVID19_data.csv", header = TRUE)
dat <- dat[,c(3, 2)]
colnames(dat) <- c("day", "cases")
dat$day <- dat$day-1
dat$cuml <- cumsum(dat$cases)

ls <-loess(cases~day, data=dat,control=loess.control(surface="direct"))
day.pred <- seq(0, 50, 1)
pred <- predict(ls, newdata = data.frame(day=day.pred),
                se=TRUE)

png(paste0("Somalia_day", day,".png"), width = 18, height = 12, units = "cm", res = 300)
par(col="gray", mar=c(4, 4, 0.1, 0.1))
plot(dat$day, dat$cases, xlim = c(0, 50), ylim = c(0, 160), pch=20, las=1,
     ylab="# new cases", xlab="time since the first case (days)", col="deepskyblue2",
     main="")
abline(v=seq(0, 60, 2), h=seq(0, 20, 2), col="gray95", lty=3)
text(0, 145, labels = "COVID19 trend @Somalia.", pos = 4, col="black", cex=1.25)

lines(day.pred, pred$fit, col=2, lwd=2)
lines(day.pred, pred$fit+pred$se.fit, lty=2, col="orange", lwd=1.5)
lines(day.pred, sapply(pred$fit-pred$se.fit, function(x) max(0, x)), lty=2, col="orange", lwd=1.5)
abline(v=day-1, lty=3, col="black")
text(day-2, 150, labels = paste0("today (", date,")"), 
     pos = 2, col="black", srt=90, offset = 0.01, cex=0.75)
dev.off()


# ----------- Sudan  ---------------------------
dat <- read.csv("data/Sudan_COVID19_data.csv", header = TRUE)
dat <- dat[,c(3, 1)]
colnames(dat) <- c("day", "cases")
dat$day <- dat$day-1
dat$cuml <- cumsum(dat$cases)

ls <-loess(cases~day, data=dat,control=loess.control(surface="direct"))
day.pred <- seq(0, 50, 1)
pred <- predict(ls, newdata = data.frame(day=day.pred),
                se=TRUE)

png(paste0("Sudan_day", day,".png"), width = 18, height = 12, units = "cm", res = 300)
par(col="gray", mar=c(4, 4, 0.1, 0.1))
plot(dat$day, dat$cases, xlim = c(0, 50), ylim = c(0, 50), pch=20, las=1,
     ylab="# new cases", xlab="time since the first case (days)", col="deepskyblue2",
     main="")
abline(v=seq(0, 60, 2), h=seq(0, 20, 2), col="gray95", lty=3)
text(0, 40, labels = "COVID19 trend @Sudan.", pos = 4, col="black", cex=1.25)

lines(day.pred, sapply(pred$fit, function(x) max(0, x)), col=2, lwd=2)
lines(day.pred, pred$fit+pred$se.fit, lty=2, col="orange", lwd=1.5)
lines(day.pred, sapply(pred$fit-pred$se.fit, function(x) max(0, x)), lty=2, col="orange", lwd=1.5)
abline(v=day-1, lty=3, col="black")
text(day-2, 36, labels = paste0("today (", date,")"), pos = 4, col="black", srt=90, offset = 0.01, cex=0.75)
dev.off()
