data <- data.frame(x(), y())
model <- lm(y ~ x, data = data)
scatter <- par(mfrow=c(1,2))
plot(data,
     pch=20,
     col="blue",
     main="Scatter Plot",
     xlab="x values",
     ylab="y values")
abline(lm(y~x, col="red")
       plot(
         fitted(model), resid(model),
         main = "Residual Plot",
         xlab = "Fitted Values",
         ylab = "Residuals",
         pch = 20, col = "blue"
       )
       abline(h = 0, col = "red", lwd = 2)
       print(scatter)


       output$scatter<- renderPlot({
         x<-x()
         y<-y)_
       data <- data.frame(x,y)
       model <- lm(y ~ x,data=data)
       scatter <- par(mfrow=c(1,2))
       plot(data,
            pch=20,
            col="blue",
            main="Scatter Plot",
            xlab="x values",
            ylab="y values")
       abline(lm(y~x, col="red")
              plot(
                fitted(model), resid(model),
                main = "Residual Plot",
                xlab = "Fitted Values",
                ylab = "Residuals",
                pch = 20, col = "blue"
              )
              abline(h = 0, col = "red", lwd = 2)
              print(scatter)
       })
