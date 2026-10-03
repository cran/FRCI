#' @title Plot for "psi" type objects.
#'
#' @param x A "psi" type object, list containing the following information.
#' \describe{
#'   \item{[[1]]}{the membership function}
#'   \item{[[2]]}{the name of the sample distribution}
#'   \item{[[3]]}{sample size}
#'   \item{[[4]]}{the name of method}
#'   \item{[[5]]}{standard deviation (for the case of normal distribution).}
#'   \item{[[6]]}{the upper and lower bound function of the support of the membership function.}
#' }
#'@param ... Other objects of the "psi" type.
#'@param gamma the confidence level.
#'@param xlim A vector containing the upper and lower values for the x-axis of the plot.
#'@param tlim A vector containing the upper and lower values for theta.
#'@param yval A vector containing the values of omega to be included in the plot. (for type = "psi" only)
#'@param ylim A vector containing the upper and lower values for the y-axis of the plot. (for type = "length" only)
#'@param type The type of the plot, by default is the menbership function, and "length" for the expected length plot.
#'
#'@return No return value, called for side effects
#'
#'@examples
#'x<-psi(distribution = "bernoulli", method = "GM", n = 10)
#'plot(x)
#'plot(x, type = "length")
#'
#'x<-psi(distribution = "bernoulli", method = 0.5, n = 10)
#'plot(x)
#'plot(x, type = "length")
#'@export



plot.psi <- function(x,...,
                     gamma = 0.95,
                     type = "psi",
                     xlim = NULL,
                     tlim = NULL,
                     yval = NULL,
                     ylim = NULL) {

  oldpar <- par(no.readonly = TRUE) # code line i
  on.exit(par(oldpar)) # code line i + 1
  abc<-list(...)

  c25 <- c(
    "black",
    "dodgerblue2",
    "#E31A1C",
    # red
    "green4",
    "#6A3D9A",
    # purple
    "#FF7F00",
    # orange
    "gold1",
    "skyblue2",
    "#FB9A99",
    # lt pink
    "palegreen2",
    "#CAB2D6",
    # lt purple
    "#FDBF6F",
    # lt orange
    "gray70",
    "khaki2",
    "maroon",
    "orchid1",
    "deeppink1",
    "blue1",
    "steelblue4",
    "darkturquoise",
    "green1",
    "yellow4",
    "yellow3",
    "darkorange4",
    "brown"
  )

  pl <- NULL
  pl[[1]] <- (list(x))

  test <- abc[sapply(abc, class) == "psi"]
  testplot <- abc[sapply(abc, class) != "psi"]


  if (type == "psi")
  {
    npt <- 1000
    rf <- 4
  } else if (type == "length")
  {
    npt <- 100
    rf <- 3
  } else
  {
    stop("Invalid type.")
  }

  cl <- c(x[[rf]])



  for (ps in test) {
    for (i in 1:length(cl)) {
      if (ps[[rf]] == cl[i]) {
        pl[[i]][[length(pl[[i]]) + 1]] <- ps
        break
      }
      if (i == length(cl)) {
        cl <- c(cl, ps[[rf]])
        pl[length(pl) + 1] <- list(list(ps))
      }
    }

  }





  ######################################################################################################################

  dist0 <- x[[2]]
  {
    #Define the x_values
    if (is.null(xlim) & is.null(tlim))
    {
      if (dist0 == "normal")
      {
        sigma <- x[[5]]
        th <- (-floor(npt / 2):floor(npt / 2)) / npt * 8 * sigma
      }
      else if (dist0 == "bernoulli")
      {
        th <- (1:npt) / (npt + 1)
      }
      else if (dist0 == "poisson")
      {
        th <- (1:npt) / (npt + 1) * 10
      }
    }
    else if (is.null(xlim) & !is.null(tlim))
    {
      if (tlim[1] > tlim[2] |
          (dist0 == "bernoulli" & (tlim[1] < 0 | tlim[2] > 1)) |
          (dist0 == "poisson" & (tlim[1] < 0)))
      {
        stop("Invalid tlim value.")
      }
      th <- tlim[1] + (1:npt) / (npt + 1) * (tlim[2] - tlim[1])
    }
    else if (!is.null(xlim) & is.null(tlim))
    {
      if (xlim[1] > xlim[2] |
          (dist0 == "bernoulli" & (xlim[1] < 0 | xlim[2] > 1)) |
          (dist0 == "poisson" & (xlim[1] < 0)))
      {
        stop("Invalid xlim value.")
      }
      th <- xlim[1] + (1:npt) / (npt + 1) * (xlim[2] - xlim[1])
    }
    else if (!is.null(xlim) & !is.null(tlim))
    {
      if (tlim[1] > tlim[2] |
          (dist0 == "bernoulli" & (tlim[1] < 0 | tlim[2] > 1)) |
          (dist0 == "poisson" & (tlim[1] < 0)))
      {
        stop("Invalid tlim value.")
      }
      if (xlim[1] > xlim[2] |
          (dist0 == "bernoulli" & (xlim[1] < 0 | xlim[2] > 1)) |
          (dist0 == "poisson" & (xlim[1] < 0)))
      {
        stop("Invalid xlim value.")
      }
      if (xlim[2] < tlim[1] | xlim[1] > tlim[2])
      {
        stop("Invalid xlim and tlim values.")
      }
      th <- max(xlim[1], tlim[1]) + (1:npt) / (npt + 1) * (min(xlim[2], tlim[2]) -
                                                             max(xlim[1], tlim[1]))
    }
  }#Define the x_values
  name <- round(min(th) + (0:10) * (max(th) - min(th)) / 10, digits = 2)
  if (type == "psi")
  {
    {
      #Define y values
      if (is.null(yval))
      {
        if (dist0 == "normal")
        {
          om <- -4:4
        }
        else if (dist0 == "bernoulli")
        {
          om <- 0:min(9, x[[3]])
        }
        else if (dist0 == "poisson")
        {
          om <- 0:9
        }
      }
      else
      {
        om <- yval
      }
    }#Define y values
    for (ps in pl)
    {
      plot(
        -1,-1,
        type = "n",
        xlim = c(min(th), max(th)),
        ylim = c(.5, length(om) + .5),
        xaxt = "n",
        yaxt = "n",
        ann = FALSE
      )
      title(main = ps[[1]][[4]])
      mtext(expression(psi * group("(", tau * "|" * omega, ")")), side = 2, line = 3)
      axis(
        1,
        at = name ,
        labels = name,
        las = 1,
        cex.axis = 0.8
      )
      mtext(expression(tau), side = 1, line = 2)
      i <- 0
      nomem <- NULL
      for (pp in ps)
      {
        i <- i + 1
        fpsi <- pp[[1]]
        dist <- pp[[2]]
        n <- pp[[3]]
        met <- pp[[4]]
        nomem <- c(nomem, paste("n =", n))
        sup <- pp[[6]]
        if (dist == "normal") {
          sigma <- pp[[5]]
        }
        for (o in 1:length(om))
        {
          if (dist == "bernoulli" & n >= om[o])
          {
            fu <- fpsi(th, om[o], g = gamma)
            lines(
              th,
              fu * 0.8 - 0.4 + o,
              col = c25[i],
              lty = i,
              lwd = 2 * .95^i
            )
          }
          else if (dist != "bernoulli")
          {
            fu <- fpsi(th, om[o], g = gamma)
            lines(
              th,
              fu * 0.8 - 0.4 + o,
              col = c25[i],
              lty = i,
              lwd = 2 * .95^i
            )
          }
        }
      }
      for (o in 1:length(om))
      {
        axis(
          2,
          at = o,
          labels = substitute(omega == x, list(x = om[o])),
          las = 2
        )
      }
      legend(
        "bottomright",
        legend = nomem,
        col = c25[1:i],
        lty = 1:i,
        cex = 0.8,
        bg = "white",
        lwd = 2
      )
    }
  } else if (type == "length")
  {
    for (ps in pl) {
      length(ps)
      i <- 0
      nmet <- NULL
      for (pp in ps) {
        i <- i + 1
        fpsi <- pp[[1]]
        dist <- pp[[2]]
        n <- pp[[3]]
        met <- pp[[4]]
        nmet <- c(nmet, met)
        sup <- pp[[6]]
        if (dist == "normal") {
          sigma <- pp[[5]]
          {
            normalTE <- function(t, gamma, n, sigma2,lim=NULL) {
              s2 <- sigma2 / n
              s <- sqrt(s2)
              l <- qnorm((1 + gamma) / 2) * sqrt(sigma2 / n)
              if(is.null(lim))
              {
                2*sqrt(sigma2/n)*qnorm((1+gamma)/2)+0*th
              }
              else
              {
                a<-lim[1]
                b<-lim[2]
                ifelse(
                  b - l > a + l+0*t,
                  (t - a + l) * (pnorm((a + l - t) / s) - pnorm((a - l - t) / s)) +
                    s * (dnorm((a - l - t) / s) - dnorm((a + l - t) / s)) +
                    2 * l * (pnorm((b - l - t) / s) - pnorm((a + l - t) / s)) +
                    (b + l - t) * (pnorm((b + l - t) / s) - pnorm((b - l - t) / s)) -
                    s * (dnorm((b - l - t) / s) - dnorm((b + l - t) / s)),
                  (t - a + l) * (pnorm((b - l - t) / s) - pnorm((a - l - t) / s)) +
                    s * (dnorm((a - l - t) / s) - dnorm((b - l - t) / s)) +
                    (b - a) * (pnorm((a + l - t) / s) - pnorm((b - l - t) / s)) +
                    (b + l - t) * (pnorm((b + l - t) / s) - pnorm((a + l - t) / s)) -
                    s * (dnorm((a + l - t) / s) - dnorm((b + l - t) / s)))
              }
            }

            normaloTE <- function(t, o, gamma, n, sigma2,lim=NULL) {
              s2 <- sigma2 / n
              s <- sqrt(s2)
              l <- qnorm(gamma) * sqrt(sigma2 / n)
              if(is.null(lim))
              {
                auxf <- function(x) {
                  x * pnorm(x) + dnorm(x)
                }
                sqrt(sigma2 / n) * (auxf(qnorm(gamma) + (o - t) / sigma2 * sqrt(n)) +
                                      auxf(qnorm(gamma) - (o - t) / sigma2 * sqrt(n)))
              }
              else
              {
                a<-lim[1]
                b<-lim[2]
                (b - o) * (1 - pnorm((b - l - t) / s)) +
                  (o - a) * pnorm((a + l - t) / s) +
                  (t - o + l) * (pnorm((b - l - t) / s) - pnorm((o - l - t) / s)) +
                  s * (dnorm((o - l - t) / s) - dnorm((b - l - t) / s)) +
                  (o + l - t) * (pnorm((o + l - t) / s) - pnorm((a + l - t) / s)) -
                  s * (dnorm((a + l - t) / s) - dnorm((o + l - t) / s))
              }
            }
          }
          if(startsWith(met,"Felix"))
          {
            auxv2<-unlist(strsplit(met," "))
            fval<-normaloTE(t = th,o = as.numeric(auxv2[length(auxv2)]),gamma = gamma,n = n,sigma2 = sigma^2,lim = tlim)
          }
          else if(startsWith(met,"Geyer"))
          {
            fval<-normalTE(t = th,gamma = gamma,n = n,sigma2 = sigma^2,lim = tlim)
          }

          if (i == 1) {
            Tmin<-normaloTE(t = th,o = th,gamma = gamma,n = n,sigma2 = sigma^2,lim = tlim)
            if (is.null(ylim))
            {
              ylim <- c(0, 1.5 * max(Tmin))
            }
            xlim <- c(min(th), max(th))
            plot(
              -1,-1,
              main = dist,
              xlim = c(min(th), max(th)),
              ylim = ylim,
              xaxt = "n",
              yaxt = "n",
              ann = FALSE
            )
            title(main = paste("n =", ps[[1]][[3]]))
            lines(th, Tmin, lwd = 3, lty = 1)
            axis(2,
                 round(ylim[1] + (0:5) / 5 * (ylim[2] - ylim[1]), digits = 2),
                 round(ylim[1] + (0:5) / 5 * (ylim[2] - ylim[1]), digits = 2))
            axis(1,
                 round(xlim[1] + (0:5) / 5 * (xlim[2] - xlim[1]), digits = 2),
                 round(xlim[1] + (0:5) / 5 * (xlim[2] - xlim[1]), digits = 2))
            mtext(expression(theta),
                  side = 1,
                  line = 2)
            mtext(expression(EL(theta, psi, lambda)), side = 2, line = 2)
          }
          lines(
            th,
            fval,
            col = c25[i + 1],
            lwd = 3 * .95^i,
            lty = i + 1
          )
          if (i == length(ps))
          {
            legend(
              "topright",
              legend = c("Infimum", nmet),
              col = c25[1:(i + 1)],
              lty = 1:(i + 1),
              cex = 0.8,
              bg = "white",
              lwd = 2
            )
          }
        }
        else if (dist == "bernoulli")
        {
          if (is.null(tlim))
          {
            tlim <- c(0, 1)
          }
          omegaT <- NULL
          for (om in 0:n) {
            omegaT <- c(omegaT,
                        integrate(function(x) {
                          fpsi(x, om, gamma)
                        }, lower = tlim[1], upper = tlim[2])[[1]])
          }
          ac <- function(tau, theta, gamma) {
            q <- ifelse(
              tau < theta,
              qbinom(gamma, size = n, prob = tau),
              qbinom(1 - gamma, size = n, prob = tau)
            )
            ifelse(
              tau < theta,
              pbinom(q - 1, size = n, prob = theta) + (gamma - pbinom(
                q - 1, size = n, prob = tau
              )) * dbinom(q, n, theta) / dbinom(q, n, tau),
              pbinom(
                q,
                size = n,
                prob = theta,
                lower.tail = FALSE
              ) + (gamma - pbinom(
                q,
                size = n,
                prob = tau,
                lower.tail = FALSE
              )) * dbinom(q, n, theta) / dbinom(q, n, tau)
            )
          }
          fpsiT <- NULL
          Tmin <- NULL
          for (th1 in th) {
            fpsiT <- c(fpsiT, sum(dbinom(
              0:n, size = n, prob = th1
            ) * omegaT))
            Tmin <- c(Tmin,
                      integrate(
                        function(x) {
                          ac(x, th1, gamma)
                        },
                        lower = tlim[1],
                        upper = tlim[2],
                        subdivisions = 1e6
                      )[[1]])
          }
          if (is.null(ylim))
          {
            ylim <- c(0, 1)
          }
          xlim <- c(min(th), max(th))
          if (i == 1)
          {
            plot(
              -1,-1,
              main = dist,
              xlim = c(min(th), max(th)),
              ylim = ylim,
              xaxt = "n",
              yaxt = "n",
              ann = FALSE
            )

            lines(th, Tmin, lwd = 3, lty = 2)

            lines(th, Tmin, lwd = 3, lty = 1)
            axis(2,
                 round(ylim[1] + (0:5) / 5 * (ylim[2] - ylim[1]), digits = 2),
                 round(ylim[1] + (0:5) / 5 * (ylim[2] - ylim[1]), digits = 2))
            axis(1,
                 round(xlim[1] + (0:5) / 5 * (xlim[2] - xlim[1]), digits = 2),
                 round(xlim[1] + (0:5) / 5 * (xlim[2] - xlim[1]), digits = 2))
            mtext(expression(theta),
                  side = 1,
                  line = 2)
            mtext(expression(EL(theta, psi, lambda)), side = 2, line = 2)
            title(main = paste("n =", ps[[1]][[3]]))
          }
          lines(
            th,
            fpsiT,
            col = c25[i + 1],
            lwd = 3 * .95^i,
            lty = i + 1
          )
          if (i == length(ps))
          {
            legend(
              "topright",
              legend = c("Infimum", nmet),
              col = c25[1:(i + 1)],
              lty = 1:(i + 1),
              cex = 0.8,
              bg = "white",
              lwd = 2
            )
          }

        }
        else if (dist == "poisson")
        {
          if(is.null(tlim))
          {
            tlim<-c(0,Inf)
          }
          N <- 0
          cont <- TRUE
          while (cont) {
            #print((n * max(xlim)^3) * ppois(N, n * max(xlim), lower.tail = FALSE))
            if ((n * max(th)^3) * ppois(N, n * max(th), lower.tail = FALSE) >=
                1e-5) {
              N <- N + 1
            }
            else{
              cont <- FALSE
            }
          }
          N <- N + 3
          omegaT <- NULL
          for (om in 0:N) {
            omegaT <- c(
              omegaT,
              integrate(
                function(x) {
                  fpsi(x, om, gamma)
                },
                lower = max(sup(om, gamma)[1],tlim[1]),
                upper = min(sup(om, gamma)[2],tlim[2]),
                subdivisions = 1e6,
                stop.on.error = FALSE
              )[[1]]
            )
          }
          ac <- function(tau, theta, gamma) {
            q <- ifelse(tau < theta,
                        qpois(gamma, n * tau),
                        qpois(1 - gamma, n * tau))
            ifelse(
              tau < theta,
              ppois(q - 1, n * theta) + (gamma - ppois(q - 1, n * tau)) *
                dpois(q, n * theta) / dpois(q, n * tau),
              ppois(q, n * theta, lower.tail = FALSE) + (gamma - ppois(q, n *
                                                                         tau, lower.tail = FALSE)) * dpois(q, n * theta) / dpois(q, n * tau)
            )
          }
          fpsiT <- NULL
          Tmin <- NULL
          for (th1 in th) {
            fpsiT <- c(fpsiT, sum(dpois(0:N, n * th1) * omegaT))
            Tmin <- c(
              Tmin,
              integrate(
                function(x) {
                  ac(x, th1, gamma)
                },
                lower = tlim[1],
                upper = tlim[2],
                subdivisions = 1e6,
                stop.on.error = FALSE
              )[[1]]
            )
          }
          if (is.null(ylim))
          {
            ylim <- c(0, 1.5 * max(Tmin))
          }
          xlim <- c(min(th), max(th))
          if (i == 1) {
            plot(
              -1,-1,
              main = dist,
              xlim = c(min(th), max(th)),
              ylim = ylim,
              xaxt = "n",
              yaxt = "n",
              ann = FALSE
            )
            title(main = paste("n =", ps[[1]][[3]]))
            lines(th, Tmin, lwd = 3, lty = 1)
            axis(2,
                 round(ylim[1] + (0:5) / 5 * (ylim[2] - ylim[1]), digits = 2),
                 round(ylim[1] + (0:5) / 5 * (ylim[2] - ylim[1]), digits = 2))
            axis(1,
                 round(xlim[1] + (0:5) / 5 * (xlim[2] - xlim[1]), digits = 2),
                 round(xlim[1] + (0:5) / 5 * (xlim[2] - xlim[1]), digits = 2))
            mtext(expression(theta),
                  side = 1,
                  line = 2)
            mtext(expression(EL(theta, psi, lambda)), side = 2, line = 2)
          }
          lines(
            th,
            fpsiT,
            col = c25[i + 1],
            lwd = 3 * .95^i,
            lty = i + 1
          )
          if (i == length(ps))
          {
            legend(
              "topright",
              legend = c("Infimum", nmet),
              col = c25[1:(i + 1)],
              lty = 1:(i + 1),
              cex = 0.8,
              bg = "white",
              lwd = 2
            )
          }
        }

      }
    }
  } else
  {
    stop("Invalid type.")
  }
}
