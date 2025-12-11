#' @title Plot for "psi" type objects.
#'
#' @param x A list containing the following information.
#' \describe{
#'   \item{[[1]]}{the membership function}
#'   \item{[[2]]}{the name of the sample distribution}
#'   \item{[[3]]}{sample size}
#'   \item{[[4]]}{the name of method}
#'   \item{[[5]]}{standard deviation (for the case of normal distribution).}
#'   \item{[[6]]}{the upper and lower bound function of the support of the membership function.}
#' }
#' @param ... generic plot parameter.
#'@param psi2 Another list containing the following information.
#'\describe{
#'  \item{[[1]]}{the membership function}
#'  \item{[[2]]}{the name of the sample distribution}
#'  \item{[[3]]}{sample size}
#'  \item{[[4]]}{the name of method}
#'  \item{[[5]]}{standard deviation (for the case of normal distribution).}
#'  \item{[[6]]}{the upper and lower bound function of the support of the membership function.}
#'}
#'@param gamma the confidence level.
#'@param xlim A vector containing the upper and lower values for the theta.
#' @param ylim A vector containing the values of omega to be included in the plot.
#' @param type The type of the plot, by default is the menbership function, and "length" for the expected length plot.
#'
#'@return No return value, called for side effects
#'
#'@examples
#'x<-psi("GM",n=10)
#'plot(x)
#'plot(x,type="length")
#'
#'x<-psi(0.5,n=10)
#'plot(x)
#'plot(x,type="length")
#'@export



plot.psi <- function(x,...,
                     psi2 = NULL,
                     gamma = 0.95,
                     xlim = NULL,
                     ylim = NULL,
                     type = "psi") {
  plo <- function(f,
                  om,
                  th,
                  gamma = gamma,
                  col = 1,
                  lty = 1,
                  add = FALSE,
                  lwd = 1,
                  l = TRUE,
                  b = TRUE) {
    yl <- NULL
    name <- round(min(th) + (0:10) * (max(th) - min(th)) / 10,digits = 2)
    if (add == TRUE) {
      par(new = TRUE)
    } else{
      plot(
        -1,-1,
        type = "n",
        xlim = c(min(th), max(th)),
        ylim = c(min(om) - .5, max(om) + .5),
        xaxt = "n",
        yaxt = "n",
        ann = FALSE
      )
      if (l == TRUE) {
        mtext(expression(psi * group("(", tau * "|" * omega, ")")),
              side = 2,
              line = 3)
      }
      if (b == TRUE) {
        axis(
          1,
          at = name ,
          labels = name,
          las = 1,
          cex.axis = 0.8
        )
        mtext(expression(tau), side = 1, line = 2)
      }
    }
    ## x-axis already created

    ## y-axis creation
    for (o in om) {
      if (add == FALSE & l == TRUE) {
        axis(
          2,
          at = o,
          labels = substitute(omega == x, list(x = o)),
          las = 2
        )
      }
      fu <- f(th, o, g = gamma)
      lines(th,
            fu * 0.8 - 0.4 + o,
            col = col,
            lty = lty,
            lwd = lwd)
    }
  }
  oldpar <- par(no.readonly = TRUE) # code line i
  on.exit(par(oldpar)) # code line i + 1
  fpsi <- x[[1]]
  dist <- x[[2]]
  n <- x[[3]]
  met <- x[[4]]
  sup <- x[[6]]
  if (dist == "normal") {
    sigma <- x[[5]]
  }
  v2 <- FALSE
  if (is.null(psi2) == FALSE) {
    v2 <- TRUE
    fpsi2 <- psi2[[1]]
    dist2 <- psi2[[2]]
    n2 <- psi2[[3]]
    if (n != n2) {
      stop("Diferent sample lenght.")
    }
    met2 <- psi2[[4]]
    if (dist2 == "normal") {
      sigma2 <- psi2[[5]]
    }
    sup2 <- psi2[[6]]
  }

  if (is.null(ylim)) {
    if (dist == "normal") {
      ylim = c(-1.5 * qnorm((1 + gamma) / 2) * sigma / sqrt(n),
               1.5 * qnorm((1 + gamma) / 2) * sigma / sqrt(n))
    }
    if (dist == "bernoulli") {
      ylim = 0:n
    }
    if (dist == "poisson") {
      ylim = 0:10
    }
  }
  if (is.null(xlim)) {
    if (dist == "normal") {
      xlim = ylim
    }
    if (dist == "bernoulli") {
      xlim = c(0, 1)
    }
    if (dist == "poisson") {
      xlim = c(0, sup(max(ylim), gamma)[2])
    }
  }

  if (type == "psi") {
    if (dist == "normal") {
      stop("Plot for normal distribution not avaible.")
      om <- ylim[1] + (ylim[2] - ylim[1]) * (0:40) / 40
      th <- xlim[1] + (xlim[2] - xlim[1]) * (0:40) / 40
      teste <- outer(th, om, fpsi)
      contour(
        th,
        om,
        teste,
        drawlabels = FALSE,
        nlevels = 1,
        xlab = expression(tau),
        ylab = expression(omega)
      )
      .filled.contour(th, om, teste, c(0, 0.00001, 0.9999999, 1), c(0, 1, 1))
    }
    else if (dist == "bernoulli") {
      om <- ylim
      th <- xlim[1] + (xlim[2] - xlim[1]) * (0:100) / 100
      plo(fpsi, om, th, gamma = gamma, lwd = 2)
      if (v2) {
        plo(
          fpsi2,
          om,
          th,
          gamma = gamma,
          col = "red",
          lty = 2,
          add = TRUE,
          lwd = 2
        )
        legend(
          "bottomright",
          legend = c(met, met2),
          col = c(1, "red"),
          lty = c(1, 2),
          cex = 0.8,
          bg = "white",
          lwd = c(1, 1)
        )

      }
    }
    else if (dist == "poisson") {
      om <- ylim
      th <- xlim[1] + (xlim[2] - xlim[1]) * (0:100) / 100
      plo(fpsi, om, th, gamma = gamma)
      if (v2) {
        plo(
          fpsi2,
          om,
          th,
          gamma = gamma,
          col = "red",
          lty = 2,
          add = TRUE,
          lwd = 2
        )
        legend(
          "bottomright",
          legend = c(met, met2),
          col = c(1, "red"),
          lty = c(1, 2),
          cex = 0.8,
          bg = "white",
          lwd = c(1, 1)
        )

      }
    }
    else{
      stop("Invalid distribution function.")
    }
  }
  else if (type == "length") {
    if (dist == "normal") {
      stop("Plot for normal distribution not avaible.")
    }
    else if (dist == "bernoulli") {
      theta <- xlim[1] + 1:100 / 100 * xlim[2]
      omegaT <- NULL
      for (om in 0:n) {
        omegaT <- c(omegaT, integrate(function(x) {
          fpsi(x, om, gamma)
        }, lower = 0, upper = 1)[[1]])
      }
      if (v2) {
        omegaT2 <- NULL
        for (om in 0:n2) {
          omegaT2 <- c(omegaT2, integrate(function(x) {
            fpsi2(x, om, gamma)
          }, lower = 0, upper = 1)[[1]])
        }
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
      for (th in theta) {
        fpsiT <- c(fpsiT, sum(dbinom(
          0:n, size = n, prob = th
        ) * omegaT))
        Tmin <- c(Tmin,
                  integrate(
                    function(x) {
                      ac(x, th, gamma)
                    },
                    lower = 0,
                    upper = 1,
                    subdivisions = 1e6
                  )[[1]])
      }
      if (v2) {
        fpsiT2 <- NULL
        for (th in theta) {
          fpsiT2 <- c(fpsiT2, sum(dbinom(
            0:n, size = n, prob = th
          ) * omegaT2))
        }
      }
      ylim <- c(0, 1)
      plot(
        -1,
        -1,
        main = dist,
        xlim = xlim,
        ylim = ylim,
        xaxt = "n",
        yaxt = "n",
        ann = FALSE
      )
      lines(theta, fpsiT, col = "red", lwd = 3)
      lines(theta, Tmin, lwd = 3, lty = 2)
      axis(1, (0:5) / 5, (0:5) / 5)
      axis(2, (0:5) / 5, (0:5) / 5)
      mtext(expression(theta), side = 1, line = 2)
      mtext(expression(TE(theta, psi, lambda)), side = 2, line = 2)
      if (v2) {
        lines(theta, fpsiT2, col = "blue", lwd = 3)
        legend(
          "topright",
          legend = c("Infimum", met, met2),
          col = c(1, "red", "blue"),
          lty = c(2, 1, 1),
          cex = 0.8,
          bg = "white",
          lwd = 2
        )
      }
      else{
        legend(
          "topright",
          legend = c("Infimum", met),
          col = c(1, "red"),
          lty = c(2, 1),
          cex = 0.8,
          bg = "white",
          lwd = 2
        )
      }

    }
    else if (dist == "poisson") {
      N <- 0
      cont <- TRUE
      while (cont) {
        #print((n * max(xlim)^3) * ppois(N, n * max(xlim), lower.tail = FALSE))
        if ((n * max(xlim)^3) * ppois(N, n * max(xlim), lower.tail = FALSE) >=
            1e-5) {
          N <- N + 1
        }
        else{
          cont <- FALSE
        }
      }
      N <- N + 3
      theta <- xlim[1] + 1:100 / 100 * xlim[2]
      omegaT <- NULL
      for (om in 0:N) {
        omegaT <- c(omegaT,
                    integrate(
                      function(x) {
                        fpsi(x, om, gamma)
                      },
                      lower = sup(om, gamma)[1],
                      upper = sup(om, gamma)[2],
                      subdivisions = 1e6
                    )[[1]])
      }
      if (v2) {
        omegaT2 <- NULL
        for (om in 0:N) {
          omegaT2 <- c(
            omegaT2,
            integrate(
              function(x) {
                fpsi2(x, om, gamma)
              },
              lower = sup(om, gamma)[1],
              upper = sup(om, gamma)[2],
              subdivisions = 1e6
            )[[1]]
          )
        }
      }
      ac <- function(tau, theta, gamma) {
        q <- ifelse(tau < theta, qpois(gamma, n * tau), qpois(1 - gamma, n * tau))
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
      for (th in theta) {
        fpsiT <- c(fpsiT, sum(dpois(0:N, n * th) * omegaT))
        Tmin <- c(Tmin,
                  integrate(
                    function(x) {
                      ac(x, th, gamma)
                    },
                    lower = 0,
                    upper = Inf,
                    subdivisions = 1e6
                  )[[1]])
      }
      if (v2) {
        fpsiT2 <- NULL
        for (th in theta) {
          fpsiT2 <- c(fpsiT2, sum(dpois(0:N, n * th) * omegaT2))
        }
      }
      ylim <- c(0, max(Tmin, fpsiT))
      plot(
        -1,
        -1,
        main = dist,
        xlim = xlim,
        ylim = ylim,
        xaxt = "n",
        yaxt = "n",
        ann = FALSE
      )
      lines(theta, fpsiT, col = "red", lwd = 3)
      lines(theta, Tmin, lwd = 3, lty = 2)

      axis(2,
           round(ylim[1] + (0:5) / 5 * (ylim[2] - ylim[1]), digits = 2),
           round(ylim[1] + (0:5) / 5 * (ylim[2] - ylim[1]), digits = 2))
      axis(1,
           round(xlim[1] + (0:5) / 5 * (xlim[2] - xlim[1]), digits = 2),
           round(xlim[1] + (0:5) / 5 * (xlim[2] - xlim[1]), digits = 2))
      mtext(expression(theta), side = 1, line = 2)
      mtext(expression(TE(theta, psi, lambda)), side = 2, line = 2)
      if (v2) {
        lines(theta, fpsiT2, col = "blue", lwd = 3)
        legend(
          "topright",
          legend = c("Infimum", met, met2),
          col = c(1, "red", "blue"),
          lty = c(2, 1, 1),
          cex = 0.8,
          bg = "white",
          lwd = 2
        )
      }
      else{
        legend(
          "topright",
          legend = c("Infimum", met),
          col = c(1, "red"),
          lty = c(2, 1),
          cex = 0.8,
          bg = "white",
          lwd = 2
        )
      }
    }
  } else{
    stop("Invalid type.")
  }
}
