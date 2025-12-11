#' @title Fuzzy & Randomized Confidence Intervals
#'
#' @description
#' Function to create "psi" type objects.
#'
#'
#' @param method numeric value or name of the method that defines how obtain confidence intervals.
#' \itemize{
#'  \item A numeric value utilizes the value as reference.
#'  \item "GM" utilize the Gayer-Meeden method.
#'  \item "AC" utilize the Agrest-Coull method (for the binomial distribution only).
#'  \item "SC" utilizes the score method (for the poisson distribution only).
#' }
#' @param distribution name of the distribution of the sample.
#' \itemize{
#'  \item "normal" for the normal distribution.
#'  \item "bernoulli" for the Bernoulli or binomial distribution.
#'  \item "poisson" for the Poisson distribution.
#' }
#' @param n sample size.
#' @param sigma standard deviation (for the case of normal distribution).
#'
#' @return A list containing the following information.
#'\describe{
#'  \item{[[1]]}{the membership function}
#'  \item{[[2]]}{distribution}
#'  \item{[[3]]}{n}
#'  \item{[[4]]}{method}
#'  \item{[[5]]}{sigma}
#'  \item{[[6]]}{the upper and lower bound function of the support of the membership function.}
#'}
#'
#'@references FELIX, Carlos Henrique Trigo Nasser, Garcia, Nancy Lopes & Sousa, Alex Rodrigo dos Santos. (2025). Uma Nova Abordagem para Construção de Intervalos de Confiança Aleatorizados (Master's thesis, Universidade Estadual de Campinas, Campinas).
#'
#'GEYER, C. J.; MEEDEN, G. D. Fuzzy and randomized confidence intervals and
#'p-values. Statistical Science, v. 20, n. 4, p. 358–366, 2005. ISSN 08834237. Disponível em:
#'  http://www.jstor.org/stable/20061193.
#'
#'@examples
#'x<-psi("GM",n=10)
#'x[[1]](0.3,2)
#'x[[2]]
#'x[[3]]
#'x[[4]]
#'x[[5]]
#'x[[6]](2)
#'
#'x<-psi(0.5,n=10)
#'x[[1]](0.6,8)
#'x[[2]]
#'x[[3]]
#'x[[4]]
#'x[[5]]
#'x[[6]](8)
#' @import ump
#' @import zipfR
#' @import calculus
#' @importFrom DescTools PoissonCI
#' @import graphics
#' @import stats
#' @export

psi <- function(method,
                distribution = "bernoulli",
                n = 1,
                sigma = 1) {
  if(is.numeric(method)){
    nmet<-paste("Felix et al (2025) - o =",method)
  }
  else if(method=="GM"){
    nmet<-"Geyer & Meeden (2005)"
  }
  else if(method=="AC"){
    nmet<-"Agresti & Coull (1998)"
  }
  else{

    nmet<-method
  }

    if (distribution == "normal") {
    if (is.numeric(method)) {
      out <- list(function(theta, omega, gamma = 0.95) {
        ifelse(
          method > theta,
          ifelse(omega > theta + qnorm(gamma) *
                   sigma / sqrt(n), 0, 1),
          ifelse(omega < theta - qnorm(gamma) *
                   sigma / sqrt(n), 0, 1)
        )
      }, distribution, n, nmet, sigma, function(omega,gamma=0.95){c(min(method,omega-qnorm(gamma) *
                                                                                   sigma / sqrt(n)),max(method,omega+qnorm(gamma) *
                                                                                                          sigma / sqrt(n)))})
      class(out) <- "psi"
      return(out)
    } else if (method == "GM") {
      out <- list(function(theta, omega, gamma = 0.95) {
        ifelse(
          omega + qnorm((1 + gamma) / 2) * sigma / sqrt(n) >= theta,
          ifelse(omega - qnorm((1 +
                                  gamma) / 2) * sigma / sqrt(n) <= theta, 1, 0),
          0
        )
      }, distribution, n, nmet, sigma,function(omega,gamma=0.95){c(omega-qnorm(0.5+gamma/2) *
                                                                              sigma / sqrt(n),omega+qnorm(0.5+gamma/2) *
                                                                              sigma / sqrt(n))})
      class(out) <- "psi"
      return(out)
    } else{
      stop("Invalid method for this distribution.")
    }

  }
  else if (distribution == "bernoulli") {
    if (is.numeric(method)) {
      psi_o_binom <- function(r, o, t, g, n) {
        ifelse(t < o,
               ifelse(
                 t < Rbeta.inv(1 - g, r, n - r + 1),
                 0,
                 ifelse(
                   t < Rbeta.inv(1 - g, r + 1, n - r),
                   (g - 1 + Rbeta(t, r, n -
                                    r + 1)) / dbinom(r, n, t),
                   1
                 )
               ),
               ifelse(
                 t < Rbeta.inv(g, r, n - r + 1),
                 1,
                 ifelse(t < Rbeta.inv(g, r + 1, n - r), (g - Rbeta(t, r +
                                                                     1, n - r)) / dbinom(r, n, t), 0)
               ))
      }
      out <- list(function(theta, omega, gamma = 0.95) {
        psi_o_binom(
          r = omega,
          o = method,
          t = theta,
          g = gamma,
          n = n
        )
      }, distribution, n, nmet,sigma,function(omega,gamma=0.95){c(min(method,Rbeta.inv(1-gamma,omega,n-omega+1)),max(method,Rbeta.inv(gamma,omega+1,n-omega)))})
      class(out) <- "psi"
      return(out)
    } else if (method == "GM") {
      out <- list(function(theta, omega, gamma = 0.95) {
        1 - umpu.binom(omega, n, theta, alpha = 1 - gamma)
      }, distribution, n, nmet,sigma,function(omega,gamma=0.95){c(Rbeta.inv(0.25-gamma/4,omega,n-omega+1),Rbeta.inv(0.75+gamma/4,omega+1,n-omega))})
      class(out) <- "psi"
      return(out)
    } else if (method == "AC") {
      agresticoull <- function(n = 10,
                               x = 0:n,
                               conf.level = 0.95,
                               method = NULL) {
        a <- 1 - conf.level
        z <- qnorm(1 - a / 2)
        n2 <- n + z^2
        p <- (x + z^2 / 2) / n2
        t <- z * sqrt(p / n2 * (1 - p))
        upper <- p + t
        lower <- p - t
        upper <- ifelse(upper >= 1, 1, upper)
        lower <- ifelse(lower <= 0, 0, lower)
        upper[which(x == n)] = 1
        lower[which(x == 0)] = 0
        out <- as.data.frame(rbind(upper, lower))
        names(out) <- paste('x =', x)
        ind = paste('agresticoull ; n =', n, ' ; conf.level =', conf.level)
        if (length(x) == 1)
        {
          ind = paste(ind, '; x =', x)
        }
        r <- list(ind, out)
        return(r)
      }
      out <- list(function(theta, omega, gamma = 0.95) {
        c <- agresticoull(n = n,
                          x = omega,
                          conf.level = gamma)
        return(ifelse(theta > c[[2]][1, 1], 0, ifelse(theta < c[[2]][2, 1], 0, 1)))
      }, distribution, n, nmet,sigma,function(omega,gamma=0.95){c(c[[2]][2, 1],c[[2]][1, 1])})
      class(out) <- "psi"
      return(out)
    }

  }
  else if (distribution == "poisson") {
    if (is.numeric(method)) {
      psi_o_poisson <- function(r, o, t, g) {
        ifelse(t < o,
               ifelse(
                 t < qchisq(1 - g, 2 * r) / 2,
                 0,
                 ifelse(
                   t < qchisq(1 - g, 2 * r + 2) / 2,
                   (g - 1 + pchisq(2 * t, 2 * r)) / dpois(x = r, lambda = t),
                   1
                 )
               ),
               ifelse(t < qchisq(g, 2 * r) / 2, 1, ifelse(
                 t < qchisq(g, 2 * r + 2) / 2,
                 (g - pchisq(2 * t, 2 * r + 2)) / dpois(x = r, lambda = t),
                 0
               )))
      }
      out <- list(function(theta, omega, gamma = 0.95) {
        psi_o_poisson(
          r = omega,
          o = n * method,
          t = n * theta,
          g = gamma
        )
      }, distribution, n, nmet,sigma,function(omega,gamma=0.95){c(min(method/n,qchisq(1 - gamma, 2 * omega) / 2/n),max(method/n,qchisq(gamma, 2 * omega + 2) / 2/n))})
      class(out) <- "psi"
      return(out)
    } else if (method == "GM") {
      gm_pois <- function(c1, c2, gamma, l) {
        p1 <- dpois(c1, l)
        p2 <- dpois(c2, l)
        if (c2 - 2 >= c1) {
          p12 <- sum(dpois((c1 + 1):(c2 - 1), l))
          m12 <- sum(((c1 + 1):(c2 - 1)) * dpois((c1 + 1):(c2 - 1), l))
        } else{
          p12 <- 0
          m12 <- 0
        }
        c((gamma * (c2 - l) + m12 - c2 * p12) / p1 / (c2 - c1),
          (gamma * (l - c1) - m12 + c1 * p12) / p2 / (c2 - c1))
      }

      gm_test <- function(l, gamma) {
        c1 <- floor(l)
        c2 <- c1 + 1
        obj <- NULL
        erro1 <- 0
        erro2 <- 0
        while (1) {
          obj <- gm_pois(c1, c2, gamma, l)
          erro1 <- obj[1]
          erro2 <- obj[2]
          if (erro1 <= 1 & erro1 >= 0 & erro2 <= 1 & erro2 >= 0)
            break

          if (erro1 <= 1 & erro1 >= 0)
            erro1 <- 0

          if (erro2 <= 1 & erro2 >= 0)
            erro2 <- 0

          if (erro1 > 1)
            erro1 = erro1 - 1

          if (erro2 > 1)
            erro2 = erro2 - 1

          if (abs(erro1) > abs(erro2)) {
            c1 <- c1 - sign(erro1)
          } else{
            c2 <- c2 + sign(erro2)
          }
        }
        c(c1, erro1, c2, erro2)
      }
      psi_gm <- function(w, l, gamma) {
        out <- NULL
        for (lv in l) {
          if (lv == floor(lv) & dpois(floor(lv), lv) >= gamma) {
            out <- c(out, gamma / dpois(floor(lv), lv) * (w == floor(lv)))
          }
          else{
            obj <- gm_test(lv, gamma)
            if (w == obj[1])
              out <- c(out, obj[2])

            if (w == obj[3])
              out <- c(out, obj[4])

            if (w < obj[1] | w > obj[3])
              out <- c(out, 0)

            if (w > obj[1] & w < obj[3])
              out <- c(out, 1)

          }
        }
        out
      }
      out <- list(function(theta, omega, gamma = 0.95) {
        psi_gm(w = omega,
               l = n * theta,
               gamma = gamma)
      }, distribution, n, nmet,sigma,function(omega,gamma=0.95){c(qchisq(1 - gamma, 2 * omega) / 2/n,qchisq(gamma, 2 * omega + 2) / 2/n)})
      class(out) <- "psi"
      return(out)
    } else if (method == "Score") {

      out <- list(function(theta, omega, gamma = 0.95) {
        c89 <- PoissonCI(x = omega,
                         conf.level = gamma,
                         n = n)
        return(ifelse(theta > c89[3], 0, ifelse(theta < c89[2], 0, 1)))
      }, distribution, n, nmet,sigma,function(omega,gamma=0.95){PoissonCI(x = as.integer(omega),
                                               conf.level = gamma,
                                               n = n)[2:3]})
      class(out) <- "psi"
      return(out)
    }
  } else{
    stop("Invalid distribution function.")
  }
}
