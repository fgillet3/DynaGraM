### Periodic rate of relative change

chaw <-
  data.frame(
    Year = 1:nyears,
    No = 0,
    Nm = 0,
    W = 0,
    Ba = 0,
    Bb = 0,
    Bc = 0,
    Bd = 0,
    Be = 0,
    Br = 0,
    Bl = 0,
    Bt = 0,
    LAI = 0,
    Beve = 0
  )

for (i in 2:nyears) {
  chaw$Ba[i] <- 100 * mean(abs(out$B1[out$year == chaw$Year[i]] -
                               out$B1[out$year == chaw$Year[i - 1]]) /
                               out$Bt[out$year == chaw$Year[i - 1]])
  chaw$Bb[i] <- 100 * mean(abs(out$B2[out$year == chaw$Year[i]] -
                               out$B2[out$year == chaw$Year[i - 1]]) /
                               out$Bt[out$year == chaw$Year[i - 1]])
  chaw$Bc[i] <- 100 * mean(abs(out$B3[out$year == chaw$Year[i]] -
                               out$B3[out$year == chaw$Year[i - 1]]) /
                               out$Bt[out$year == chaw$Year[i - 1]])
  chaw$Bd[i] <- 100 * mean(abs(out$B4[out$year == chaw$Year[i]] -
                               out$B4[out$year == chaw$Year[i - 1]]) /
                               out$Bt[out$year == chaw$Year[i - 1]])
  chaw$Be[i] <- 100 * mean(abs(out$B5[out$year == chaw$Year[i]] -
                               out$B5[out$year == chaw$Year[i - 1]]) /
                               out$Bt[out$year == chaw$Year[i - 1]])
  chaw$Br[i] <- 100 * mean(abs(out$B6[out$year == chaw$Year[i]] -
                               out$B6[out$year == chaw$Year[i - 1]]) /
                               out$Bt[out$year == chaw$Year[i - 1]])
  chaw$Bl[i] <- 100 * mean(abs(out$B7[out$year == chaw$Year[i]] -
                               out$B7[out$year == chaw$Year[i - 1]]) /
                               out$Bt[out$year == chaw$Year[i - 1]])
  chaw$Bt[i] <- 100 * mean(abs(out$Bt[out$year == chaw$Year[i]] -
                               out$Bt[out$year == chaw$Year[i - 1]]) /
                               out$Bt[out$year == chaw$Year[i - 1]])
  chaw$LAI[i] <- 100 * mean(abs(out$LAIt[out$year == chaw$Year[i]] -
                                out$LAIt[out$year == chaw$Year[i - 1]]) /
                                out$LAIt[out$year == chaw$Year[i - 1]])
  chaw$No[i] <- 100 * mean(abs(out$No[out$year == chaw$Year[i]] -
                               out$No[out$year == chaw$Year[i - 1]]) /
                               out$No[out$year == chaw$Year[i - 1]])
  chaw$Nm[i] <- 100 * mean(abs(out$Nm[out$year == chaw$Year[i]] -
                               out$Nm[out$year == chaw$Year[i - 1]]) /
                               out$Nm[out$year == chaw$Year[i - 1]])
  chaw$W[i] <- 100 * mean(abs(out$W[out$year == chaw$Year[i]] -
                              out$W[out$year == chaw$Year[i - 1]]) /
                              out$W[out$year == chaw$Year[i - 1]])
  chaw$Beve[i] <- 100 * mean(abs(out$Beve[out$year == chaw$Year[i]] -
                                 out$Beve[out$year == chaw$Year[i - 1]]) /
                                 out$Beve[out$year == chaw$Year[i - 1]])
}

chal <- as_tibble(chaw) %>% gather(key = "Variable", value = "Rate", -Year)
