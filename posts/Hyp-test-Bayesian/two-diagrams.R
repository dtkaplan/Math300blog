NP_compete_graph <- function(prior, like1, like2, ymax=1) {
  ggplot() +
    ylim(c(0,ymax)) +
    geom_point(aes(x=prior/2, y=0.8),
                 color="green", size=4) +
    geom_segment(aes(x=prior, xend=1, y=like2, yend=like2),
                 color="blue", linewidth=2) +
    geom_vline(xintercept=prior, color="red", linewidth=0.1) +
    geom_text(aes(x=prior/2, y=like1/2, label="Alternative (power)", angle=90)) +
    geom_text(aes(x=1/2+prior/2, y=like2/2, label="Null Hypoth.")) +
    theme_minimal() + xlab("") + ylab("Likelihood at observation") +
    labs(title="For the given observation.") +
    scale_x_continuous(breaks=c(), limits=c(0,1))
}

Significance_compete_graph <- function(prior, like1, like2, ymax=1) {
  area_amount <- function(p, h, hyp=1){
    paste(ifelse(p>=0.25, "Area", "A"), "=", round(p*h, 4), ifelse(hyp==0, "\nNull", "\nAlternative"))
  }
  ggplot() +
    ylim(c(0,ymax)) +
    geom_segment(aes(x=prior, xend=1, y=like2, yend=like2),
                 color="blue", linewidth=2) +
    geom_vline(xintercept=prior, color="red", linewidth=0.1) +
    geom_text(aes(x=1/2+prior/2, y=like2/2, label="Null Hypoth.")) +
    theme_minimal() + xlab("") + ylab("Likelihoods at observation") +
    labs(title="For the given observation.") +
    scale_x_continuous(breaks=c(), limits=c(0,1))
}
