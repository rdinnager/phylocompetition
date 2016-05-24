## df = data.frame of species and environental variables joined to species, site
## level parameters. The variable data is one long vector with a species col, a
## var col, a site col, and then this is joined to species and site level
## parameters Admittedly not the most efficient way of doing it space-wise, but
## greatly simplifies writing out the math!
generate_fund_niche <- function(df) {
  df <- df %>% 
    mutate(gaus = exp(-((val - spec_opt)/spec_tol)))
  Ps <- df %>%
    group_by(species, site) %>%
    summarise(Ps = prod(gaus)*K[1])
}

## df is a data.frame containing the species as a column, a competition variable
## column specifying which of n competition variables the value is, and then the
## values (3 columns), latent variables representing a species' position in
## latent 'competition space' w is the 'niche width', it controls how fast
## competition decays with increasing distance in competition space. A is
## intraspecific competition (generally equal to 1), could be negative to model
## mutualism. specs is just a list of the species in the dataset
generate_comp_coef <- function(df, w, A, specs) {
  df <- df %>%
    group_by(species) %>%
    do(dat = .$val)
  new_df <- data_frame(expand.grid(specs, specs))
}