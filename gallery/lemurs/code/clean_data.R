lem <- readRDS("data/lemurs.rds")
taxons <- read.csv("data/taxon.csv")

# Data Wrangling ----
lem_nums <- lem %>%
    # join taxonomy description data to lemur data
    left_join(taxons, by = "taxon") %>%
    select(taxon, name, common, latin, description) %>%
    distinct() %>%
    # count number of each species and sort taxonomy as a factor
    group_by(taxon) %>%
    mutate(n = n()) %>%
    ungroup() %>%
    select(taxon, common, latin, n, description) %>%
    distinct() %>%
    arrange(desc(n)) %>%
    mutate(taxon = factor(taxon, levels = taxon)) %>%
    # join species name, latin name, and description together with HTML formatting
    mutate(species = glue::glue("<p align='left'><b>{common}</b><br><i>{latin}</i><br><small>{description}</small><p>")) %>%
    select(taxon, n, species)

lem_count <- lem %>%
    mutate(
        current = if_else(is.na(dod), "CURRENT", "DEAD"),
        current = factor(current, levels = c("CURRENT", "DEAD"))) %>%
    select(c(current, taxon, sex, name)) %>%
    distinct() %>%
    group_by(taxon, sex) %>%
    # include total number of male and female lemurs
    mutate(n_tot = n()) %>%
    ungroup() %>%
    group_by(current, taxon, sex) %>%
    mutate(n = n()) %>%
    filter(sex %in% c("F", "M")) %>%
    select(-name) %>%
    distinct()

lem_weights <- lem %>%
    select(c(taxon, sex, age_at_wt_mo, weight_g)) %>%
    filter(sex %in% c("F", "M")) %>%
    drop_na() %>%
    # break down into 2 year age groups and calculate avg weight
    mutate(age_group = cut(age_at_wt_mo,
        breaks = seq(0, 500, 24))) %>%
    group_by(taxon, sex, age_group) %>%
    mutate(avg_wt = mean(age_at_wt_mo)) %>%
    select(-c(age_at_wt_mo, weight_g)) %>%
    distinct() %>%
    # remove zero weights as these seem to be incorrect values
    filter(avg_wt != 0)
