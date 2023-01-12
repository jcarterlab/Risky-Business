# sets the seed and working directory. 
set.seed(1983)
setwd("C:\\Users\\HUAWEI\\Desktop\\Projects\\Risky-Business\\Data")

# a list of different violence risk categories. 
# the order is important for weighting scores in the prs algorithm. 
categories <- c("organized_crime", 
                "military_conflict", 
                "terrorism", 
                "social_unrest", 
                "regime_instability")

# FUNCTIONS ##################################################################

### Word bank functions

# combines 2 terms with the option for up to 2 words between each term. 
combine_terms <-  function(term_1, term_2) {
  terms <-  paste0(
    term_1, "\\s", "([\\w_.,!']+\\s){0,2}", term_2
  )
  return(terms)
}

# allows multiple combinations to be combined into one string. 
combine_strings <- function(combinations) {
  first_string <- combinations[1]
  other_strings <- combinations[2:length(combinations)]
  string <- first_string
  for(i in 2:length(combinations)) {
    string <- string %>%
      paste0("|", other_strings[i-1])
  }
  return(string)
}

### PRS algorithm functions

# parses text so that it can be evaluated as an object.   
parse_text <- function(vector_1, string) {
  text <- eval(
    parse(text=paste(vector_1, string, sep = ""))
  )
  return(text)
}

# calculates a score for a given word bank. 
calculate_word_bank <- function(text, string) {
  headlines <- str_to_lower(
    text
  )
  score <- 0
  for(word in 1:length(string)) {
    score <- if(str_detect(headlines, string[word])) score + 1 else score
  }
  return(score)
}

# calculates a score for all word bank types in a given category. 
calculate_word_banks <- function(text, category) {
    one <- calculate_word_bank(
      text, parse_text(category, "_1")
    )
    two <- calculate_word_bank(
      text, parse_text(category, "_2")
    )
    three <- calculate_word_bank(
      text, parse_text(category, "_3")
    )
    four <- calculate_word_bank(
      text, parse_text(category, "_4")
    )
    total <- (
      (one*0.5)+(two*0.75)+(three*1)+(four*1.25)
    )
    importance <- which(
      str_detect(category, categories)
    )
    weighted <- (
      total+(total*(((importance+1)/10)))
    )
    len <- length(
      unlist(str_split(text, " "))
    )
    len_adjusted <- (
      (weighted*10)/len
    )
  return(round(len_adjusted, 2))
}

# calculates a score depending on the type of threat. 
calculate_category <- function(category, text) {
  scores <- unlist(
    lapply(text, calculate_word_banks, category)
  )
  return(scores)
}

# calculates the political risk scores (prs) for different headlines. 
calculate_all_categories <- function(text) {
  scores <- as_tibble(
    sapply(categories, calculate_category, text)
  )
  final <- mutate(
    scores, Score = rowSums(scores)
  )
  return(final)
}

# determines the main category under which the headlines fall. 
assign_category <- function(scores) {
  data <- tibble()
  for(i in 1:nrow(scores)) {
    data[i,1] <- if(sum(scores[i,1:5])==0) "None"
    else {
      categories[max(which(scores[i,1:5]==max(scores[i,1:5])))]
    } 
  }
  return(data)                
}

# calculates a political risk score (PRS) for the headlines. 
prs <- function(text) {
  headlines <- as_tibble(
    text
  )
  scores <- calculate_all_categories(
    text
  )
  categories <- assign_category(
    scores
  )
  final <- tibble(
    as_tibble(text), scores[,6], categories
  )
  colnames(final) <- c(
    "Headline", "Score", "Category"
  )
  return(final)
}

# 0) GENERAL ##################################################################

### 0.1) RED_HERRINGS phrases

military_red_herrings <-  paste0(
  "(",
  war_of_words <-  "war of words\\b|",
  war_on_prices <-  "war on\\s([\\w_.,!']+\\s){0,2}prices\\b|",
  tug_of_war <-  "tug of war\\b|",
  cult_war <- "cult wars?\\b|",
  war_on_drugs <- "war on drugs?\\b|",
  war_veterans <- "war veterans?\\b|",
  jawar <-  "jawar\\b|",
  think_tank <- "think tanks?\\b|",
  munawar <- "munawar\\b|",
  peshawar <- "peshawar\\b|",
  anwar <- "anwar\\b",
  ")"
)

terrorism_red_herrings <-  paste0(
  "(",
  conspiracy <- "conspiracy\\b|",
  ")"
)

unrest_red_herrings <-  paste0(
  "(",
  patriot <-  "patriots?\\b|",
  compatriot <- "compatriots?\\b|",
  chariot <-  "chariots?\\b|",
  marriot <- "marriots?\\b|",
  run_riot_against <-  "run riot against\\b|",
  cypriots <- "cypriots?\\b",
  ")"
)

regime_red_herrings <-  paste0(
  "(",
  revolution_transforming <-  "revolution is transforming lives\\b|",
  industrial_revolution <-  "industrial revolution\\b",
  ")"
)

### 0.2) GENERAL words

# genocide words. 
genocide_words <-  paste0(
  "(",
  genocide <-  "commi(t|ts) (genocide|war crimes)\\b|",
  mass_murder <-  "mass( | )murder\\b|",
  exterminate <-  "exterminat(e|es|ed)\\b",
  ")"
)

# strong aggression words. 
strong_aggression_words <-  paste0(
  "(",
  attack <-  "attac(k|ks|ked|king)\\b|",
  aggression <-  "aggre(ssion|ssive)\\b|",
  combat <-  "combat\\b|",
  conflict <-  "conflic(t|ts|ted|ting)\\b|",
  assault <-  "assaul(t|ts|ted|ting)\\b|",
  shell <-  "shel(l|ls|led|ling)\\b|",
  invade <-  "inva(de|des|ded|ding)\\b|",
  annex <-  "anne(x|xes|xed|xing)\\b|",
  conquer <-  "conqu(er|ers|ered|ering)\\b",
  ")"
)
# weak aggression words.
weak_aggression_words <-  paste0(
  "(",
  bomb <-  "bom(b|bs|bed|bing)\\b|",
  strike <-  "strik(e|es|ing)\\b|",
  bombard <-  "bombar(d|ds|ded|ding)\\b|",
  blow_up <-  "bl(ow|ows|ew|owing) up\\b|",
  blast <-  "blas(t|ts|ted|ting)\\b|",
  fire <-  "fir(e|es|ed|ing)(shots|at)\\b|",
  ambush <-  "ambus(h|hes|hed|hing)\\b|",
  shoot <-  "sho(ot|ots|t|oting)\\b|",
  gun_down <-  "gu(n|ns|ned|ning) down\\b",
  ")"
)

# strong clash words. 
strong_clash_words <-  paste0(
  "(",
  clash <-  "clas(h|hes|hed|hing)\\b|",
  fight <-  "f(ight|ights|ighted|ighting|ought)\\b|",
  skirmish <-  "skirmis(k|kes|ked|king)\\b|",
  battle <-  "batt(le|les|led|ling)\\b|",
  brawl <-  "braw(l|ls|led|ling)\\b|",
  scrap <-  "scra(p|ps|ped|ping)\\b",
  ")"
)
# weak clash words. 
weak_clash_words <-  paste0(
  "(",
  confront <-  "confron(t|ts|ted|ting|tation)\\b|",
  hostility <-  "hostili(ty|ties)\\b|",
  dispute <-  "disput(e|es|ed|ing)\\b|",
  contest <-  "contes(t|ts|ted|ting|tation)\\b|",
  disagree <-  "dis(agree|agreement)\\b|",
  scuffle <-  "scuff(le|les|led|ling)\\b",
  ")"
)

# strong seizure words. 
strong_seizure_words <-  paste0(
  "(",
  seize <-  "sei(ze|zes|zed|zing)\\b|",
  loot <-  "loo(t|ts|ted|ting)\\b|",
  ransack <-  "ransa(ck|cks|cked|cking)\\b|",
  overrun <-  "overr(un|uns|an|uning)\\b|",
  overwhelm <-  "overwhel(m|ms|med|ming)\\b",
  ")"
)
# weak seizure words. 
weak_seizure_words <-  paste0(
  "(",
  rob <-  "ro(b|bs|bbed|bbing)\\b|",
  steal <-  "st(eal|eals|ole|ealing)\\b|",
  plunder <-  "plund(er|ers|ered|ering)\\b|",
  pillage <-  "pilla(ge|ges|ged|ging)\\b|",
  raid <-  "rai(d|ds|ded|ding)\\b",
  ")"
)

# strong terror words. 
strong_terror_words <-  paste0(
  "(",
  detonate <-  "detonat(e|es|ed|ing)\\b|",
  spread_fear <-  "sprea(d|ds|ded|ding) (terror|fear|panic)\\b|",
  terrorize <-  "terrori(z|s)(e|es|ed|ing)\\b|",
  blast <-  "blas(t|ts|ted|ting)\\b|",
  explode <-  "explod(e|es|ed|ing)\\b|",
  bomb <-  "bom(b|bs|bed)\\b|",
  ravage <-  "ravag(e|es|ed|ing)\\b|",
  rampage <-  "rampag(e|es|ed|ing)\\b|",
  terrify <-  "terrif(y|ies|ied|ying)\\b|",
  tyrannize <-  "tyranni(z|s)(e|es|ed|ing)\\b",
  ")"
)
# weak terror words. 
weak_terror_words <-  paste0(
  "(",
  exploit <-  "exploi(t|ts|ted|ting)\\b|",
  intimidate <-  "intimida(te|tes|ted|ting)\\b|",
  menace <-  "mena(ce|ces|ced|cing)\\b|",
  frighten <-  "fright(en|ens|ened)\\b|",
  threaten <-  "threat(en|ens|ened|ening)\\b|",
  torment <-  "tormen(t|ts|ted|ting)\\b|",
  abuse <-  "abus(e|es|ed|ing)\\b|",
  persecute <-  "persecut(e|es|ed|ing)\\b|",
  oppress <-  "oppre(ss|sses|ssed|ssing)\\b",
  ")"
)

# strong violence words. 
strong_violence_words <-  paste0(
  "(",
  slaughter <-  "slaughte(r|rs|red|ring)\\b|",
  massacre <-  "massacr(e|es|ed|ring)\\b|",
  butcher <-  "butche(r|rs|red|ring)\\b|",
  assassinate <-  "assassinat(e|es|ed|ing)\\b|",
  murder <-  "murde(r|rs|red|ring)\\b|",
  fire <-  "fir(e|es|ed|ing)(shots|at)\\b|",
  shoot <-  "sho(ot|ots|t|oting)\\b|",
  bomb <-  "bom(b|bs|bed|bing)\\b|",
  blow_up <-  "bl(ow|ows|ew|owing) up\\b|",
  gun_down <-  "gu(n|ns|ned|ning) down\\b|",
  torture <-  "tortur(e|es|ed|ing)\\b",
  ")"
)
# weak violence words. 
weak_violence_words <-  paste0(
  "(",
  kill <-  "kil(l|ls|led|ling)\\b|",
  attack <-  "attac(k|ks|ked|king)\\b|",
  execute <-  "execut(e|es|ed|ing)\\b|",
  assault <-  "assaul(t|ts|ted|ting)\\b|",
  fight <-  "f(ight|ights|ighted|ighting|ought)\\b|",
  skirmish <-  "skirmis(k|kes|ked|king)\\b|",
  battle <-  "batt(le|les|led|ling)\\b|",
  brawl <-  "braw(l|ls|led|ling)\\b|",
  scrap <-  "scra(p|ps|ped|ping)\\b",
  ")"
)

# strong destruction words.
strong_destruction_words <-  paste0(
  "(",
  destroy <-  "destro(y|ys|yed|ying)\\b|",
  annihilate <-  "annihilat(e|es|ed)\\b|",
  demolish <-  "demolis(h|hes|hed|hing)\\b|",
  dismantle <-  "dismantl(e|es|ed|ing)\\b|",
  deface <-  "defac(e|es|ed|ing)\\b|",
  topple <-  "toppl(e|es|ed|ing)\\b|",
  obliterate <-  "obliterat(e|es|ed|ing)\\b|",
  vandalize <-  "vandali(s|z)(e|es|ed|ing)\\b",
  ")"
)
# weak destruction words.
weak_destruction_words <-  paste0(
  "(",
  knock_down <-  "knoc(k|ks|ked|king) (down|over)\\b|",
  tear_down <-  "t(ear|ears|eared|ore|earing) down\\b|",
  pull_down <-  "pul(l|ls|led|ling) (down|over)\\b|",
  ruin <-  "rui(n|ns|ned|ning)\\b|",
  wreck <-  "wr(eck|ecks|ecked|oke|ecking)\\b|",
  damage <-  "damag(e|es|ed|ing)\\b|",
  smash <-  "smas(h|hes|hed|hing)\\b|",
  crush <-  "crus(h|hes|hed|hing)\\b|",
  shatter <-  "shatte(r|rs|red|ring)\\b",
  ")"
)

# strong capture words.
strong_capture_words <-  paste0(
  "(",
  capture <-  "captur(e|es|ed|ing)\\b|",
  hijack <-  "hijac(k|ks|ked|king)\\b|",
  hold_prisoner <-  "h(old|olds|eld|olding) (hostag(e|es)|to ransom)\\b|",
  take_prisoner <-  "t(ake|akes|ook|aken|aking) (prisoner|hostage)\\b|",
  abduct <-  "abduc(t|ts|ted|ting)\\b|",
  kidnap <-  "kidna(p|ps|ped|ping)\\b",
  ")"
)
# weak capture words.
weak_capture_words <-  paste0(
  "(",
  imprison <-  "impriso(n|ns|ed|ning)\\b|",
  arrest <-  "arres(t|ts|ted|ting)\\b|",
  detain <-  "detai(n|ns|ned|ning)\\b",
  ")"
)

# strong seize power words. 
strong_seize_power_words <-  paste0(
  "(",
  seize <-  "seiz(e|es|ed|ing) (power|control)\\b|",
  capture <-  "captur(e|es|ed|ing) (power|control)\\b|",
  grab <-  "gra(b|bs|bed|bing) (power|control)\\b|",
  snatch <-  "snatc(h|hes|hed|hing) (power|control)\\b|",
  gain <-  "gai(n|ns|ned|ning) (power|control)\\b",
  ")"
)
# weak seize power words. 
weak_seize_power_words <-  paste0(
  "(",
  assume <-  "assum(e|es|ed|ing) (power|control)\\b|",
  obtain <-  "obtai(n|ns|ned|ned) (power|control)\\b|",
  secure <-  "secur(e|es|ed|ing) (power|control)\\b|",
  win <-  "w(in|ins|on|ining) (power|control)\\b|",
  acquire <-  "acquir(e|es|ed|ing) (power|control)\\b|",
  attain <-  "attai(n|ns|ned|ning) (power|control)\\b",
  ")"
)

# hyperbolic situation words. 
hyperbolic_situation_words <-  paste0(
  "(",
  escalating <-  "escalat(e|ed|es|ing)\\b|",
  growing <-  "gr(ow|ows|ew|owing)\\b|",
  rising <-  "r(ise|ises|ose|ising)\\b|",
  increasing <-  "increas(e|es|ed|ing)\\b|",
  soaring <-  "soa(r|rs|red|ring)\\b|",
  surging <-  "surg(e|es|ed|ing)\\b|",
  spiraling <-  "spira(l|ls|led|ling)\\b|",
  worsening <-  "worse(n|ns|ned|ning)\\b|",
  heightening <-  "heighte(n|ns|ned|ning)\\b|",
  intensifying <-  "intensif(y|ies|ied|ying)\\b|",
  deteriorating <-  "deteriora(te|tes|ted|ting)\\b",
  ")"
)


# 1) ORGANIZED_CRIME #########################################################

### 1.1) ORGANIZED_CRIME words

# strong crime keywords. 
strong_crime_keywords <-  paste0(
  "(",
  organized_violence <-  "organized violence\\b|",
  kidnapping <-  "kidnappings?\\b|",
  abduction <-  "abductions?\\b|",
  held_to_ransom <-  "h(old|eld)(| to) ransom\\b|",
  international_crime <-  "international crimes?\\b|",
  transnational_crime <-  "trans(| )national crimes?\\b|",
  international_criminals <-  "international criminals?\\b|",
  transnational_criminals <-  "trans(| )national criminals?\\b|",
  slave_trade <-  "slave trade\\b|",
  slave_smuggling <-  "slave smuggling\\b|",
  sex_slavery <-  "sex slavery\\b|",
  arms_trade <-  "arms? trades?\\b|",
  arms_smuggling <-  "arms? smuggling\\b|",
  illegal_weapons_trade <-  "illegal weapons? trades?\\b|",
  weapons_smuggling <-  "weapons? smuggling\\b|",
  organ_trade <-  "organs? trades?\\b|",
  organ_smuggling <-  "organs? smuggling\\b|",
  people_trafficking <-  "people trafficking\\b|",
  people_smuggling <-  "people smuggling\\b|",
  human_trafficking <-  "human trafficking\\b",
  ")"
)
# weak crime keywords. 
weak_crime_keywords <-  paste0(
  "(",
  organized_crime <-  "organized crime\\b|",
  illegal_racket <-  "illegal rackets?\\b|",
  racketeering <-  "racketeering\\b|",
  narcotraffic <-  "narcotraffi(c|cking)\\b|",
  drugs_trade <-  "drugs? trades?\\b|",
  drugs_smuggling <-  "drugs? smuggling\\b|",
  drugs_trafficking <-  "drugs? trafficking\\b|",
  international_drugs_networks <-  "international drugs? networks?\\b|",
  transnational_drugs_networks <-  "transnational drugs? networks?\\b|",
  international_smuggling_networks <-  "international smuggling networks?\\b|",
  transnational_smuggling_networks <-  "transnational smuggling networks?\\b|",
  illicit_trade <-  "illicit trades?\\b|",
  criminal_world <-  "criminal worlds?\\b|",
  criminal_underworld <-  "criminal underworlds?\\b|",
  criminal_enterprise <-  "criminal enterprises?\\b|",
  criminal_network <-  "criminal networks?\\b|",
  illicit_network <-  "illicit networks?\\b|",
  illegal_network <-  "illegal networks?\\b|",
  criminal_association <-  "criminal associations?\\b|",
  hold_prisoner <-  "h(old|olds|eld|olding) (hostag(e|es)|to ransom)\\b|",
  take_prisoner <-  "t(ake|akes|ook|aken|aking) (prisoner|hostage)\\b|",
  abduct <-  "abduc(t|ts|ted|ting)\\b|",
  kidnap <-  "kidna(p|ps|ped|ping)\\b|",
  scam_victims <- "scam victims\\b",
  ")"
)

# crime actors. 
crime_actors <-  paste0(
  "(",
  cartel <-  "cartels?\\b|",
  syndicate <-  "syndicates?\\b|",
  mob <-  "mobs?\\b|",
  mobster <-  "mobsters?\\b|",
  mafia <-  "mafias?\\b|",
  racketeers <-  "racketeers?\\b|",
  gang <-  "gangs?\\b|",
  gangsters <-  "gangsters?\\b|",
  drug_dealers <-  "drugs? dealers?\\b|",
  narcotraffickers <-  "narcotraffickers?\\b|",
  drug_lord <-  "drugs? lords?\\b|",
  smuggler <-  "smugglers\\b|",
  crime_syndicate <-  "crime syndicates?\\b|",
  crime_family <-  "crime famil(y|ies)\\b|",
  crime_boss <-  "crime bos(s|sess)\\b",
  ")"
)

# crime situation words. 
crime_situation_words <-  paste0(
  "(",
  crime_actor_violence <-  paste(crime_actors, "violence\\b|"),
  crime_actor_tensions <-  paste(crime_actors, "tensions?\\b|"),
  crime_actor_strain <-  paste(crime_actors, "strain\\b|"),
  crime_actor_friction <-  paste(crime_actors, "frictions?\\b|"),
  crime_actor_hostility <-  paste(crime_actors, "hostilit(y|ies)\\b|"),
  crime_actor_crime <-  paste(crime_actors, "crimes?\\b"),
  ")"
)

### 1.2) ORGANIZED_CRIME groups

organized_crime_4 <-  c(
  combine_terms(crime_actors, genocide_words),
  combine_terms(crime_actors, strong_seizure_words),
  combine_terms(crime_actors, strong_terror_words),
  combine_terms(crime_actors, strong_violence_words),
  combine_terms(crime_actors, strong_capture_words)
)
organized_crime_3 <-  c(
  combine_terms(crime_actors, weak_seizure_words),
  combine_terms(crime_actors, weak_terror_words),
  combine_terms(crime_actors, weak_violence_words),
  combine_terms(crime_actors, weak_capture_words),
  combine_terms(crime_actors, strong_destruction_words),
  combine_terms(crime_actors, strong_seize_power_words)
)
organized_crime_2 <-  c(
  strong_crime_keywords,
  combine_terms(crime_actors, weak_destruction_words),
  combine_terms(crime_actors, weak_seize_power_words),
  combine_terms(crime_actors, strong_aggression_words),
  combine_terms(crime_actors, strong_clash_words),
  combine_terms(hyperbolic_situation_words, crime_situation_words)
)
organized_crime_1 <-  c(
  weak_crime_keywords,
  crime_situation_words
)


# 2) MILITARY_CONFLICT #########################################################

### 2.1) MILITARY_CONFLICT words

# strong military keywords. 
strong_military_keywords <-  paste0(
  "(",
  declare_war <-  "declar(e|es) war\\b|",
  war_crimes <-  "war crim(e|es)\\b|",
  mobalize_troops <-  "mobali(z|s)(e|ed) troops\\b|",
  military_intervention <-  "military intervention\\b|",
  airstrike <- "airstrikes?\\b|",
  missile_launch <- "missile launc(h|hes|hed|hing)\\b",
  ")"
)
# weak military keywords. 
weak_military_keywords <-  paste0(
  "(",
  war <-  "wa(r|rfare)\\b|",
  tanks <-  "tan(k|ks)\\b|",
  air_strikes <-  "air strik(e|es)\\b|",
  fighter_jets <-  "fighter je(t|ts)\\b|",
  attack_jets <-  "attack je(t|ts)\\b|",
  mercenaries <-  "mercenar(y|ies)\\b|",
  foreign_intervention <-  "foreign intervention\\b|",
  military_planes <- "military planes?\\b|",
  missile <- "missiles?\\b|",
  invade <- "invad(e|es|ed|ing)\\b",
  ")"
)

# military actors. 
military_actors <-  paste0(
  "(",
  military <-  "militar(y|ies)\\b|",
  army <-  "arm(y|ies)\\b|",
  soldiers <-  "soldie(r|rs)\\b|",
  troops <-  "troo(p|ps)\\b|",
  armed_forces <-  "armed forces\\b|",
  air_force <-  "air forc(e|es)\\b|",
  navy <-  "nav(y|ies)\\b|",
  marines <-  "marin(e|es)\\b",
  ")"
)

# military situation words. 
military_situation_words <-  paste0(
  "(",
  military_actor_tensions <-  paste(military_actors, "tensions?\\b|"),
  military_actor_tenseness <-  paste(military_actors, "tenseness\\b|"),
  military_actor_pressure <-  paste(military_actors, "pressures?\\b|"),
  military_actor_strain <-  paste(military_actors, "strains?\\b|"),
  military_actor_ambitions <-  paste(military_actors, "ambitions?\\b|"),
  military_actor_uncertainty <-  paste(military_actors, "uncertaint(y|ies)\\b|"),
  military_actor_frictions <-  paste(military_actors, "frictions?\\b|"),
  military_actor_hostility <-  paste(military_actors, "hostilit(y|ies)\\b"),
  ")"
)

# border words. 
border_words <-  paste0(
  "(",
  border <-  "borde(r|rs)\\b|",
  territory <-  "territor(y|ial|ies)\\b|",
  frontier <-  "frontie(r|rs)\\b|",
  boundary <-  "boundar(y|ies)\\b",
  ")"
)

### 2.2) MILITARY_CONFLICT groups

military_conflict_4 <-  c(
  combine_terms(military_actors, genocide_words),
  combine_terms(military_actors, strong_aggression_words)
)
military_conflict_3 <-  c(
  combine_terms(military_actors, strong_clash_words),
  combine_terms(border_words, strong_clash_words)
)
military_conflict_2 <-  c(
  strong_military_keywords,
  combine_terms(military_actors, weak_aggression_words),
  combine_terms(military_actors, weak_clash_words),
  combine_terms(military_actors, strong_seizure_words),
  combine_terms(border_words, weak_clash_words),
  combine_terms(hyperbolic_situation_words, military_situation_words)
)
military_conflict_1 <-  c(
  weak_military_keywords,
  combine_terms(military_actors, weak_seizure_words),
  military_situation_words
)


# 3) TERRORISM ################################################################

### 3.1) TERRORISM words

# strong terrorism keywords. 
strong_terrorism_keywords <-  paste0(
  "(",
  terrorism <-  "terrorism\\b|",
  terror_networks <-  "terror (networks?|cells?)\\b|",
  jihadism <-  "j(i|e)ha(d|dism)\\b|",
  suicide_attack <-  "suicide (attacks?|bombs?)\\b|",
  religious_extremism <-  "religious extremism\\b|",
  islamist_extremism <-  "  islam(ic|ist) extremism\\b|",
  religious_fanaticism <-  "religious fanaticism\\b|",
  islamist_fanaticism <-  "  islam(ic|ist) fanaticism\\b|",
  religious_fundamentalism <-  "religious fundamentalism\\b|",
  islamist_fundamentalism <-  "  islam(ic|ist) fundamentalism\\b|",
  religious_militancy <-  "religious militancy\\b|",
  islamist_militancy <-  "  islam(ic|ist) militancy\\b",
  ")"
)
# weak terrorism keywords.
weak_terrorism_keywords <-  paste0(
  "(",
  terror <-  "terror\\b|",
  fanaticism <-  "fanati(cism|cal)\\b|",
  extremism <-  "extremism\\b|",
  fundamentalism <-  "fundamentalism\\b|",
  martyr <-  "martyrdom\\b|",
  holy_war <-  "holy (wars?|crusades?|struggles?)\\b|",
  religious_war <-  "religious (wars?|crusades?|struggles?)\\b|",
  infidel <-  "infidels?\\b|",
  radicalization <-  "radical(ism|i(s|z)ation|i(s|z)ed?|i(s|z)es)\\b|",
  piracy <-  "piracy\\b",
  ")"
)

# terrorism actors.
terrorism_actors <-  paste0(
  "(",
  terrorists <-  "terrorists?\\b|",
  extremists <-  "extremists?\\b|",
  fanatics <-  "fanatics?\\b|",
  radicals <-  "radicals?\\b|",
  martyrs <-  "martyrs?\\b|",
  terror_groups <-  "terror (groups?|suspects?|ploters?|attackers?)\\b|",
  jihadists <-  "j(i|e)hadists?\\b|",
  jihadi <-  "j(i|e)hadi\\b|",
  suicide_attackers <-  "suicide (attackers?|bombers?)\\b|",
  religious_extremists <-  "islam(ic|ist) extremists?\\b|",
  religious_fanatics <-  "islam(ic|ist) fanatics?\\b|",
  religious_fundamentalists <-  "islam(ic|ist) fundamentalists?\\b|",
  religious_militants <-  "islam(ic|ist) militants?\\b|",
  religious_radicals <-  "islam(ic|ist) radicals?\\b|",
  freedom_fighters <-  "freedom fighters?\\b|",
  pirates <-  "pirates?\\b|",
  islamic_state <-  "i(sis|sil|slamic state)\\b|",
  daesh <-  "daesh\\b|",
  al_shabaab <-  "al shab(aa?|a)b\\b|",
  boko_haram <-  "boko haram\\b|",
  al_qaeda <-  "al (qaida|qaeda|qa'ida)\\b|",
  abu_sayyaf <-  "abu sayyaf\\b|",
  ebu_seyyaf <-  "ebu sayyaf\\b|",
  harakat_ul_mujahidin <-  "harkat ul mujahideen\\b|",
  kahane_chai <-  "kahane chai\\b|",
  kurdistan_workers_party <-  "kurdistan workers' party\\b|",
  pkk <-  "pkk\\b|",
  liberation_tigers <-  "liberation tigers\\b|",
  ltte <-  "ltte\\b|",
  ltte <-  "tamil tigers\\b|",
  national_liberation_army <-  "national liberation army\\b|",
  eln <-  "eln\\b|",
  palestine_islamic_jihad <-  "palestinian islamic jihad\\b|",
  pflp <-  "popular front for the liberation of palestine\\b|",
  farc <-  "the revolutionary armed forces of colombia\\b|",
  farc_ac <-  "farc\\b|",
  dhkp <-  "revolutionary people's liberation (party|front)\\b|",
  shining_path <-  "shining path\\b|",
  mpcp <-  "militarized communist party of peru\\b|",
  islamic_movement_uzbekistan <-  "islamic movement of uzbekistan\\b|",
  al_aqsa_martyrs <-  "al aqsa martyrs\\b|",
  asbat_al_ansar <-  "osbat al ansar\\b|",
  aqim <-  "aqim\\b|",
  new_people_army <-  "new people's army\\b|",
  jemaah_islamiya <-  "jemaah islamiyah\\b|",
  lashkar_jhangvi <-  "lashkar e jhangvi\\b|",
  ansar_al_islam <-  "ansar al islam\\b|",
  islamic_jihad_union <-  "islamic jihad union\\b|",
  harakat_ul_jihad_islam <-  "harkat ul jihad al islami\\b|",
  tehrik_taliban <-  "tehrik i taliban pakistan\\b|",
  indian_mujahedeen <-  "indian mujahideen\\b|",
  abdallah_azzam_brigades <-  "abdullah azzam brigades\\b|",
  haqqani_network <-  "haqqani network\\b|",
  ansar_al_dine <-  "ansar dine\\b|",
  ansaru <-  "ansaru\\b|",
  al_mulathamun_battalion <-  "al (mulathameen|mulathamun)\\b|",
  ansar_al_sharia <-  "ansar al sharia\\b|",
  al_nusra_front <-  "al nusra front\\b|",
  naqshbandi_army <-  "naqshbandi (army|order)\\b|",
  hizbul_mujahideen <-  "hizb ul mujahideen\\b|",
  al_ashtar_brigades <-  "al ashtar brigades\\b|",
  jamaat_nusrat <-  "nusrat al islam\\b|",
  asaib_ahl_al_haq <-  "asa'ib ahl al ha(q|qq)\\b|",
  harakat_sawad_misr <-  "arms of egypt movement\\b|",
  oromo_liberation_army <- "oromo liberation (army|front)\\b|",
  ola <- "ola\\b",
  ")"
)

# terrorism situation words. 
terrorism_situation_words <-  paste0(
  "(",
  terror_threat <-  "terror threats?\\b|",
  terror_risk <-  "terror risks?\\b|",
  terror_danger <-  "terror dangers?\\b|",
  terror_liklihood <-  "terror likelihood\\b|",
  terror_prospect <-  "terror prospects?\\b|",
  terror_hazard <-  "terror hazards?\\b|",
  terror_fears <-  "terror fears?\\b|",
  terror_anxiety <-  "terror anxiet(y|ies)\\b|",
  terror_uneasiness <-  "terror uneasiness\\b|",
  terror_capability <-  "terror capabilit(y|ies)\\b|",
  terror_capacity <-  "terror capacit(y|ies)\\b",
  ")"
)

### 3.2) TERRORISM groups

terrorism_4 <-  c(
  combine_terms(terrorism_actors, genocide_words),
  combine_terms(terrorism_actors, strong_violence_words),
  combine_terms(terrorism_actors, strong_seize_power_words),
  combine_terms(terrorism_actors, strong_terror_words)
)
terrorism_3 <-  c(
  combine_terms(terrorism_actors, strong_destruction_words),
  combine_terms(terrorism_actors, strong_clash_words),
  combine_terms(terrorism_actors, strong_capture_words),
  combine_terms(terrorism_actors, strong_seizure_words),
  combine_terms(terrorism_actors, weak_violence_words),
  combine_terms(terrorism_actors, weak_seize_power_words),
  combine_terms(terrorism_actors, weak_terror_words)
)
terrorism_2 <-  c(
  strong_terrorism_keywords,
  combine_terms(terrorism_actors, weak_destruction_words),
  combine_terms(terrorism_actors, weak_clash_words),
  combine_terms(terrorism_actors, weak_capture_words),
  combine_terms(terrorism_actors, weak_seizure_words),
  combine_terms(hyperbolic_situation_words, terrorism_situation_words)
)
terrorism_1 <-  c(
  weak_terrorism_keywords,
  terrorism_situation_words,
  terrorism_actors
)


# 4) SOCIAL_UNREST ############################################################

### 4.1) SOCIAL_UNREST  words

# strong unrest keywords. 
strong_unrest_keywords <-  paste0(
  "(",
  public_protest <- "public protest\\b|",
  social_unrest <-  "social unrest\\b|",
  social_upheaval <-  "social upheaval\\b|",
  ethnic_violence <-  "ethnic (violence|conflicts?)\\b|",
  racial_violence <-  "racial (violence|conflicts?)\\b|",
  sectarian_violence <-  "sectarian (violence|conflict)\\b|",
  mass_riots <-  "mass( | )rio(t|ts|ting)\\b|",
  mass_looting <-  "mass( | )loo(t|ts|ting)\\b",
  ")"
)
# weak unrest keywords. 
weak_unrest_keywords <-  paste0(
  "(",
  protest <-  "protest\\b|",
  unrest <-  "unrest\\b|",
  uprising <-  "uprisings?\\b|",
  ethnic_tensions <-  "ethnic tensions?\\b|",
  ethnic_tensions <-  "racial tensions?\\b|",
  riots <-  "rio(t|ts|ting)\\b|",
  looting <-  "loo(t|ts|ting)\\b",
  ")"
)

# unrest actors.
unrest_actors <-  paste0(
  "(",
  protesters <-  "protesters?\\b|",
  rioters <-  "rioters?\\b|",
  looters <-  "looters?\\b|",
  demonstrators <-  "demonstrators?\\b|",
  anarchists <-  "anarchists?\\b|",
  revolters <-  "revolters?\\b|",
  activists <-  "activists?\\b",
  ")"
)

# unrest situation words. 
unrest_situation_words <-  paste0(
  "(",
  civil_unrest <-  "civil unrest\\b|",
  social_tension <-  "social tensions?\\b|",
  social_upheaval <-  "social upheaval\\b|",
  social_conflict <-  "social conflict\\b|",
  social_chaos <-  "social chaos\\b|",
  ethnic_unrest <-  "ethnic unrest\\b|",
  ethnic_tension <-  "ethnic tensions?\\b|",
  ethnic_upheaval <-  "ethnic upheavals?\\b|",
  ethnic_conflict <-  "ethnic conflicts?\\b|",
  ethnic_chaos <-  "ethnic chaos\\b|",
  racial_unrest <-  "racial unrest\\b|",
  racial_tension <-  "racial tensions?\\b|",
  racial_upheaval <-  "racial upheavals?\\b|",
  racial_conflict <-  "racial conflicts?\\b|",
  racial_chaos <-  "racial chaos\\b|",
  hate_speech <-  "hate speech\\b",
  ")"
)

### 4.2) UNREST groups

social_unrest_4 <-  c(
  combine_terms(unrest_actors, genocide_words),
  combine_terms(unrest_actors, strong_violence_words),
  combine_terms(unrest_actors, strong_seize_power_words),
  combine_terms(unrest_actors, strong_clash_words),
  combine_terms(unrest_actors, strong_aggression_words)
)
social_unrest_3 <-  c(
  combine_terms(unrest_actors, strong_seizure_words),
  combine_terms(unrest_actors, strong_terror_words),
  combine_terms(unrest_actors, strong_destruction_words),
  combine_terms(unrest_actors, strong_capture_words),
  combine_terms(unrest_actors, weak_violence_words),
  combine_terms(unrest_actors, weak_seize_power_words),
  combine_terms(unrest_actors, weak_clash_words),
  combine_terms(unrest_actors, weak_aggression_words)
)
social_unrest_2 <-  c(
  strong_unrest_keywords,
  combine_terms(unrest_actors, weak_seizure_words),
  combine_terms(unrest_actors, weak_terror_words),
  combine_terms(unrest_actors, weak_destruction_words),
  combine_terms(unrest_actors, weak_capture_words),
  combine_terms(hyperbolic_situation_words, unrest_situation_words)
)
social_unrest_1 <-  c(
  weak_unrest_keywords,
  unrest_situation_words
)


# 5) REGIME_INSTABILITY ############################################################

### 5.1) REGIME_INSTABILITY  words

# strong regime keywords. 
strong_regime_keywords <-  paste0(
  "(",
  military_coup <-  "military coup\\b|",
  attempted_coup <-  "attempted coup\\b|",
  coup_attempt <-  "coup attempt\\b|",
  coup_plot <- "coup plot\\b|",
  regime_overthrow <-  "regime overthrow\\b|",
  regime_collapse <-  "regime collapse\\b|",
  military_takeover <-  "military takeover\\b|",
  insurrection <-  "insurrection\\b|",
  putsch <-  "putsch\\b|",
  secession <-  "secession\\b|",
  sedition <-  "sedition\\b",
  ")"
)
# weak regime keywords. 
weak_regime_keywords <-  paste0(
  "(",
  coup <-  "coup\\b|",
  regime_change <-  "regime change\\b|",
  oust <-  "ous(t|ts|ted|ting)\\b|",
  deposition <-  "depos(ition|e|es|ed|ing)\\b|",
  treason <-  "treaso(n|nous)\\b|",
  uprising <-  "uprisin(g|gs)\\b|",
  mutiny <-  "mutin(y|ous)\\b|",
  rebellion <-  "rebellion\\b|",
  revolution <-  "revolution\\b|",
  in_fighting <-  "in(-|)fighting\\b|",
  opposition_parties_protest <- "opposition parties protest\\b",
  ")"
)

# regime actors.
regime_actors <-  paste0(
  "(",
  rebels <-  "rebel(s| groups)\\b|",
  insurrectionists <-  "insurrectionists\\b|",
  insurgents <-  "insurgent(s| groups?)\\b|",
  secessionists <-  "secessionis(t|ts)\\b|",
  separatists <-  "separatists? groups?\\b|",
  seditionists <-  "seditionists? groups\\b|",
  militants <-  "militants? groups?\\b|",
  militia <-  "militias? groups?\\b|",
  war_lords <-  "war lor(d|ds)\\b|",
  conspirators <-  "conspirato(r|rs)\\b|",
  guerrillas <-  "guerrill(a|as)\\b|",
  revolutionaries <-  "revolutiona(ries|ary groups?|ary forces)\\b",
  ")"
)

# regime situation words. 
regime_situation_words <-  paste0(
  "(",
  regime_instability <-  "regime instability\\b|",
  regime_illegitimacy <-  "regime illegitimacy\\b|",
  regime_tensions <-  "regime tensions?\\b|",
  regime_fragility <-  "regime fragility\\b|",
  political_instability <-  "political instability\\b|",
  political_illegitimacy <-  "political illegitimacy\\b|",
  political_tensions <-  "political tensions?\\b|",
  political_fragility <-  "political fragility\\b|",
  party_instability <-  "party instability\\b|",
  party_illegitimacy <-  "party illegitimacy\\b|",
  party_tensions <-  "party tensions?\\b|",
  party_fragility <-  "party fragility\\b|",
  party_infighting <-  "party in( | )fighting\\b|",
  internal_political_tensions <-  "internal political tensions?\\b",
  ")"
)

### 5.2) REGIME groups

regime_instability_4 <-  c(
  combine_terms(regime_actors, genocide_words),
  combine_terms(unrest_actors, strong_aggression_words),
  combine_terms(unrest_actors, strong_violence_words),
  combine_terms(unrest_actors, strong_terror_words)
)
regime_instability_3 <-  c(
  combine_terms(regime_actors, genocide_words),
  combine_terms(unrest_actors, weak_aggression_words),
  combine_terms(unrest_actors, weak_violence_words),
  combine_terms(unrest_actors, weak_terror_words),
  combine_terms(regime_actors, strong_clash_words),
  combine_terms(unrest_actors, strong_seizure_words),
  combine_terms(unrest_actors, strong_destruction_words),
  combine_terms(unrest_actors, strong_capture_words)
)
regime_instability_2 <-  c(
  strong_regime_keywords,
  combine_terms(regime_actors, weak_clash_words),
  combine_terms(unrest_actors, weak_seizure_words),
  combine_terms(unrest_actors, weak_destruction_words),
  combine_terms(unrest_actors, weak_capture_words),
  combine_terms(hyperbolic_situation_words, regime_situation_words)
)
regime_instability_1 <-  c(
  weak_regime_keywords,
  regime_situation_words
)
