module Countries where
  
import Data.String

data CountryCode
    = ALB
    | ARM
    | AUS
    | AUT
    | AZE
    | BLR
    | BEL
    | BGR
    | HRV
    | CYP
    | CZE
    | DNK
    | EST
    | FIN
    | FRA
    | GEO
    | DEU
    | GRC
    | ISL
    | ISR
    | IRL
    | ITA
    | LVA
    | LTU
    | MLT
    | MDA
    | NLD
    | MKD
    | NOR
    | POL
    | PRT
    | ROU
    | RUS
    | SMR
    | SRB
    | SVN
    | ESP
    | SWE
    | UKR
    | GBR
  deriving (Show, Read, Eq, Ord)

getFlag :: IsString s => CountryCode -> s
getFlag ALB = ":flag_al:"
getFlag ARM = ":flag_am:"
getFlag AUS = ":flag_au:"
getFlag AUT = ":flag_at:"
getFlag AZE = ":flag_az:"
getFlag BLR = ":flag_by:"
getFlag BEL = ":flag_be:"
getFlag BGR = ":flag_bg:"
getFlag HRV = ":flag_hr:"
getFlag CYP = ":flag_cy:"
getFlag CZE = ":flag_cz:"
getFlag DNK = ":flag_dk:"
getFlag EST = ":flag_ee:"
getFlag FIN = ":flag_fi:"
getFlag FRA = ":flag_fr:"
getFlag GEO = ":flag_ge:"
getFlag DEU = ":flag_de:"
getFlag GRC = ":flag_gr:"
getFlag ISL = ":flag_is:"
getFlag ISR = ":flag_il:"
getFlag IRL = ":flag_ie:"
getFlag ITA = ":flag_it:"
getFlag LVA = ":flag_lv:"
getFlag LTU = ":flag_lt:"
getFlag MLT = ":flag_mt:"
getFlag MDA = ":flag_md:"
getFlag NLD = ":flag_nl:"
getFlag MKD = ":flag_mk:"
getFlag NOR = ":flag_no:"
getFlag POL = ":flag_pl:"
getFlag PRT = ":flag_pt:"
getFlag ROU = ":flag_ro:"
getFlag RUS = ":flag_ru:"
getFlag SMR = ":flag_sm:"
getFlag SRB = ":flag_sk:"
getFlag SVN = ":flag_sl:"
getFlag ESP = ":flag_es:"
getFlag SWE = ":flag_se:"
getFlag UKR = ":flag_ua:"
getFlag GBR = ":flag_gb:" 

getEmoji :: IsString s => CountryCode -> s
getEmoji ALB = "\127462\127473"
getEmoji ARM = "\127462\127474"
getEmoji AUS = "\127462\127482"
getEmoji AUT = "\127462\127481"
getEmoji AZE = "\127462\127487"
getEmoji BLR = "\127463\127486"
getEmoji BEL = "\127463\127466"
getEmoji BGR = "\127463\127468"
getEmoji HRV = "\127469\127479"
getEmoji CYP = "\127464\127486"
getEmoji CZE = "\127464\127487"
getEmoji DNK = "\127465\127472"
getEmoji EST = "\127466\127466"
getEmoji FIN = "\127467\127470"
getEmoji FRA = "\127467\127479"
getEmoji GEO = "\127468\127466"
getEmoji DEU = "\127465\127466"
getEmoji GRC = "\127468\127479"
getEmoji ISL = "\127470\127480"
getEmoji ISR = "\127470\127473"
getEmoji IRL = "\127470\127466"
getEmoji ITA = "\127470\127481"
getEmoji LVA = "\127473\127483"
getEmoji LTU = "\127473\127481"
getEmoji MLT = "\127474\127481"
getEmoji MDA = "\127474\127465"
getEmoji NLD = "\127475\127473"
getEmoji MKD = "\127474\127472"
getEmoji NOR = "\127475\127476"
getEmoji POL = "\127477\127473"
getEmoji PRT = "\127477\127481"
getEmoji ROU = "\127479\127476"
getEmoji RUS = "\127479\127482"
getEmoji SMR = "\127480\127474"
getEmoji SRB = "\127480\127472"
getEmoji SVN = "\127480\127473"
getEmoji ESP = "\127466\127480"
getEmoji SWE = "\127480\127466"
getEmoji UKR = "\127482\127462"
getEmoji GBR = "\127468\127463"

getName :: IsString s => CountryCode -> s
getName ALB = "Albania"
getName ARM = "Armenia"
getName AUS = "Australia"
getName AUT = "Austria"
getName AZE = "Azerbaijan"
getName BLR = "Belarus"
getName BEL = "Belgium"
getName BGR = "Bulgaria"
getName HRV = "Croatia"
getName CYP = "Cyprus"
getName CZE = "Czechia"
getName DNK = "Denmark"
getName EST = "Estonia"
getName FIN = "Finland"
getName FRA = "France"
getName GEO = "Georgia"
getName DEU = "Germany"
getName GRC = "Greece"
getName ISL = "Iceland"
getName ISR = "Israel"
getName IRL = "Ireland"
getName ITA = "Italy"
getName LVA = "Latvia"
getName LTU = "Lithuania"
getName MLT = "Malta"
getName MDA = "Moldova"
getName NLD = "The Netherlands"
getName MKD = "North Macedonia"
getName NOR = "Norway"
getName POL = "Poland"
getName PRT = "Portugal"
getName ROU = "Romania"
getName RUS = "Russia"
getName SMR = "San Marino"
getName SRB = "Serbia"
getName SVN = "Slovenia"
getName ESP = "Spain"
getName SWE = "Sweden"
getName UKR = "Ukraine"
getName GBR = "United Kingdom" 