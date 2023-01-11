# ---
# title: "MEA Acute Evaluation with assay controls"
# author: "Amy Carpenter"
# date: "July 18, 2022"
# output:
# html_document:
#    code_folding: hide
#    toc: true
#    toc_float: true
#    df_print: paged
# ---

#+ include = FALSE
knitr::opts_chunk$set(out.width = '100%')

library(data.table)
library(openxlsx)
library(stringi)
library(ggplot2)
library(plotly)


## # Background ----

## - The lack of sensitivity of the MEA Acute MFR endpoints in the SC toward positive controls is a bit concerning. 
## - We want to know if the mc mea acute endpoints enhance the sensitivity.
## - If so, then we are probably content with releasing the MEA Acute sc data as is
## - If not, then we might want to consider adjusting the mea acute sc cutoffs to be more sensitive
##
# Just including this so I can easily re-compile from R, dont' have to type it out
# knitr::spin('investigations/sc_and_cytotox_endpoints_July2022/R/check_mea_acute_sc_and_mc_results_against_assay_controls_2022-07-18.R', precious = T, doc = '##\\s*') 
##

## # Prepare data ----

## Load tcpl results & strickland supplement

# Load mc tcpl results
# What we want:
# mc5_mc6 <- as.data.table(read.xlsx('L:/Lab/NHEERL_MEA/ccte_shafer_mea_acute/ccte_shafer_mea_acute_multiconc_8Feb2022.xlsx', sheet = 'mc5+mc6'))
# but absolute paths aren't working in my RMD, have to reference relative to project path
current.wd <- getwd()
current.wd.minus.target <- sub('L:/Lab/NHEERL_MEA','',current.wd)
num.folders.up <- stri_count(current.wd.minus.target, fixed = '////')
mc5_mc6 <- as.data.table(read.xlsx(paste0(rep('../', times = num.folders.up),'L:/Lab/NHEERL_MEA/ccte_shafer_mea_acute/ccte_shafer_mea_acute_multiconc_8Feb2022.xlsx'), sheet = 'mc5+mc6'))

# OLD:
# load('../../mea_nfa_vs_acute/data/mea_acute_and_dev_mc5_mc6_2022-02-10.RData')
# mc5_mc6 <- mc5_mc6[assay_type == 'acute']

# Load sc tcpl results
# load('../../mea_nfa_vs_acute/data/mea_acute_and_dev_sc2_2022-02-10.RData')
# sc2 <- sc2[assay_type == 'acute']
load('investigations/sc_and_cytotox_endpoints_July2022/data/sc0_and_sc2_invitrodb_05042022_2022-08-03.RData')

# Strickland results, and consensus data
concordance.dat <- as.data.table(read.xlsx('investigations/sc_and_cytotox_endpoints_July2022/data/Stricklandetal2018_AcuteSC-Supplement1.xlsx', sheet = 'S2-DSSTox_MEAREF'))
concordance.dat <- concordance.dat[,1:14]
new.colnames <- unlist(concordance.dat[1,])
setnames(concordance.dat, old = names(concordance.dat), new = unlist(new.colnames))
concordance.dat <- concordance.dat[2:nrow(concordance.dat)]


## Lists of positives and negatives

# Function to wrangle tables
text_to_table <- function(text) {
  list <- stri_split(text, fixed = '\n')[[1]]
  tb <- rbindlist(lapply(list, function(rowi) as.list(stri_split(rowi, fixed = '\t')[[1]])))
  names(tb) <- unlist(tb[1,])
  tb <- tb[2:nrow(tb)]
}

# Prepare Validivia data
val.tb1.first.cohort <- "Compound name	CAS#	Class	Target/actionc	NVS_IC hitsd	MIEe	Known neurotoxicf	% wMFR change	MEA outcome	%Activity in ToxCastg
1st Cohort	Empty Cell	Empty Cell	Empty Cell	Empty Cell	Empty Cell	Empty Cell	Empty Cell	Empty Cell	Empty Cell
Aldicarb	116-06-3	Carbamate	AChE inhibition (reversible)	0	–	Yes	−7.9	NEG	2
Bensulide	741-58-2	Organophosphate	AChE inhibition	1	5	Yes	−88.7	POS	16
Carbaryl	63-25-2	Carbamate	AChE inhibition (reversible)	0	–	Yes	−30.5	POS	6
Chlorpyrifos	2921-88-2	Organophosphate	AChE inhibition	1	5	Yes	−47.5	POS	9
Chlorpyrifos oxon	5598-15-2	Organophosphate	AChE inhibition	0	–	Yes	−63.6	POS	9
Abamectin	71751-41-2	Mectin	GABA-a agonist	4	3,5,6	Yes	−100	POS	18
Emamectin	155569-91-8	Mectin	GABA-a agonist	4	3,4,5	Yes	−100	POS	23
Milbemectin	NOCAS_34742	Mectin	GABA-a agonist	3	3,6	Yes	−99.9	POS	21
Chlordane	57-74-9	Organochlorine	GABA-a antagonist	0	–	Yes	66.9	POS	15
Dieldrin	60-57-1	Organochlorine	GABA-a antagonist	0	–	Yes	56.8	POS	7
Endosulfan	115-29-7	Organochlorine	GABA-a antagonist	0	–	Yes	18.9	POS	13
Fipronil	120068-37-3	Phenylpyrazole	GABA-a antagonist	0	–	Yes	−86.9	POS	8
Heptachlor epoxide	1024-57-3	Organochlorine	GABA-a antagonist	0	–	Yes	2.5	NEG	6
Lindane	58-89-9	Organochlorine	GABA-a antagonist	0	–	Yes	93.7	POS	2
Methoxychlor	72-43-5	Organochlorine	GABA-a antagonist	0	–	Yes	−72.1	POS	11
Acetamiprid	135410-20-7	Neonicotinoid	Nicotinic agonist	2	7	Yes	14	NEG	1
Clothianidin	210880-92-5	Neonicotinoid	Nicotinic agonist	2	7	Yes	3.9	NEG	1
Imidacloprid	138261-41-3	Neonicotinoid	Nicotinic agonist	2	7	Yes	−20.5	POS	1
Nicotine	54-11-5	Neonicotinoid	Nicotinic agonist	2	7	Yes	3.3	NEG	1
Thiacloprid	111988-49-9	Neonicotinoid	Nicotinic agonist	2	7	Yes	−2.5	NEG	2
Thiamethoxam	153719-23-4	Neonicotinoid	Nicotinic agonist	0	–	Yes	−11.8	NEG	1
Pharma 1	298198-52-4	Failed Pharma	Nicotinic agonist, partial alpha 7	4	4,7,8	Yes	0.3	NEG	4
5,5-Diphenylhydantoin	57-41-0		Na channel antagonist	0	–	Yes	−51.2	POS	1
Allethrin	584-79-2	Pyrethroid	Na channel modifier	1	5	Yes	−100	POS	14
Bifenthrin	82657-04-3	Pyrethroid	Na channel modifier	0	–	Yes	17.3	POS	4
Cyfluthrin	68359-37-5	Pyrethroid	Na channel modifier	0	–	Yes	−99.8	POS	4
Fenvalerate	51630-58-1	Pyrethroid	Na channel modifier	0	–	Yes	−73.6	POS	6
Permethrin	52645-53-1	Pyrethroid	Na channel modifier	0	–	Yes	16.4	NEG	5
Eugenol	97-53-0	Phenylpropene	Na channel antagonist	0	–	Yes	−43.1	POS	2
p,p-DDT	50-29-3	Organochlorine	Na channel modifier	0	–	Yes	−27.7	POS	16
p,p-DDD	72-54-8	Organochlorine	Breakdown product of DDT	0	–	–	−95.3	POS	15
p,p-DDE	72-55-9	Organochlorine	Breakdown product of DDT	0	–	-	41.2	POS	14
Myclobutanil	88671-89-0	Conazole	Ergosterol biosynthesis inhibitor	1	4	–	−61.3	POS	5
Hexaconazole	79983-71-4	Conazole	Demethylation inhibitor	1	5	–	−90.7	POS	8
Propiconazole	60207-90-1	Conazole	Demethylation inhibitor	1	4	–	−92.6	POS	7
Tetraconazole	112281-77-3	Conazole	Demethylation inhibitor	3	3,4	–	−87.2	POS	10
Flusilazole	85509-19-9	Organosilicone fungicide	Ergosterol biosynthesis inhibitor	2	3,4	–	−82.8	POS	8
Imazalil	35554-44-0	Fungicide	Ergosterol biosynthesis inhibitor	1	4	–	−82.8	POS	12
1,2-Propylene glycol	57-55-6	Glycol	No intended target	0	–	–	−1.6	NEG	1
1,3-Diphenylguanidine	102-06-7	Plasticizer	No intended target	2	3,5	–	−96.9	POS	5
Saccharin	82385-42-0	Artificial sweetener	No intended target	NEG-Cont	–	–	−24.7	POS	1
Acetaminophen	103-90-2	Aniline analgesics	COX inhibitor	NEG-Cont	–	–	−9.7	NEG	1
Amiodaronea	19774-82-4	Antiarrhythmic	Adrenergic blocker	4	2,3,4	–	−69	POS	24
Cyazofamid	120116-88-3	Fungicide	Complex III Qi inhibitor	2	7	–	−63.7	POS	11
Dibutyl phthalate	84-74-2	Plasticizer	No intended target	0	–	–	−71.3	POS	6
Diphenhydramine	147-24-0	antihistamine	H1 receptor antagonist	2	3,4	Yes	−92.3	POS	7
Enadoline	124378-77-4	Failed pharma	Kappa-opioid agonist	0	–	Yes	−80.8	POS	1
Haloperidol	52-86-8	Antipsychotic	Dopamine Antagonist	3	3,4	Yes	−90.8	POS	12
Isotiazoline	26172-55-4	Isothiazolinones	Biocide	0	–	–	−17.3	POS	8
Maneb	12427-38-2	Herbicide(dithiocarbamate)	Dopamine β-hydroxylase inhibitor	0	–	Yes	9.1	NEG	13
Mepiquat	24307-26-4	plant growth regulator	Inhibits gibberellic acid synthesis	2	7	Yes	−7.3	NEG	1
Pentamidinea	140-64-7	Anti-microbial	Nuclear metabolism inhibitor	3	1,3,4	Yes	−96.1	POS	15
Reserpine	50-55-5	Antihypertensive	Monoamine transmitter depletion	3	3,4	Yes	−99.5	POS	13
Rotenone	83-79-4	Botanical insecticide	Electron transport inhibitor	0	–	Yes	−94.4	POS	11
Spiroxamine	118134-30-8	Fungicide	Fungal RNA polymerase inhibition	2	3,4	–	−40.7	POS	5
Thidiazuron	51707-55-2	Plant growth regulator	Plant growth regulator	2	5	–	−57.2	POS	3
Tributyltin	1461-22-9	Organometal	Multiple potential neuronal	4	2,3,4	Yes	−100	POS	27
Volinanserin	139290-65-6		5-HT2A antagonist	3	3,4	Yes	−89	POS	6
Zamifenacin	127308-82-1		Muscarinic antagonist (m3)	3	3,4	Yes	−62.9	POS	17
Acrylamide	79-06-1			0	–	Yes	34.5	POS	1
Butachlor	23184-66-9	Herbicide		1	3	–	7.1	NEG	15
Diethyl butanedioate	123-25-1	flavoring/food additive		0	–	–	−20.4	POS	1
Difenzoquat	43222-48-6	Herbicide		2	3,4	Yes	−97.8	POS	7
Pharma 2	NOCAS_47377	Failed pharma		5	2,3,4,5,6	–	−78.7	POS	25
Pharma 3	349495-42-7	Failed pharma		0	–	–	−29.9	POS	4
Pharma 4	NOCAS_47362	Failed pharma		4	3,4,5	–	−78.5	POS	16
Pharma 5	478149-53-0	Failed pharma		3	4,7,8	–	18.8	POS	2
Pharma 6	NOCAS_47387	Failed pharma		4	2,3,4	–	−99.2	POS	18"
val.tb1.second.cohort	<- "Compound name	CAS#	Class	Target/actionc	NVS_IC hitsd	MIEe	Known neurotoxicf	% wMFR change	MEA outcome	%Activity in ToxCastg
Cyproconazole	94361-06-5	Conazole		0	–	–	−39.4	POS	3
Difenoconazole	119446-68-3	Conazole		0	–	–	−95.3	POS	14
Diniconazole	83657-24-3	Conazole		0	–	–	−100	POS	12
Fenbuconazole	114369-43-6	Conazole		0	–	–	−100	POS	4
Fluconazole	86386-73-4	Conazole		0	–	–	−28	NEG	0
1H-1,2,4-Triazole	288-88-0	Conazole precursor/synthesis		0	–	–	−25.6	NEG	1
Ziram	137-30-4	Dimethyldithiocarbamate		0	–	Yes	6	NEG	17
Mancozeb	2234562	Dithiocarbamate		0	–	–	−8.7	NEG	16
Lactofen	77501-63-4	Herbicide	Protoporphyrinogen oxidase inhibitor	0	–	–	−97.9	POS	7
17beta-Estradiol	50-28-2	Hormone	ER-Agonist	0	–	Yes	−37.6	NEG	11
17beta-Trenbolone	10161-33-8	Hormone	AR-Agonist	0	–	–	−65.7	POS	13
Prochloraz	67747-09-5	Imidazole fungicide?	AR-Antagonist	0	–	–	−100	POS	14
Fenthion	55-38-9	Organothiophosphate	AChE inhibition	0	–	Yes	−68.5	POS	5
Tetramethrin	7696-12-0	Pyrethroid	Na channel modifier	0	–	Yes	−90.1	POS	7
Piperonyl butoxide	51-03-6	Synergist	P450-inhibition	0	–	–	−51.8	POS	6
Butyl benzyl phthalate	85-68-7			0	–	–	−99.2	POS	7
Di(2-ethylhexyl) phthalate	117-81-7			0	–	–	0.3	NEG	4
Genistein	446-72-0		ER-agonist	0	–	Yes	−75.7	POS	14
Perfluorooctane sulfonic acid	1763-23-1			0	–	–	−10.1	NEG	15
Vinclozolin	50471-44-8		AR-antagonist	0	–	–	−83.6	POS	3
Amoxicillinb	26787-78-0			NEG-Cont	–	–	−4.9	NEG	NA
Glyphosateb	1071-83-6			NEG-Cont	–	–	3.6	NEG	NA
Saccharinb	82385-42-0			NEG-Cont	–	–	−6.5	NEG	1
Salicylic acidb	69-72-7			NEG-Cont	–	–	−1.6	NEG	4
Sorbitolb	50-7-4			NEG-Cont	–	–	−16.3	NEG	NA"
# Note: had to remove the ' from p-p'-DDT and others to compile
# Also added col names from first cohort to second cohort manually

# wrangle chemical list from table 1 McConnell, 2012
pos <- 'Chemical positives	Chemical class	CAS #	Vehicle	Purity (%)	Sourcec	Previous data in MEAs
Bicuculline	GABAA antagonist	40709-69-1	DMSO/EtOH	≥90	Sigma	Gross et al. (1997)
Bifenthrina	VGSC pesticide	82657-04-3	DMSO	89.0	Gift	Losa et al. (2009)
Carbaryl	AChE inhibitor	63-25-2	DMSO	99.8	Chem Service	Defranchi et al. (2011)
Chlorpyrifos oxon	AChE inhibitor	5598-15-2	DMSO	>98	Chem Service	Unpublished lab data
b-Cyfluthrina	VGSC pesticide	68359-37-5	DMSO	99.2	Gift	Losa et al. (2009)
Deltamethrin	VGSC pesticide	52918-63-5	DMSO	99.5	Chem Service	Meyer et al. (2008)
Diazepam	GABAA modulator	439-14-5	DMSO	>98	Sigma	Novellino et al. (2011)
Domoic acidb	Glutamate R antagonist	14277-97-5	H2O	≥90	Sigma	Hogberg et al. (2011)
Fipronil	GABAA antagonist	120068-37-3	DMSO	98.5	Chem Service	Defranchi et al. (2011)
Fluoxetine	SSRI	114247-09-5	DMSO	>98	Sigma	Novellino et al. (2011)
Imidacloprid	nAChR pesticide	138261-41-3	DMSO	99.5	Chem Service	–
Ketamine	NMDA R antagonist	33795-24-3	DMSO	>99	Sigma	Gross et al. (1995)
Leadb	Neurotoxic heavy metal	6080-56-4	H2O	>98	Aldrich	–
l-Glutamate	Glutamate R agonist	19285-83-7	H2O	≥98	Sigma	Frega et al. (2011)
Lindane	GABAA antagonist	58-89-9	DMSO	97	Aldrich	Unpublished lab data
Methylmercuryb	Neurotoxic heavy metal	115-09-3	DMSO	93	Aldrich	van Vliet et al. (2007)
Muscimol	GABAA agonist	18174-72-6	H2O	≥98	Sigma	Novellino et al. (2011)
Nicotine	nAChR agonist	54-11-5	DMSO	>99	Sigma	Defranchi et al. (2011)
Permethrin	VGSC pesticide	52645-53-1	DMSO	55	Chem Service	Meyer et al. (2008)
RDXa	GABAA antagonist	121-82-4	DMSO	>99.5	Gift	Williams et al. (2010)
Trimethyltinb	Neurotoxic heavy metal	56-24-6	H2O	>95	ICN Biomedicals	Gramowski et al. (2000)
Valproic acid	Broad spectrum anticonvulsant	1069-66-5	DMSO	>98	Sigma	Gross et al. (1995)
Verapamil	VGSC blocker	152-11-4	DMSO	≥99.0	Sigma	Novellino et al. (2011)'

neg <- 'Chemical negatives	Chemical class	CAS #	Vehicle	Purity (%)	Sourcec	Reference
Acetaminophen	Cox-2 inhibitor	103-90-2	DMSO	99	Sigma	Breier et al. (2008)
Amoxicillin	Antibiotic	26787-78-0	DMSO	N/A	Sigma	Breier et al. (2008)
Glyphosate	Herbicide	1071-83-6	H2O	>99	Chem Service	Breier et al. (2008)
Paraquat	Herbicide	1910-42-5	H2O	99.9	Sigma	Defranchi et al. (2011)
Saccharin	Sweetener	82385-42-0	DMSO	>99	Sigma	Breier et al. (2008)
Salicylic acid	Phytohormone	69-72-7	DMSO	≥99	Sigma	Defranchi et al. (2011)
D-Sorbitol	Sweetener	50-70-4	DMSO	>98	Sigma	Breier et al. (2008)'

## Label chemical identify in strickland 2018
mcc.pos <- text_to_table(pos)
mcc.neg <- text_to_table(neg)
val.1 <- text_to_table(val.tb1.first.cohort)
val.2 <- text_to_table(val.tb1.second.cohort)
# View(val.1)
# View(val.2)

# Load positives/negatives from Kosnik, 2019
kosnik.tb1 <- as.data.table(read.xlsx('investigations/sc_and_cytotox_endpoints_July2022/data/Kosnik2019-Supplementary_file1.xlsx', sheet = 'Table S1'))
kosnik.pos <- kosnik.tb1[, c(1:2)]
kosnik.neg <- kosnik.tb1[!is.na(Negatives), c(3:4)]

# Combine together
assay.controls.tb <- rbind(mcc.pos[, .(control_type = 'positive', CASRN = `CAS #`, name = `Chemical positives`, class = `Chemical class`, ref = 'McConnel2012 assay control')],
                           mcc.neg[, .(control_type = 'negative', CASRN = `CAS #`, name = `Chemical negatives`, class = `Chemical class`, ref = 'McConnel2012 assay control')],
                           val.1[grepl('Yes',`Known neurotoxicf`), .(control_type = 'positive', CASRN = `CAS#`, name = `Compound name`, class = Class, ref = 'Validivia2014 known neurotoxic, 1st cohort')],
                           val.2[grepl('Yes',`Known neurotoxicf`), .(control_type = 'positive', CASRN = `CAS#`, name = `Compound name`, class = Class, ref = 'Validivia2014 known neurotoxic, 2nd cohort')],
                           val.2[`NVS_IC hitsd` == 'NEG-Cont', .(control_type = 'negative', CASRN = `CAS#`, name = `Compound name`, class = Class, ref = 'Validivia2014 negative control, 2nd cohort')],
                           kosnik.pos[Neuroactives != 'CASRN', .(control_type = 'positive', CASRN = Neuroactives, name = X2, ref = 'Kosnik et al 2019 S1')],
                           kosnik.neg[Negatives != 'CASRN', .(control_type = 'negative', CASRN = Negatives, name = X4, ref = 'Kosnik et al 2019 S1')],
                           fill = TRUE)

# collapse if there were multiple references for same casrn
assay.controls.tb2 <- assay.controls.tb[, .(refs = paste0(sort(unique(ref)),collapse = ";"),
                                            class = paste0(sort(unique(class)),collapse = ";"),
                                            name = paste0(sort(unique(name)),collapse = ";")), by = .(control_type, CASRN)]
# View(assay.controls.tb2)
check.chem <- assay.controls.tb2[, .N, by = .(CASRN)][N > 1, CASRN]
assay.controls.tb2[CASRN %in% check.chem]
# empty, good

## Merge with MEA results
mc5_mc6 <- merge(mc5_mc6, assay.controls.tb2, by.x = 'casn', by.y = 'CASRN', all.x = T)
sc2 <- merge(sc2, assay.controls.tb2, by.x = 'casn', by.y = 'CASRN', all.x = T)
concordance.dat <- merge(concordance.dat, assay.controls.tb2, by.x = 'Substance_CASRN', by.y = 'CASRN', all.x = T)


## # Analyze activity in MFR endpoints only ----

## Note: the un-weighted mean firing rate was used in Strickland et al 2018
mc5_mc6[grepl('firing_rate',aenm), .N, by = aenm]

# is the endpoint in Strickland weighted?

## Create combined data summary

# activity.tb <- rbind(mc5_mc6[aenm %in% c('CCTE_Shafer_MEA_acute_firing_rate_mean_dn','CCTE_Shafer_MEA_acute_firing_rate_mean_up'), 
#                              .(type = 'mc',
#                                mea_activity = ifelse(any(hitc %in% 1), 1, 0)), by = .(spid, dsstox_substance_id, casn, chnm, control_type)],
#                      sc2[grepl('MFR',aenm), 
#                          .(type = 'sc',
#                            mfr_activity = ifelse(any(hitc %in% 1), 1, 0)), by = .(spid, dsstox_substance_id, casn, chnm, control_type)],
#                      concordance.dat[, .(type = 'MEA_Ref_Consensus',
#                                          mfr_activity = MEA_Ref_Consensus,
#                                          dsstox_substance_id = DSSTox_Substance_Id, 
#                                          casn = Substance_CASRN, chnm = Substance_Name, control_type)],
#                      fill = TRUE)
activity.tb <- rbind(mc5_mc6[aenm %in% c('CCTE_Shafer_MEA_acute_firing_rate_mean_dn','CCTE_Shafer_MEA_acute_firing_rate_mean_up'), 
                             .(type = 'mc',
                               mfr_activity = ifelse(any(hitc %in% 1), 1, 0)), by = .(spid, dsstox_substance_id, casn, chnm, control_type)],
                     sc2[grepl('MFR',aenm), 
                         .(type = 'sc',
                           mfr_activity = ifelse(any(hitc %in% 1), 1, 0)), by = .(spid, dsstox_substance_id, casn, chnm, control_type)])
rm(list = setdiff(ls(),c('mc5_mc6','sc2','activity.tb','concordance.dat')))

# Prepare data to plot
# activity.tb[, casn_tested_in_tcpl := as.numeric(any(type %in% c('sc','mc'))), by = .(casn)]
plotdat <- activity.tb[!is.na(control_type)]
plotdat$type <- factor(plotdat$type, levels =c('sc','mc'), ordered = T)

# define yaxis
plotdat[, chnm_stnd := paste0(sort(unique(chnm[!is.na(chnm)])),collapse = ","), by = .(casn)]
plotdat[, plot_chnm := ifelse(nchar(chnm_stnd) <= 27, chnm_stnd, paste0(stri_sub(chnm_stnd, 1, 27),'...'))]
plotdat[, .(length(unique(dsstox_substance_id))), by = .(plot_chnm)][V1 > 1]
# empty -> so chnm can be used as y=axis

# Any cases where different spid have different hit calls?
check.chem <- plotdat[, .(length(unique(mfr_activity))), by = .(dsstox_substance_id, type)][V1 > 1, dsstox_substance_id]
plotdat[dsstox_substance_id %in% check.chem]
# Let's use the "hit,negative" notation that will merge with the mea consensus info

plotdat2 <- plotdat[, .(mfr_activity_mean = mean(mfr_activity, na.rm = T)),by = .(dsstox_substance_id, casn, plot_chnm, control_type, type)]
plotdat2[mfr_activity_mean == 0, mfr_activity_text := 'neg']
plotdat2[mfr_activity_mean == 1, mfr_activity_text := 'hit']
plotdat2[mfr_activity_mean > 0 & mfr_activity_mean < 1, mfr_activity_text := 'hit,neg']
plotdat2$mfr_activity_text <- factor(plotdat2$mfr_activity_text, levels = sort(unique(plotdat2$mfr_activity_text)), ordered = T)

# # Create order for chemicals
# tb.wide <- dcast(plotdat2, plot_chnm ~ type, value.var = 'mfr_activity_mean')
# tb.wide[, c(names(tb.wide)[!grepl('plot_chnm',names(tb.wide))]) := lapply(.SD, function(coli) {
#   coli[is.na(coli)] <- -1
#   coli
# }), .SDcols = c(names(tb.wide)[!grepl('plot_chnm',names(tb.wide))])]
# chem.levels <- tb.wide[order(sc, mc), plot_chnm]
# plotdat2$plot_chnm <- factor(plotdat2$plot_chnm, levels = chem.levels, ordered = T)

# Merge in activity from mea consensus
# First, get the identifiers as the appear in plotdat
concordance.dat.overlap <- merge(concordance.dat, plotdat2[, unique(.SD), .SDcols = c('dsstox_substance_id','casn','plot_chnm')],
                                 all = F, by.x = 'DSSTox_Substance_Id', by.y = 'dsstox_substance_id')
concordance.dat.overlap[,type:='MEA_Ref_Consensus']
concordance.dat.overlap[, mfr_activity_text := tolower(MEA_Ref_Consensus)]
plotdat3 <- rbind(plotdat2,
                  concordance.dat.overlap,
                  fill = T)

# Order for substances
plotdat3$mfr_activity_text <- factor(plotdat3$mfr_activity_text, levels = c('hit','hit,neg','neg','not tested'), ordered = T)
tb.wide <- dcast(plotdat3, plot_chnm ~ type, value.var = 'mfr_activity_text')
tb.wide[, c(names(tb.wide)[!grepl('plot_chnm',names(tb.wide))]) := lapply(.SD, function(coli) {
  coli[is.na(coli)] <- 'not tested'
  coli
}), .SDcols = c(names(tb.wide)[!grepl('plot_chnm',names(tb.wide))])]
chem.levels <- tb.wide[order(sc, mc, MEA_Ref_Consensus), plot_chnm]
plotdat3$plot_chnm <- factor(plotdat3$plot_chnm, levels = chem.levels, ordered = T)

# Clean up x-axis labels
plotdat3[type == 'sc', plot_type := 'single-conc\n(MFR up or dn)']
plotdat3[type == 'mc', plot_type := 'multi-conc\n(MFR up or dn)']
plotdat3[type == 'MEA_Ref_Consensus', plot_type := 'MEA reference\nlit. consensus']
# plotdat3$type <- factor(plotdat3$type, levels =c('sc','mc','MEA_Ref_Consensus'), ordered = T)
plotdat3$plot_type <- factor(plotdat3$plot_type, levels =c('single-conc\n(MFR up or dn)','multi-conc\n(MFR up or dn)','MEA reference\nlit. consensus'), ordered = T)


#+ fig.height = 11

# Plot it
ggplot(plotdat3, aes(x = plot_type, y = plot_chnm))+
  geom_tile(aes(fill = mfr_activity_text), col = 'white')+
  scale_x_discrete(position = 'top')+
  ylab('')+
  xlab('')+
  scale_y_discrete(position = 'right')+
  theme_bw()+
  facet_grid(rows = vars(control_type),
             scale = 'free_y',
             space = 'free_y',
             switch = 'y')+
  theme(strip.text.y = element_text(angle = 0),
        strip.text = element_text(angle = 0),
        legend.position = 'top')+
  ggtitle('MEA Acute MFR activity of assay controls in TCPL\nin sc, mc, and compared to previous studies')

## Details:
##
## * Hit = hit = 1 for MFR down OR up. Neg = no hits.
## * Control type: positive and negative identified in McConnell et al., 2012 (see Table 1) and Validivia et al., 2014 (See Table 1, "Known Neurotic"=="Yes" and where  "NVS_IC hits"=="Neg-Cont")
## * activity in TCPL: 'hit' = hitc == 1 for MFR up or dn endpoint
## * activity in Strickland 2018 = hit in the up or dn direction for the normalized MFR. Data was this studies was pipelined in TCPL
## * Consensus from other MEA studies: Consensus of activity of in MEA assays from published literature. Largely Validivia et al., 2014, but many other studies included as well. See Strickland et al., 2018 Supplment Table 2
##

## <br>Summary of results:
summary.tb <- activity.tb[!is.na(control_type), .(num_chem_screened = length(unique(dsstox_substance_id)),
                                                  num_chem_detected_any_spid = length(unique(dsstox_substance_id[mfr_activity == 1]))),
                          by = .(type, control_type)]
summary.tb[, percent_detected := signif(num_chem_detected_any_spid/num_chem_screened,3)*100]
summary.tb[order(type, control_type)]

## Pooled across mc and sc
summary.tb <- activity.tb[!is.na(control_type), .(num_chem_screened = length(unique(dsstox_substance_id)),
                                                  num_chem_detected_any_spid = length(unique(dsstox_substance_id[mfr_activity == 1]))),
                          by = .(control_type)]
summary.tb[, percent_detected := signif(num_chem_detected_any_spid/num_chem_screened,3)*100]
summary.tb



## # Analyze activity with additional mc endpoints ----

# Get the list of mea acute acnm's that we are releasing
# main15.acnms <- as.data.table(read.csv('../mea_acute_main15_acnm_aenm_2020-12-08.csv'))

## Get activity from any of the 15 "main" acnm's, exlcuding LDH adn AB
mc5_mc6[, length(unique(aenm))]
activity.tb <- rbind(mc5_mc6[!grepl('(LDH)|(AB)',aenm), 
                             .(type = 'mc',
                               mea_activity = ifelse(sum(hitc %in% 1) >= 3, 1, 0)), by = .(spid, dsstox_substance_id, casn, chnm, control_type)],
                     sc2[grepl('MFR',aenm), 
                         .(type = 'sc',
                           mea_activity = ifelse(any(hitc %in% 1), 1, 0)), by = .(spid, dsstox_substance_id, casn, chnm, control_type)])
rm(list = setdiff(ls(),c('mc5_mc6','sc2','activity.tb','concordance.dat')))

# Prepare data to plot
# activity.tb[, casn_tested_in_tcpl := as.numeric(any(type %in% c('sc','mc'))), by = .(casn)]
plotdat <- activity.tb[!is.na(control_type)]
plotdat$type <- factor(plotdat$type, levels =c('sc','mc'), ordered = T)

# define yaxis
plotdat[, chnm_stnd := paste0(sort(unique(chnm[!is.na(chnm)])),collapse = ","), by = .(casn)]
plotdat[, plot_chnm := ifelse(nchar(chnm_stnd) <= 27, chnm_stnd, paste0(stri_sub(chnm_stnd, 1, 27),'...'))]
# plotdat[, .(length(unique(dsstox_substance_id))), by = .(plot_chnm)][V1 > 1]
# empty -> so chnm can be used as y=axis

# Any cases where different spid have different hit calls?
check.chem <- plotdat[, .(length(unique(mea_activity))), by = .(dsstox_substance_id, type)][V1 > 1, dsstox_substance_id]
plotdat[dsstox_substance_id %in% check.chem]
# Let's use the "hit,negative" notation that will merge with the mea consensus info

plotdat2 <- plotdat[, .(mea_activity_mean = mean(mea_activity, na.rm = T)),by = .(dsstox_substance_id, casn, plot_chnm, control_type, type)]
plotdat2[mea_activity_mean == 0, mea_activity_text := 'neg']
plotdat2[mea_activity_mean == 1, mea_activity_text := 'hit']
plotdat2[mea_activity_mean > 0 & mea_activity_mean < 1, mea_activity_text := 'hit,neg']
plotdat2$mea_activity_text <- factor(plotdat2$mea_activity_text, levels = sort(unique(plotdat2$mea_activity_text)), ordered = T)

# Merge in activity from mea consensus
# First, get the identifiers as the appear in plotdat
concordance.dat.overlap <- merge(concordance.dat, plotdat2[, unique(.SD), .SDcols = c('dsstox_substance_id','casn','plot_chnm')],
                                 all = F, by.x = 'DSSTox_Substance_Id', by.y = 'dsstox_substance_id')
concordance.dat.overlap[,type:='MEA_Ref_Consensus']
concordance.dat.overlap[, mea_activity_text := tolower(MEA_Ref_Consensus)]
plotdat3 <- rbind(plotdat2,
                  concordance.dat.overlap,
                  fill = T)

# Order for substances
plotdat3$mea_activity_text <- factor(plotdat3$mea_activity_text, levels = c('hit','hit,neg','neg','not tested'), ordered = T)
tb.wide <- dcast(plotdat3, plot_chnm ~ type, value.var = 'mea_activity_text')
tb.wide[, c(names(tb.wide)[!grepl('plot_chnm',names(tb.wide))]) := lapply(.SD, function(coli) {
  coli[is.na(coli)] <- 'not tested'
  coli
}), .SDcols = c(names(tb.wide)[!grepl('plot_chnm',names(tb.wide))])]
chem.levels <- tb.wide[order(sc, mc, MEA_Ref_Consensus), plot_chnm]
plotdat3$plot_chnm <- factor(plotdat3$plot_chnm, levels = chem.levels, ordered = T)

# Clean up x-axis labels
plotdat3[type == 'sc', plot_type := 'single-conc\n(MFR up or dn)']
plotdat3[type == 'mc', plot_type := 'multi-conc\n(3 of more any MEA hits)']
plotdat3[type == 'MEA_Ref_Consensus', plot_type := 'MEA reference\nlit. consensus']
# plotdat3$type <- factor(plotdat3$type, levels =c('sc','mc','MEA_Ref_Consensus'), ordered = T)
plotdat3$plot_type <- factor(plotdat3$plot_type, levels =c('single-conc\n(MFR up or dn)','multi-conc\n(3 of more any MEA hits)','MEA reference\nlit. consensus'), ordered = T)


#+ fig.height = 11

# Plot it
ggplot(plotdat3, aes(x = plot_type, y = plot_chnm))+
  geom_tile(aes(fill = mea_activity_text), col = 'white')+
  scale_x_discrete(position = 'top')+
  ylab('')+
  xlab('')+
  scale_y_discrete(position = 'right')+
  theme_bw()+
  facet_grid(rows = vars(control_type),
             scale = 'free_y',
             space = 'free_y',
             switch = 'y')+
  theme(strip.text.y = element_text(angle = 0),
        strip.text = element_text(angle = 0),
        legend.position = 'top')+
  ggtitle('MEA Acute MEA activity of assay controls in TCPL\nin sc (MFR up/dn only), mc (3+/15 MEA hits, up or dn),\nand previous MEA studies')

## (Old method, by chemical instead of by spid:)
# summary.tb <- activity.tb[!is.na(control_type), .(num_screened = length(unique(dsstox_substance_id)),
#                                                   num_any_hit = length(unique(dsstox_substance_id[mea_activity == 1]))),
#                           by = .(type, control_type)]
# summary.tb[, percent_detected := signif(num_any_hit/num_screened,3)*100]
# summary.tb[order(type, control_type)]
# 
# summary.tb <- activity.tb[!is.na(control_type), .(num_screened = length(unique(dsstox_substance_id)),
#                                                   num_any_hit = length(unique(dsstox_substance_id[mea_activity == 1]))),
#                           by = .(control_type)]
# summary.tb[, percent_detected := signif(num_any_hit/num_screened,3)*100]
# summary.tb

## <br>Summary of results:
summary.tb <- activity.tb[!is.na(control_type), .(num_chem_screened = length(unique(dsstox_substance_id)),
                                                  num_chem_detected_any_spid = length(unique(dsstox_substance_id[mea_activity == 1]))),
                          by = .(type, control_type)]
summary.tb[, percent_detected := signif(num_chem_detected_any_spid/num_chem_screened,3)*100]
summary.tb[order(type, control_type)]

## Pooled across mc and sc
summary.tb <- activity.tb[!is.na(control_type), .(num_chem_screened = length(unique(dsstox_substance_id)),
                                                  num_chem_detected_any_spid = length(unique(dsstox_substance_id[mea_activity == 1]))),
                          by = .(control_type)]
summary.tb[, percent_detected := signif(num_chem_detected_any_spid/num_chem_screened,3)*100]
summary.tb


## # Analyze activity with additional mc endpoints, selective hits only ----

# Get the list of mea acute acnm's that we are releasing
# main15.acnms <- as.data.table(read.csv('../mea_acute_main15_acnm_aenm_2020-12-08.csv'))


## From Kosnik et al., 2019: To avoid counting cytotoxic chemical activity as activity in a parameter, the AC50 values of these chemicals in the CTB assay were compared to the AC50 values of the chemicals in any other parameter and only those chemical-parameter hits with AC50s less than the chemical- CTB AC50 were counted as hits."<br>
## Also note that in Kosnik et al., 2019, they only used the CTB assay to characterize the cytotoxicity, not the LDH.
## So, let's require 3 selective hits for a substance to be considered "detected" in the multi-conc assay, based on the potency in the CTB assay.

## However, I just discovered that 6 substances don't have any AB data:
spid.tested.ab <- mc5_mc6[grepl('AB',aenm), unique(spid)]
mc5_mc6[!spid %in% spid.tested.ab, .(hit_count = sum(hitc %in% 1)), by = .(dsstox_substance_id, chnm, spid, control_type)]
# okay, so one of these substances is a positive control too. So I really can't ignore it
# options:
# - just assum all of the hits are selective
# - use the median modl_ga value from all other substances
# - use the LDH cytotoxicity as a surrogate
# - investigate why this substance doesn't have any AB data! (i'm guessing it was a wllq issue)

## Were any of these active in the LDH assay?
mc5_mc6[!spid %in% spid.tested.ab & grepl('LDH',aenm), .(dsstox_substance_id, chnm, spid, control_type, hitc)]
# nope, all inactive.

## I just confirmed that the AB data is indeed missing for these substances. Looks like the assay might not have been run at all (bc the values just aren't in the expected L drive folder).<br>
## Regardless, I think I would advise anyone using this data to assume that the hits are selective in the absence of cytoxocity data. So I'm going to default to the max cytotoxicity value.

## <br>Define the "Selective" hits
mc5_mc6[, summary(modl_ga)]
# max is 2. I'll set the default ab upper bound to 4
mc5_mc6[, ab_log10ac50 := ifelse(hitc[grepl('AB',aenm)] %in% 1, modl_ga[grepl('AB',aenm)], 4), by = .(dsstox_substance_id, spid)]
mc5_mc6[is.na(ab_log10ac50), ab_log10ac50 := 4]
mc5_mc6[, sel_hitc := as.numeric(hitc %in% 1 & modl_ga < ab_log10ac50), by = .(dsstox_substance_id, spid, aeid, aenm)]
mc5_mc6[, .N, by = .(hitc, sel_hitc)]

## Note that the hitc == -1 doesn't affect any of the assay controls
mc5_mc6[hitc %in% -1, .N, by = .(control_type)]

## Get activity from any of the 15 "main" acnm's, exlcuding LDH adn AB
mc5_mc6[, length(unique(aenm))]
activity.tb <- rbind(mc5_mc6[!grepl('(LDH)|(AB)',aenm), 
                             .(type = 'mc',
                               mea_activity = ifelse(sum(sel_hitc %in% 1) >= 3, 1, 0)), by = .(spid, dsstox_substance_id, casn, chnm, control_type)],
                     sc2[grepl('MFR',aenm), 
                         .(type = 'sc',
                           mea_activity = ifelse(any(hitc %in% 1), 1, 0)), by = .(spid, dsstox_substance_id, casn, chnm, control_type)])
rm(list = setdiff(ls(),c('mc5_mc6','sc2','activity.tb','concordance.dat')))

# Prepare data to plot
# activity.tb[, casn_tested_in_tcpl := as.numeric(any(type %in% c('sc','mc'))), by = .(casn)]
plotdat <- activity.tb[!is.na(control_type)]
plotdat$type <- factor(plotdat$type, levels =c('sc','mc'), ordered = T)

# define yaxis
plotdat[, chnm_stnd := paste0(sort(unique(chnm[!is.na(chnm)])),collapse = ","), by = .(casn)]
plotdat[, plot_chnm := ifelse(nchar(chnm_stnd) <= 27, chnm_stnd, paste0(stri_sub(chnm_stnd, 1, 27),'...'))]
# plotdat[, .(length(unique(dsstox_substance_id))), by = .(plot_chnm)][V1 > 1]
# empty -> so chnm can be used as y=axis

# Any cases where different spid have different hit calls?
check.chem <- plotdat[, .(length(unique(mea_activity))), by = .(dsstox_substance_id, type)][V1 > 1, dsstox_substance_id]
plotdat[dsstox_substance_id %in% check.chem]
# Let's use the "hit,negative" notation that will merge with the mea consensus info

plotdat2 <- plotdat[, .(mea_activity_mean = mean(mea_activity, na.rm = T)),by = .(dsstox_substance_id, casn, plot_chnm, control_type, type)]
plotdat2[mea_activity_mean == 0, mea_activity_text := 'neg']
plotdat2[mea_activity_mean == 1, mea_activity_text := 'hit']
plotdat2[mea_activity_mean > 0 & mea_activity_mean < 1, mea_activity_text := 'hit,neg']
plotdat2$mea_activity_text <- factor(plotdat2$mea_activity_text, levels = sort(unique(plotdat2$mea_activity_text)), ordered = T)

# Merge in activity from mea consensus
# First, get the identifiers as the appear in plotdat
concordance.dat.overlap <- merge(concordance.dat, plotdat2[, unique(.SD), .SDcols = c('dsstox_substance_id','casn','plot_chnm')],
                                 all = F, by.x = 'DSSTox_Substance_Id', by.y = 'dsstox_substance_id')
concordance.dat.overlap[,type:='MEA_Ref_Consensus']
concordance.dat.overlap[, mea_activity_text := tolower(MEA_Ref_Consensus)]
plotdat3 <- rbind(plotdat2,
                  concordance.dat.overlap,
                  fill = T)

# Order for substances
plotdat3$mea_activity_text <- factor(plotdat3$mea_activity_text, levels = c('hit','hit,neg','neg','not tested'), ordered = T)
tb.wide <- dcast(plotdat3, plot_chnm ~ type, value.var = 'mea_activity_text')
tb.wide[, c(names(tb.wide)[!grepl('plot_chnm',names(tb.wide))]) := lapply(.SD, function(coli) {
  coli[is.na(coli)] <- 'not tested'
  coli
}), .SDcols = c(names(tb.wide)[!grepl('plot_chnm',names(tb.wide))])]
chem.levels <- tb.wide[order(sc, mc, MEA_Ref_Consensus), plot_chnm]
plotdat3$plot_chnm <- factor(plotdat3$plot_chnm, levels = chem.levels, ordered = T)

# Clean up x-axis labels
plotdat3[type == 'sc', plot_type := 'single-conc\n(MFR up or dn)']
plotdat3[type == 'mc', plot_type := 'multi-conc\n(3+ selective MEA hits)']
plotdat3[type == 'MEA_Ref_Consensus', plot_type := 'MEA reference\nlit. consensus']
# plotdat3$type <- factor(plotdat3$type, levels =c('sc','mc','MEA_Ref_Consensus'), ordered = T)
plotdat3$plot_type <- factor(plotdat3$plot_type, levels =c('single-conc\n(MFR up or dn)','multi-conc\n(3+ selective MEA hits)','MEA reference\nlit. consensus'), ordered = T)


#+ fig.height = 11

# Plot it
ggplot(plotdat3, aes(x = plot_type, y = plot_chnm))+
  geom_tile(aes(fill = mea_activity_text), col = 'white')+
  scale_x_discrete(position = 'top')+
  ylab('')+
  xlab('')+
  scale_y_discrete(position = 'right')+
  theme_bw()+
  facet_grid(rows = vars(control_type),
             scale = 'free_y',
             space = 'free_y',
             switch = 'y')+
  theme(strip.text.y = element_text(angle = 0),
        strip.text = element_text(angle = 0),
        legend.position = 'top')+
  ggtitle('MEA Acute MEA activity of assay controls in TCPL\nin sc (MFR up/dn only), mc (3+/15 selective hits, up or dn),\nand previous MEA studies')

## Details:
##
## * For the multi-conc, substances were considered a "hit" if they were active in 3 or more endpoints with an AC50 below the AC50 in the Alamar Blue assay. If the substance was not active or not tested in the Alamar Blue assay, then all active endpoints were assumed to be selective.<br>

## <br>Summary of results:
summary.tb <- activity.tb[!is.na(control_type), .(num_chem_screened = length(unique(dsstox_substance_id)),
                                                  num_chem_detected_any_spid = length(unique(dsstox_substance_id[mea_activity == 1]))),
                          by = .(type, control_type)]
summary.tb[, percent_detected := signif(num_chem_detected_any_spid/num_chem_screened,3)*100]
summary.tb[order(type, control_type)]

## Pooled across mc and sc
summary.tb <- activity.tb[!is.na(control_type), .(num_chem_screened = length(unique(dsstox_substance_id)),
                                                  num_chem_detected_any_spid = length(unique(dsstox_substance_id[mea_activity == 1]))),
                          by = .(control_type)]
summary.tb[, percent_detected := signif(num_chem_detected_any_spid/num_chem_screened,3)*100]
summary.tb


## So what's the difference between teh selective and non-selective results?
mc5_mc6[, mea_hit_count := sum(hitc[!grepl('(LDH)|(AB)',aenm)] %in% 1), by = .(dsstox_substance_id, spid)]
mc5_mc6[, mea_sel_hit_count := sum(sel_hitc[!grepl('(LDH)|(AB)',aenm)] %in% 1), by = .(dsstox_substance_id, spid)]
mc5_mc6[mea_hit_count >= 3 & mea_sel_hit_count < 3, .N, by = .(dsstox_substance_id, chnm, spid, control_type)]
## Okay, so 4 substances that would be considered "detected" with just a 3-hit count threshold are not detected if we require 3 selective hits. And 1 of those 4 (Chlorpromazine hydrochloride) is an assay positive controls.
##


## # See activity in all assays as a heatmap ----

# What's the overall activity level?

mc5_mc6[, type := 'mc']
sc2[, type := 'sc']
activity.tb <- rbind(mc5_mc6,
                     sc2, fill = T)

plotdat <- activity.tb[!is.na(control_type)]
plotdat[, plot_aenm := paste0(type,'-',sub('CCTE_Shafer_MEA_[acute]*','',aenm))]

# define yaxis
plotdat[, chnm_stnd := paste0(sort(unique(chnm[!is.na(chnm)])),collapse = ","), by = .(casn)]
plotdat[, plot_chnm := ifelse(nchar(chnm_stnd) <= 27, chnm_stnd, paste0(stri_sub(chnm_stnd, 1, 27),'...'))]
plotdat[, .(length(unique(dsstox_substance_id))), by = .(plot_chnm)][V1 > 1]
# empty -> so chnm can be used as y=axis

# Cluster aenms
plotdat.wide <- dcast(plotdat, plot_chnm ~ plot_aenm, value.var = 'hitc', fill = -1)

ggplot(plotdat, aes(x = plot_aenm, y = plot_chnm))+
  geom_tile(aes(fill = as.factor(hitc)))+
  scale_fill_manual(values = c('0' = 'gray80',
                               '1' = 'blue'),
                    name = 'hitc')+
  theme_bw()+
  facet_grid(rows = vars(control_type),
             scale = 'free_y',
             space = 'free_y',
             switch = 'y')+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        strip.text.y = element_text(angle = 0),
        strip.text = element_text(angle = 0),
        legend.position = 'top')+
  ggtitle('MEA Acute MEA activity of assay controls in TCPL sc and mc')


## I would love to perfect this, but, the real question is - could we get better sensitivity and/or overall accuracy if we changed the cutoffs for the single-conc?
##

## # Distribution of max_med's for single-conc ----


# mfr up and dn are just negative opposites, right?
sc2[grepl('MFR',aenm), .(length(unique(abs(max_med)))), by = .(spid)][V1 > 1]
# empty, coool, yes

sc2.mfr <- sc2[grepl("MFR_up",aenm)]
sc2.mfr <- sc2[, .(max_med = max_med[grepl("MFR_up",aenm)],
                   coff_dn = unique(coff[grepl('MFR_dn',aenm)])*-1,
                   coff_up = unique(coff[grepl('MFR_up',aenm)]),
                   hitc_dn = hitc[grepl('MFR_dn',aenm)],
                   hitc_up = hitc[grepl('MFR_up',aenm)]),
               by = .(spid, dsstox_substance_id, chnm, casn, control_type)]
sc2.mfr[is.na(control_type), control_type := 'other tested substances']
sc2.mfr$control_type <- factor(sc2.mfr$control_type, levels = c('positive','negative','other tested substances'), ordered = T)
sc2.mfr[, .N, by = .(hitc_dn, hitc_up)]
# cool, none over overlap
sc2.mfr[, hitc_desc := '']
sc2.mfr[hitc_dn %in% 1, hitc_desc := 'dn hit']
sc2.mfr[hitc_up %in% 1, hitc_desc := paste0(hitc_desc,'up hit')]
sc2.mfr[hitc_desc == '', hitc_desc := 'no hit']

ggplot(sc2.mfr, aes(x = max_med))+
  geom_histogram(aes(fill = as.factor(hitc_desc)))+
  geom_vline(aes(xintercept = coff_dn), lty = 'dashed')+
  geom_vline(aes(xintercept = coff_up), lty = 'dashed')+
  scale_fill_discrete(name = 'hit_desc')+
  facet_wrap(vars(control_type), ncol = 1,
             scales = 'free_y')+
  ggtitle('MEA Acute sc max_med distributions,\ngated by down cutoff (left dashed line) and upper cutoff (right dashed line)')

## Takeaway: Looks like we could shift the coff_dn to to the left a bit and pick up quite a few positives, at the cost of only a few negatives. I don't see any reason to shift the coff_up though.

## The theoretical goal: Optimize coff_dn adn coff_up such that the balanced accuracy is optimized
##

## # Can we increase the sc balanced accuracy by changing coff_dn? ----
current.coff.dn <- sc2[grepl('MFR_dn',aenm), unique(coff)]
controls.test <- sc2[!is.na(control_type) & grepl('MFR',aenm)]

# Try multiple coff's
coff.test.tb <- data.table()
for (coff_dni in c(seq(from = current.coff.dn - 2*current.coff.dn, to = current.coff.dn + 2*current.coff.dn, by = 5), current.coff.dn)) {
  controls.test[grepl('MFR_dn',aenm), coff := coff_dni]
  controls.test[grepl('MFR_dn',aenm), hitc := as.numeric(max_med >= coff)]
  add.tb <- controls.test[, .(coff_dn = coff_dni,
                              num_pos_detected = length(unique(dsstox_substance_id[hitc %in% 1 & control_type == 'positive'])),
                              num_neg_detected = length(unique(dsstox_substance_id[hitc %in% 1 & control_type == 'negative'])))]
  coff.test.tb <- rbind(coff.test.tb, add.tb)
}

num.positives <- controls.test[control_type == 'positive',length(unique(dsstox_substance_id))]
num.negatives <- controls.test[control_type == 'negative',length(unique(dsstox_substance_id))]
coff.test.tb[, per_pos_detected := num_pos_detected/num.positives]
coff.test.tb[, per_neg_detected := num_neg_detected/num.negatives]
coff.test.tb[, specificity := 1-per_neg_detected]
coff.test.tb[, bal_acc := (per_pos_detected + specificity)*0.5]
coff.test.tb[, bal_acc_rank := order(bal_acc)]

coff.test.tb[, is_current_coff := as.numeric(coff_dn %in% current.coff.dn)]
coff.test.tb[, text:=paste0('coff: ',signif(-1*coff_dn,3))]

coff.test.tb <- coff.test.tb[order(bal_acc)]

p <- ggplot(coff.test.tb, aes(y = per_pos_detected, x = per_neg_detected, text = text))+
  geom_point(aes(size = as.factor(is_current_coff)))+
  geom_line(aes(group = 1))+
  scale_size_manual(values = c('1' = 3,
                               '0' = 1),
                    labels = c('1' = 'is current bmad',
                               '0' = 'not current bmad')                               ,
                    name = 'is current cutoff')+
  ylab('True Positive Rate')+
  xlab('False Positive Rate')+
  ggtitle('MEA Acute sc ROC with variable MFR dn cutoff')
plotly::ggplotly(p, tooltip = 'text')


## Where is the balanced accuracy optimized?
coff.test.tb.long <- melt(coff.test.tb, id.cols = c('coff_dn'), measure.vars = c('bal_acc','specificity','per_pos_detected'))
coff.test.tb.long[variable == 'per_pos_detected', variable := 'sensitivity']
max.bal.acc <- coff.test.tb[, max(bal_acc)]
ggplot(coff.test.tb.long, aes(x = -1*coff_dn, y = value))+
  geom_line(aes(color = variable), lwd = 2)+
  geom_point(aes(size = as.factor(is_current_coff)))+
  scale_size_manual(values = c('1' = 3,
                               '0' = 1),
                    labels = c('1' = 'is current bmad',
                               '0' = 'not current bmad')                               ,
                    name = '')+
  geom_hline(yintercept = max.bal.acc, lty = 'dashed')+
  annotate(geom = 'text', x = -150, y = max.bal.acc, label = paste0('Max bal. acc: ',signif(max.bal.acc,3)), vjust = 0)+
  xlab('MFR dn cutoff')+
  ggtitle('MEA Acute sc performance versus MFR dn cutoff')

## Note: we never get to 0% senstivity because there are always a few positives detected in the "up" direction

## View results, ordered by balanced accuracy:
# What is the maximum margin increase in balanced accuracy? (percentage, and added number of hits, how many negative hits?)
coff.test.tb[order(-bal_acc), .(is_current_coff, coff_dn, bal_acc = paste0(signif(bal_acc*100,3),'%'), 
                                sensivity_frac = paste0(num_pos_detected,'/',num.positives),
                                sensitivity = paste0(signif(per_pos_detected*100,3),'%'),
                                frac_neg_detected = paste0(num_neg_detected,'/',num.negatives),
                                specificity = paste0(signif(specificity*100,3),'%'))][1:12]

## So if we change the cutoff to 30, we could increase the number of positives detected from 41/71 (57.7%) to 52/71 (73.2%), at the cost of detected 2 negatives.<br>
## An increase of 11 hits for the positives seems worth it...seems potentially worth it... if they aren't already detected in the mc.
##

## # Visualize putative new cutoffs ---- 

proposed.new.coff <- coff.test.tb[bal_acc == max(bal_acc), max(coff_dn)]

ggplot(sc2.mfr, aes(x = max_med))+
  geom_histogram(aes(fill = as.factor(hitc_desc)))+
  geom_vline(aes(xintercept = coff_dn), lty = 'dashed')+
  geom_vline(aes(xintercept = coff_up), lty = 'dashed')+
  geom_vline(xintercept = -1*proposed.new.coff, lty = 'dashed', lwd = 2)+
  scale_fill_discrete(name = 'hit_desc')+
  facet_wrap(vars(control_type), ncol = 1,
             scales = 'free_y')+
  ggtitle('MEA Acute sc max_med distributions,\ngated by down cutoff (left dashed line) and upper cutoff (right dashed line)\nproposed new cutoff to optimize balanced accuracy: thick dashed line')


## # How many of the 11 additional detected positives are active in the mc? ----


sc2.test <- sc2[grepl('MFR',aenm)]
sc2.test[grepl('MFR_dn',aenm), coff := proposed.new.coff]
sc2.test[grepl('MFR_dn',aenm), hitc := as.numeric(max_med >= coff)]
# sc2[hitc %in% -1] # empty , cool

activity.tb <- rbind(mc5_mc6[!grepl('(LDH)|(AB)',aenm), 
                             .(type = 'mc',
                               mea_activity = ifelse(sum(hitc %in% 1) >= 3, 1, 0)), by = .(spid, dsstox_substance_id, casn, chnm, control_type)],
                     sc2[grepl('MFR',aenm), 
                         .(type = 'sc',
                           mea_activity = ifelse(any(hitc %in% 1), 1, 0)), by = .(spid, dsstox_substance_id, casn, chnm, control_type)],
                     sc2.test[grepl('MFR',aenm), 
                         .(type = 'sc_alt_coff',
                           mea_activity = ifelse(any(hitc %in% 1), 1, 0)), by = .(spid, dsstox_substance_id, casn, chnm, control_type)])
# rm(list = setdiff(ls(),c('mc5_mc6','sc2','activity.tb','concordance.dat')))

# Prepare data to plot
# activity.tb[, casn_tested_in_tcpl := as.numeric(any(type %in% c('sc','mc'))), by = .(casn)]
plotdat <- activity.tb[!is.na(control_type)]
plotdat$type <- factor(plotdat$type, levels =c('sc','mc','sc_alt_coff'), ordered = T)

# define yaxis
plotdat[, chnm_stnd := paste0(sort(unique(chnm[!is.na(chnm)])),collapse = ","), by = .(casn)]
plotdat[, plot_chnm := ifelse(nchar(chnm_stnd) <= 27, chnm_stnd, paste0(stri_sub(chnm_stnd, 1, 27),'...'))]
plotdat[, .(length(unique(dsstox_substance_id))), by = .(plot_chnm)][V1 > 1]
# empty -> so chnm can be used as y=axis

# Any cases where different spid have different hit calls?
check.chem <- plotdat[, .(length(unique(mea_activity))), by = .(dsstox_substance_id, type)][V1 > 1, dsstox_substance_id]
plotdat[dsstox_substance_id %in% check.chem]
# Let's use the "hit,negative" notation that will merge with the mea consensus info

plotdat2 <- plotdat[, .(mea_activity_mean = mean(mea_activity, na.rm = T)),by = .(dsstox_substance_id, casn, plot_chnm, control_type, type)]
plotdat2[mea_activity_mean == 0, mea_activity_text := 'neg']
plotdat2[mea_activity_mean == 1, mea_activity_text := 'hit']
plotdat2[mea_activity_mean > 0 & mea_activity_mean < 1, mea_activity_text := 'hit,neg']
plotdat2$mea_activity_text <- factor(plotdat2$mea_activity_text, levels = sort(unique(plotdat2$mea_activity_text)), ordered = T)

# Merge in activity from mea consensus
# First, get the identifiers as the appear in plotdat
# concordance.dat.overlap <- merge(concordance.dat, plotdat2[, unique(.SD), .SDcols = c('dsstox_substance_id','casn','plot_chnm')],
#                                  all = F, by.x = 'DSSTox_Substance_Id', by.y = 'dsstox_substance_id')
# concordance.dat.overlap[,type:='MEA_Ref_Consensus']
# concordance.dat.overlap[, mea_activity_text := tolower(MEA_Ref_Consensus)]
# plotdat3 <- rbind(plotdat2,
#                   concordance.dat.overlap,
#                   fill = T)
plotdat3 <- plotdat2

# Order for substances
plotdat3$mea_activity_text <- factor(plotdat3$mea_activity_text, levels = c('hit','hit,neg','neg','not tested'), ordered = T)
tb.wide <- dcast(plotdat3, plot_chnm ~ type, value.var = 'mea_activity_text')
tb.wide[, c(names(tb.wide)[!grepl('plot_chnm',names(tb.wide))]) := lapply(.SD, function(coli) {
  coli[is.na(coli)] <- 'not tested'
  coli
}), .SDcols = c(names(tb.wide)[!grepl('plot_chnm',names(tb.wide))])]
chem.levels <- tb.wide[order(sc, mc, sc_alt_coff), plot_chnm]
plotdat3$plot_chnm <- factor(plotdat3$plot_chnm, levels = chem.levels, ordered = T)

# Clean up x-axis labels
plotdat3[type == 'sc', plot_type := 'single-conc\n(MFR up or dn)']
plotdat3[type == 'mc', plot_type := 'multi-conc\n(3 of more any MEA hits)']
# plotdat3[type == 'MEA_Ref_Consensus', plot_type := 'MEA reference\nlit. consensus']
plotdat3[type == 'sc_alt_coff', plot_type := 'single-conc\n(MFR up or dn)\nnew dn cutoff']

# plotdat3$type <- factor(plotdat3$type, levels =c('sc','mc','MEA_Ref_Consensus'), ordered = T)
plotdat3$plot_type <- factor(plotdat3$plot_type, levels =c('single-conc\n(MFR up or dn)','multi-conc\n(3 of more any MEA hits)','single-conc\n(MFR up or dn)\nnew dn cutoff'), ordered = T)

#+ fig.height = 10

# Plot it
ggplot(plotdat3, aes(x = plot_type, y = plot_chnm))+
  geom_tile(aes(fill = mea_activity_text), col = 'white')+
  scale_x_discrete(position = 'top')+
  ylab('')+
  xlab('')+
  scale_y_discrete(position = 'right')+
  theme_bw()+
  facet_grid(rows = vars(control_type),
             scale = 'free_y',
             space = 'free_y',
             switch = 'y')+
  theme(strip.text.y = element_text(angle = 0),
        strip.text = element_text(angle = 0),
        legend.position = 'top')+
  ggtitle(paste0('MEA Acute MEA activity of assay controls in TCPL\nin sc (MFR up/dn only) with current cutoff (',sc2[grepl('MFR_dn',aenm), signif(unique(coff),3)],' mc (3+/15 MEA acnms, up or dn),\nor sc (MFR up/dn only) with new dn cutoff (',signif(proposed.new.coff,3),')'))


## <br>Summary of results:
summary.tb <- activity.tb[!is.na(control_type), .(num_chem_screened = length(unique(dsstox_substance_id)),
                                                  num_chem_detected_any_spid = length(unique(dsstox_substance_id[mea_activity == 1]))),
                          by = .(type, control_type)]
summary.tb[, percent_detected := signif(num_chem_detected_any_spid/num_chem_screened,3)*100]
summary.tb[order(type, control_type)]

## Pooled across mc and sc
summary.tb <- activity.tb[!is.na(control_type), .(num_chem_screened = length(unique(dsstox_substance_id)),
                                                  num_chem_detected_any_spid = length(unique(dsstox_substance_id[mea_activity == 1]))),
                          by = .(control_type)]
summary.tb[, percent_detected := signif(num_chem_detected_any_spid/num_chem_screened,3)*100]
summary.tb

activity.tb[control_type == 'positive', .N, by = .(mea_activity, type)]

## Let'scheck out what these chemicals are that are detected in the "new method", but not the original MC
## do we care about these a lot?
## is 5ish more chem (after inclusion of hte mc) worth it?


res.tb <- activity.tb[, .(detected_in_sc_org = ifelse(any(type %in% 'sc'), ifelse(any(type %in% 'sc' & mea_activity %in% 1), '1','0'), 'not tested'),
                detected_in_mc = ifelse(any(type %in% 'mc'),
                                                   ifelse(any(type %in% 'mc' & mea_activity %in% 1), '1', '0'), 
                                                   'not tested'),
                detected_in_sc_proposed = ifelse(any(type %in% 'sc_alt_coff'),
                                                            ifelse(any(type %in% 'sc_alt_coff' & mea_activity %in% 1), '1', '0'), 
                                                            'not tested')),
            by = .(dsstox_substance_id, chnm, control_type )]

## Positive controls not detected in sc_original:
res.tb[detected_in_sc_org == 0 & detected_in_sc_proposed == 1 & control_type %in% c('positive'), .(chnm, detected_in_sc_org, detected_in_mc, detected_in_sc_proposed)][order(-detected_in_mc, -detected_in_sc_proposed)]
## So we would gain 5 additional positive hits if we changed the sc cutoff. Several of these just weren't tested in the MC endpoints<br>
## (Chlorpyrifos, Genistein, 17beta-Estradiol, 5,5-Diphenyl, and 4-Bromophenyl 1,4-diazabicyclo(3.2.2)nonane-4-carboxylate )
##


## Negative controls not only detected in mc and/or proposed sc
res.tb[control_type %in% 'negative' & (detected_in_mc %in% 1 | detected_in_sc_proposed %in% 1), .(chnm, detected_in_sc_org, detected_in_mc, detected_in_sc_proposed)][order(-detected_in_mc, -detected_in_sc_proposed)]


## # Other tested substances that would be added as hits? ----
sc2[is.na(control_type), control_type := 'other tested substances']
sc2.test[is.na(control_type), control_type := 'other tested substances']
sc2[, plot_chnm := ifelse(nchar(chnm) <= 27, chnm, paste0(stri_sub(chnm, 1, 27),'...'))]
sc2.test[, plot_chnm := ifelse(nchar(chnm) <= 27, chnm, paste0(stri_sub(chnm, 1, 27),'...'))]

## Current hit counts by substance type:
sc2[grepl('MFR',aenm), .(num_chem_hit = length(unique(dsstox_substance_id[hitc %in% 1])),
                         total_chem = length(unique(dsstox_substance_id))), by = .(control_type)]

## Alternative hit counts by substance type with adjusted dn cutoff:
sc2.test[grepl('MFR',aenm), .(num_chem_hit = length(unique(dsstox_substance_id[hitc %in% 1])),
                              total_chem = length(unique(dsstox_substance_id))), by = .(control_type)]
## Woah, quite a few more other substances would be a hit!<br>
## What are they?

other.detected.in.sc2.test <- sc2.test[!control_type %in% c('positive','negative') & grepl('MFR',aenm) &hitc == 1, unique(dsstox_substance_id)]
other.detected.in.sc2 <- sc2[!control_type %in% c('positive','negative') & grepl('MFR',aenm) &hitc == 1, unique(dsstox_substance_id)]

sc2[dsstox_substance_id %in% setdiff(other.detected.in.sc2.test, other.detected.in.sc2) & grepl('MFR_dn',aenm), .(plot_chnm, max_med, coff_cur=coff)][order(max_med)]

## How many of these were rested in the mc?
length(setdiff(other.detected.in.sc2.test, other.detected.in.sc2)) # 139
length(intersect(setdiff(other.detected.in.sc2.test, other.detected.in.sc2), mc5_mc6$dsstox_substance_id)) # 23

# How many of the regular sc tested chem were rescreened in the mc?
length(other.detected.in.sc2) # 285
length(intersect(other.detected.in.sc2, mc5_mc6$dsstox_substance_id)) # 186
##

## # Current Question ----
## Do we want to lower the cutoff for the MEA acute sc MFR dn endpoint so that a few more positive controls would be detected? 
##
##  * This would increase the pooled sc/mc sensitivity from 75% to 82% (5 additional substances detected), at the cost of 2 additional negatives labelled as "hits".
##  * The balanced accuracy would increase from 79% to 84%
##  * The MFR dn cutoff would change from 3bmad = 64.89 to 30-40 (probably 1.5 or 2 bmad)
##  * The number of other tested substances hit would increase from 285/952 to 424/952. Of the 139 additional hits, 23 have been rescreened in the multi-conc (but if we really think they should be positive, maybe it doesn't matter?)
##

## # Conclusion July 20, 2022 ----
## After discussion with Tim July 20, 2022 - we determined that a 75% sensitivity with the pooled sc and mc data is sufficient. Tim reminded that the cutoff is based on the activity in the DMSO wells. If we lower the dn MFR cutoff, we might get in to the range where theoretical DMSO replicate samples might be considered a hit. So we are not going to change the cutoffs, and we are ready to release the MEA Acute MFR single-conc endpoints!

