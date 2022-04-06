
*Codi per generar l'estimació de vot al Parlament de Catalunya 
*del Baròmetre 1-2022 del Centre d'Estudis d'Opinió


/*Aquest codi realitza les següents operacions:

1. Ponderació per record de vot
2. Assignació d'indecisos de manera determinista per simpatia
3. Assignació probabilística d'indecisos restants a partir de model mlogit amb valoració de líders,
	ideologia, posicionament nacional, valoració de governs i cluster de residència
4. Càlcul matriu de transferències sobre vot estimat.
*/
import spss "Microdades anonimitzades_1019.sav", clear

*Preparem record de vot per ponderació

fre REC_PARLAMENT_VOT_CENS_R
*Unim nul, blanc i abst (BAN)
gen REC_PARLAMENT_VOT_CENS_R_old=REC_PARLAMENT_VOT_CENS_R
recode REC_PARLAMENT_VOT_CENS_R (93/94=96)
fre REC_PARLAMENT_VOT_CENS_R

*Calculem la variable de ponderació (vot real 14F2021/record de vot declarat, excloent els NS/NC)

gen pondrvot=1 /*Baseline ponderació neutra*/
replace pondrvot=	1.304222092	if REC_PARLAMENT_VOT_CENS_R==	1
replace pondrvot=	0.471791685	if REC_PARLAMENT_VOT_CENS_R==	3
replace pondrvot=	0.953111487	if REC_PARLAMENT_VOT_CENS_R==	4
replace pondrvot=	1.754926015	if REC_PARLAMENT_VOT_CENS_R==	6
replace pondrvot=	0.668643161	if REC_PARLAMENT_VOT_CENS_R==	10
replace pondrvot=	1.141610892	if REC_PARLAMENT_VOT_CENS_R==	21
replace pondrvot=	0.587573642	if REC_PARLAMENT_VOT_CENS_R==	22
replace pondrvot=	2.047768089	if REC_PARLAMENT_VOT_CENS_R==	23
replace pondrvot=	2.416383432	if REC_PARLAMENT_VOT_CENS_R==	80
replace pondrvot=	1.303544849	if REC_PARLAMENT_VOT_CENS_R==	96

*Perparem variable d'intenció de vot, unim BAN
gen INT_PARLAMENT_VOT_R_old=INT_PARLAMENT_VOT_R
replace INT_PARLAMENT_VOT_R=96 if INT_PARLAMENT_VOT_R==93
replace INT_PARLAMENT_VOT_R=96 if INT_PARLAMENT_VOT==94
fre INT_PARLAMENT_VOT_R
fre INT_PARLAMENT_VOT_R [aw=pondrvot]

*Assignem indecisos de manera determinista per simpatia de partit
gen int_simp=INT_PARLAMENT_VOT_R
replace int_simp=SIMPATIA_PARTIT_R  if INT_PARLAMENT_VOT_R>=98 & SIMPATIA_PARTIT_R!=95 
label values int_simp labels216
fre int_simp
fre int_simp [aw=pondrvot]

*Assignem indecisos restants per model mlogit

*Preparem les variables
recode INT_PARLAMENT_VOT_R  (93/99=93 "BAI"), gen(intencio_rec)
recode CONEIX_A_FERNANDEZ- VAL_I_GARRIGA (98=.a) (99=.b)
recode IDEOL_0_10 CAT_0_10 ESP_0_10 (98=.a) (99=.b)
recode intenc (80=93)
label values intencio_rec labels216
recode int_simp  (93/99=93 "BAI"), gen(int_simp_rec)
recode int_simp  (98/99=93 "NSNC"), gen(int_simp_rec2)

*Model líders i policies sobre int_simp
mlogit int_simp_rec  VAL_A_FERNANDEZ VAL_C_PUIGDEMONT VAL_O_JUNQUERAS VAL_M_ICETA VAL_S_ILLA VAL_J_ALBIACH VAL_C_CARRIZOSA VAL_E_REGUANT VAL_I_GARRIGA IDEOL_0_10 CAT_0_10 ESP_0_10 VAL_GOV_CAT VAL_GOV_ESP i.CLUSTER

*Càlcul probabilitats predites de vot a cada opció per cada enquestat
 predict m4p1-m4p10, pr
*Càlcul opció triada (màxima probabilitat)
gen maxvot_m4=.
egen maxprob_m4=rowmax(m4p1-m4p10)
foreach x of numlist 1/10 {
	replace maxvot_m4=`x' if maxprob_m4==m4p`x'
	}
	
*Predicció  amb aquest model: assignem partit amb màxima probabilitat si no tenen 
*intenció assignada prèviament i si el model prediu una p>0.6
 
gen m4ivot=int_simp_rec
recode maxvot_m4 1=1 2=3 3=4 4=6 5=10 6=21 7=22 8=23 9=80 10=93 
replace m4ivot=maxvot_m4 if m4ivot==93 & maxprob_m4>=0.6
label values m4ivot labels216

*Calculem les proporcions i els IC i els enviem a un excel 
proportion m4ivot [pw=pondrvot]
putexcel set m4tot, replace
putexcel A1 = matrix(r(table)),  names
proportion m4ivot [pw=pondrvot] if m4ivot<93
putexcel set m4partits, replace
putexcel A1 = matrix(r(table)),  names

*Matriu transferències
recode REC_PARLAMENT_VOT_CENS_R 93/99=93, gen(REC_PARLAMENT_VOT_CENS_R_trsf)
label values REC_PARLAMENT_VOT_CENS_R_trsf labels216
tab REC_PARLAMENT_VOT_CENS_R_trsf m4ivot [aw=pondrvot], row nofreq

