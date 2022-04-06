
*Codi per generar l'estimació de vot al Congrés dels Diputats 
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
replace pondrvotcong=	1.796491228	if REC_CONGRES_VOT_CENS_R==	1
replace pondrvotcong=	0.775510204	if REC_CONGRES_VOT_CENS_R==	3
replace pondrvotcong=	0.776193088	if REC_CONGRES_VOT_CENS_R==	4
replace pondrvotcong=	1.788018433	if REC_CONGRES_VOT_CENS_R==	6
replace pondrvotcong=	1.447368421	if REC_CONGRES_VOT_CENS_R==	10
replace pondrvotcong=	0.97995992	if REC_CONGRES_VOT_CENS_R==	18
replace pondrvotcong=	1.22756827	if REC_CONGRES_VOT_CENS_R==	21
replace pondrvotcong=	2.8	if REC_CONGRES_VOT_CENS_R==	23
replace pondrvotcong=	1.286713287	if REC_CONGRES_VOT_CENS_R==	80
replace pondrvotcong=	0.954214676	if REC_CONGRES_VOT_CENS_R==	96


*Perparem variable d'intenció de vot, unim BAN
gen INT_PARLAMENT_VOT_R_old=INT_PARLAMENT_VOT_R
replace INT_PARLAMENT_VOT_R=96 if INT_PARLAMENT_VOT_R==93
replace INT_PARLAMENT_VOT_R=96 if INT_PARLAMENT_VOT==94
fre INT_PARLAMENT_VOT_R
fre INT_PARLAMENT_VOT_R [aw=pondrvotcong]

*Assignem indecisos de manera determinista per simpatia de partit
gen int_simp=INT_PARLAMENT_VOT_R
replace int_simp=SIMPATIA_PARTIT_R  if INT_PARLAMENT_VOT_R>=98 & SIMPATIA_PARTIT_R!=95 
label values int_simp labels216
fre int_simp
fre int_simp [aw=pondrvotcong]

*Assignem indecisos restants per model mlogit

*Preparem les variables
recode INT_CONGRES_VOT_R  (93/99=93 "BAI"), gen(intencio_cong_rec)
recode CONEIX_A_FERNANDEZ- VAL_I_GARRIGA (98=.a) (99=.b)
recode IDEOL_0_10 CAT_0_10 ESP_0_10 (98=.a) (99=.b)
recode intencio_cong_rec (80=93)
label values intencio_cong_rec labels216
recode int_simp_cong  (93/99=93 "BAI"), gen(int_simp_cong_rec)

*Model líders i policies sobre int_simp
mlogit int_simp_cong_rec  VAL_A_FERNANDEZ VAL_C_PUIGDEMONT VAL_O_JUNQUERAS VAL_M_ICETA  VAL_J_ALBIACH VAL_C_CARRIZOSA VAL_E_REGUANT VAL_I_GARRIGA IDEOL_0_10 CAT_0_10 ESP_0_10  VAL_GOV_ESP 
*El model no pot estimar probabilitat per Cs per falta de casos, es preserven els d'intenció+simpatia d'aquest partit
predict m4p1-m4p9, pr
*Càlcul opció triada
gen maxvot_m4=.
egen maxprob_m4=rowmax(m4p1-m4p9)
foreach x of numlist 1/9 {
	replace maxvot_m4=`x' if maxprob_m4==m4p`x'
	}

*Predicció  amb aquest model
gen m4ivot_cong=int_simp_cong_rec
recode maxvot_m4 1=1 2=3 3=4 4=10 5=21 6=22 7=23 8=80 9=93 
replace m4ivot_cong=maxvot_m4 if m4ivot==93 & maxprob_m4>=0.6
label values m4ivot_cong labels216

*Calculem les proporcions i els IC i els enviem a un excel 
proportion m4ivot [pw=pondrvot]
putexcel set m4tot, replace
putexcel A1 = matrix(r(table)),  names
proportion m4ivot [pw=pondrvot] if m4ivot<93
putexcel set m4partits, replace
putexcel A1 = matrix(r(table)),  names


