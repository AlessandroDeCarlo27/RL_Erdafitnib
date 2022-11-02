$PROBLEM    ERDAFITINIB PKPD - initial SIMULATION

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
$INPUT      C ID TIME DAY II ADDL AMT EVID MDV CMT DV SEX IMPREN2 IMPREN3 WT1 WT2 AGP
            CL V2 V3 Q3 V4 Q4 KA ALAG KD BSEXCL BIMPREN2CL BIMPREN3CL BWT1V2 BWT2V2 BSEXV2 POWERAGP
            PO40 PO4P SLOPEm GAMMA parKE0 parKIN parKBASE TLAG TSLD THETASEX

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
$DATA       DATASET2.csv IGNORE=C

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
$SUBROUTINE ADVAN6 TOL=6

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
$MODEL      NCOMP=6
;pk (3 comp + assorbimento)
            COMP(1,DEFDOSE) 	; administration compartment 
            COMP(2) 		; central compartment
            COMP(3) 		; peripheral compartment 1
            COMP(4) 		; peripheral compartment 2

;pd
            COMP(5) 		; effect compartment
            COMP(6) 		; amount of attenuation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
$PK

test = THETA(1)*ETA(1)

;PK PARAMETERS-----------------------------------------------------------------------------------
fu=KD/(KD+(AGP*10632))
;fu=0.00356
V2tot=V2+AGP**POWERAGP+BWT1V2*WT1+BWT2V2*WT2  
V2free=V2tot/fu
CLfree=CL+BSEXCL*SEX+BIMPREN2CL*IMPREN2+BIMPREN3CL*IMPREN3
k32=Q3/V2free    	;inter-compartment clearance
k23=Q3/V3		;inter-compartment clearance
k42=Q4/V2free		;inter-compartment clearance
k24=Q4/V4 		;inter-compartment clearance
k02=CLfree/V2tot    	;elimination clearance

Vp1=V3     		;apparent volume of distribution of periph comp 1
Vp2=V4      		;apparent volume of distribution of periph comp 2
Kabs=KA     		;absorption rate
ALAG1=ALAG  		;lag time 

;PD PARAMETERS----------------------------------------------------------------------------------
ke0=parKE0
kin=parKIN
kbase=parKBASE

;Initial conditions
;pk
A_0(1)=0 ;administration
A_0(2)=0 ;central
A_0(3)=0 ;peripheral 1
A_0(4)=0 ;peripheral 2

;pd
A_0(5)=0 ;comp effect
A_0(6)=0 ;amount of attenuation

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
$DES

;pk ODEs
DADT(1)=-Kabs*A(1)							;administration compartment
DADT(2)=Kabs*A(1)-k02*fu*A(2)-k32*fu*A(2)+k23*A(3)-k42*fu*A(2)+k24*A(4)	;central compartment 
DADT(3)=k32*fu*A(2)-k23*A(3)						;peripheral compartment 1 
DADT(4)=k42*fu*A(2)-k24*A(4)						;peripheral compartment 2

Ctot=A(2)/V2tot ;drug plasmatic concentration
Cfree=fu*Ctot

;pd ODEs
DADT(5)=ke0*(Cfree-A(5))
DADT(6)=kin*(1-A(6))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
$ERROR
admPK  	 = A(1)   	;defdose comp output
centPK 	 = A(2)  	;central comp pk
peri1PK	 = A(3) 	;peripheral comp 1 pk
peri2PK	 = A(4)		;peripheral comp 2 pk
compeffPD= A(5)		;comp effect
attPD    = A(6)		;amount of attenuation

EPS1=EPS(1)

;IF(CMT.EQ.5) THEN
Y = EPS1
;ENDIF


IF(TSLD.GT.TLAG) THEN
EBSL=PO4P-(PO4P-PO40)*EXP(-kbase*(TSLD-TLAG))
ELSE
EBSL=PO40
ENDIF
EBSLfin=EBSL+THETASEX*SEX
M=SLOPEm*(1-A(6))
IF(A(5).GT.0) THEN
;E=EBSLfin+SLOPEm*(1-A(6))*(A(5)**GAMMA)
E=EBSLfin+M*(A(5)**GAMMA)
flag = 1
ELSE
E=EBSLfin
flag = -1
ENDIF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Initial estimates of the population parameters
$THETA ;typical values (BASE MODEL)

; test
1  

$OMEGA  ;inter-individual variability

; pd
0.2 ;test

$SIGMA
0.004

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;simstart
$SIMULATION (20902,NEW) NSUB=1 ONLYSIMULATION;
;simend

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
$TABLE      ID TIME DAY MDV CMT admPK centPK peri1PK peri2PK compeffPD attPD E flag Cfree Ctot NOAPPEND NOPRINT ONEHEADER FILE=sdtab1 ;simulazioni

