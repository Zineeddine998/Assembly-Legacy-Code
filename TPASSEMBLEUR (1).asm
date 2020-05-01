
; ----------------------------------- Segment de donnees----------------------------------------- 

sdonnee SEGMENT     
    
;-------------- Segment de donnee associe a l'affichage de l'introduction-------------------------------
  

;----------------------- Segment de donnee associe a l'affichage du menu ------------------------      
        
      

menu  db 10,13,10,13,"----------------------------------------------------------------" 
      db 10,13,10,13,"   PROGRAMME DE CONVERTION DE SYSTEM NUMERIQUE(ARABE/ROMAIN)                    "
            "
	     
	
	  db 10,13,10,13,"----------------------------------------------------------------"
	  
		 
		 
		 
		 
		    
  db 10,13,10,13," Veuillez introduire le nombre de l'operation que vous desirez :  " 
      db 10,13," "
      db 10,13,"  1/ CONVERSION ARABE VERS ROMAIN. " 
      db 10,13,"  2/ CONVERSION ROMAIN VERS ARABE. "
      db 10,13,"  3/ QUITTER LE PROGRAMME. " 
      db 10,13,'$'
      
      
choix:  db 10,13,"     Votre Choix :  $"   
Erreur_Menu db 10,13,"!!! ERREUR !!! Le choix doit etre soit 1,2 ou 3.",10,13,'$' 


      

                                                     
               
                
;------------------ Donnees pour conversion d'un nombre ARABE vers ROMAIN -------------------
   
entree  DB 8 DUP(?)  
decimal DW ? 
chromain1 DW "M","CM","D",'CD',"C","XC","L","XL","X","IX","V","IV","I " 
charabe1  DW  1000h,900h,500h,400h,100h,90h,50h,40h,10h,9h,5h,4h,1h
chaine DW 15 dup(?),'$'
 
;---------------------------------------- LES MESSAGES ----------------------------------------------
MESSAGE01  DB 13,10,'Veuillez entrer un nombre inferieur ou egal a 3999:',13,10,13,10,'  -->  $' 
MESSAGE02  DB 13,10,'L''equivalent de votre nombre en romain est(Patientez...) :  ','$'
MESSAGE03  DB 13,10,'Veuillez entrer un nombre en chiffres romains en majuscules:  ',13,10,13,10,'   -->  $' 
MESSAGE04  DB 13,10,'L''equivalent de votre nombre en chiffres arabes est(Patientez...) :  ','$'
ERREUR01 DB 13,10,'!!! ERREUR !!! le nombre entre doit etre inferieur ou egal a 3999, veuillez entrer un autre nombre:  ','$'
ERREUR02 DB 13,10,'!!! ERREUR !!! le nombre "0" n''a pas d''equivalents dans le systeme romain,veuillez entrer un autre nombre  :  ','$'
ERREUR03 DB 13,10,'!!! ERREUR !!! le nombre entre est invalide, veuillez entrer un autre nombre  :  ','$'
ERREUR04 DB  13,10,'!!! ASTUCE !!! pensez a ajouter des zeros au nombre introduit si il est compose de moins de 4 chiffres   :  ','$'
                                                            
                                                            
;------------------ Donnes pour counversion d'un nombre ROMAIN vers ARABE ----------------------
 
nombre dw 16 dup(?),'$'                 
chromain2 dw 'M','D','C','L','X','V','I','.' 
charabe2  dw  1000,500,100,50,10,5,1
precedent dw ? 
Val_Ltr dw ?
cpt dw ?
cpt2 dw ?
cpt3 dw ?                  
trouv_bool dw ? 
romain dw ?
superieur dw ?  
resultat db 4 dup(?),'$'


sdonnee ENDS 

;----------------------------------- Fin du segment de donnees------------------------------------


;---------------------------------------- Segment de code ---------------------------------------------


scode SEGMENT 
    
ASSUME DS:sdonnee,CS:scode


 



;------------------------------------ INTRODUCTION ----------------------------------------------- 

Introduction PROC
      


M_Menu:   MOV DX,offset menu
          MOV AH,9
          INT 21H
          
          
          
           

M_Choix:  MOV DX,offset choix
          MOV AH,9
          INT 21H        
          MOV AH,1
          INT 21H
          CMP AL,31h
          JE Arabe_Vers_Romain
          CMP AL,32h
          JE  Romain_Vers_Arabe
          CMP AL,33h
          JE QUITTER
          MOV DX,offset Erreur_Menu
          MOV AH,9
          INT 21H
          JMP M_Choix 

      RET 
      
Introduction ENDP 

;------------------------------------------------------------------------------------------------ 

;---------------------- Lecture d'un des chiffres du nombre charabe1 (sur un octet) -------------------------------------
                                      

CHIF  PROC
    
      MOV AH,1    
      INT 21H  
      
      
      CMP	 AL, 30h  ; comparer le chiffre introduit avec 0, s'il est inferieur alors il est invalide   
      JL INVALID
      CMP	 AL, 39h  ; comparer le chiffre introduit avec 9, s'il est superieur alors il est invalide
      JG	 INVALID
     
      SUB AL,30h
      MOV [SI],AL
      INC SI
      MOV [SI],0
      INC SI
          
      LOOP CHIF
      
      RET
       
CHIF  ENDP      

;--------------------------------------------------------------------------------------------------    
  
  
;------------------------------- Lecture du nombre entier ---------------------------------------  
      

LECT1 PROC 
      
      MOV DX,offset MESSAGE01
      MOV AH,9
      INT 21H
             
      
DEB:  LEA SI,entree
      
      MOV CX,4   ; le nombre charabe1 se compose de 4 chiffres au plus 
             
      CALL CHIF  ; lecture du chiffre
      
      ;On recupere dans BX le nombre sur 4 chiffres
            
      MOV AX,0
      LEA SI,entree
      ADD SI,6
      
      MOV BX,[SI] 
      
      OR AX,BX 
      SUB SI,2
      MOV BX,[SI]
      SHL BX,4
     
      OR AX,BX  
      
      SUB SI,2
      MOV BX,[SI]
      SHL BX,8
     
      OR AX,BX 
      
      SUB SI,2
      MOV BX,[SI]
      SAL BX,12
     
      OR AX,BX
      
      MOV BX,AX 
                   
      ;Comparaison du nombre introduit avec 3000
      
      CMP BX,3999h
      JA INVALID 
      
      ;comparaison du nombre introduit avec 0000
      
      CMP BX,0000h
      JE invalid0   
      
      Jmp CONV   ; faire la conversion Arabe --> Romain
       
      
INVALID:      
      MOV DX,offset ERREUR01
      MOV AH,9
      INT 21H   
      
      MOV DX,offset ERREUR04
      MOV AH,9
      INT 21H
      
      JMP DEB 
      
      RET 
      
invalid0: 

      MOV DX,offset ERREUR02
      MOV AH,9
      INT 21H
      
      JMP DEB 
      
      RET
      

LECT1 ENDP             

;---------------------------------------------------------------------------------------------------------------

    


;------------------------                                         -------------------------------          


;----------------------- Conversion du resultat de HEXA vers ASCII ------------------------------

Affichage_Resultat  PROC
 
        MOV SI,offset resultat  ;  on range le resultat trouvee dans la case memoire 
        MOV BX,0Ah                           
        MOV CX,00h
BOUCLE_01:     MOV DX,0
               DIV BX
               ADD DL,'0'
               PUSH DX
               INC CX
               CMP AX,0ah
               JGE BOUCLE_01
               ADD AL,'0'
               MOV [SI],AL
BOUCLE_02:     POP AX
               INC SI
               MOV [SI],AL
               LOOP BOUCLE_02
               INC SI
               MOV AL,'$'
               MOV [SI],AL
                
      ;affichage du resultat           
    MOV AH,,09h     
    MOV DX,offset MESSAGE04
    INT 21h
    MOV AH,,09h     
    MOV DX,offset resultat
    INT 21h          
    JMP M_Menu
    
    RET
Affichage_Resultat ENDP  


;------------------------- Conversion du nombre en chiffres romains depuis l'arabe ----------------------------- 



Arabe_Vers_Romain PROC   
    
    CALL LECT1  ;Lecture du nombre en arabe 
        
      
CONV:                                                                               

        MOV DX,offset MESSAGE02
        MOV AH,9
        INT 21H  
      
        MOV decimal,BX   
    
    
        MOV DI,0
    
        LEA SI,chaine

qtrboucle:    Cmp [SI],'$'       ;Quatrieme boucle
                                 
              JNE eff1 
              JMP SUITE 
              
              
              
eff1:      MOV [SI],0
              INC SI
              JMP qtrboucle 
           
SUITE: Mov si,0            
    
    TQe:
    
             MOV BX, decimal  ;
             CMP BX,0
             JE  FTQe
    
    
prboucle: MOV BX, decimal          ;Premiere boucle
            CMP BX, charabe1[SI]
            JL  fin 
            SUB BX,charabe1[SI] 
            MOV decimal,BX
            MOV AX,chromain1[SI]
    
            MOV chaine[DI],AX 
            ADD DI,2
    
            JMP prboucle
        
    
fin:
            ADD SI,2 
            JMP TQe
    
FTQe:
         
         
             ;Affichage de la chaine du resultat
    
         
    LEA DX,chaine
    MOV AH,9
    INT 21H
        
        
    JMP M_Menu
    
    RET
     
Arabe_Vers_Romain ENDP 

;------------------------------------------------------------------------------------------------


;------------------------- Verification de saisie d'un nombre en chiffres ROMAINS -------------------------------- 
                     
VERIFICATION_ROMAIN  PROC                         
           ;Verification que les caracteres inserees sont romains                                       
           MOV DI,offset nombre       
lecture:   MOV SI,0
           MOV AH,1
           INT 21H          
           CMP Al,13  ;13 est le code ascii du retour chariot
           JE fin_lecture
                                                
COMPARAISON: CMP chromain2[SI],'.'
             JE  INVALIDE2
             CMP byte chromain2[si],al
             JE  suiv
             ADD SI,2
             JMP COMPARAISON                      
suiv:        MOV [DI],AL
             ADD di,2
             JMP lecture 

INVALIDE:  MOV DX,offset ERREUR03
           MOV AH,9
           INT 21H 
           JMP lecture
 
fin_lecture: Mov [DI],'$' 
           ;verification de la syntaxe du nombre entree
           MOV SI,offset nombre
           MOV cpt2 , 0   
           MOV [cpt3],0          

boucle2:    MOV DI,SI
            ADD DI,2 
            CMP [DI],'$'
            JE cvt
            MOV AX,[SI]
            CMP AX,[DI] 
            JNE NO
            INC [cpt3]  
            CMP [cpt3],3
            JGE INVALIDE2
            JMP LP1 
  
           ;X,V,I peuvent suivre I
NO:      MOV [cpt3],0 
LP1:      CMP [SI],'I'
            JNE LP2
            CMP [DI],'I'
            JE LP7
            CMP [DI],'X'
            JE LP6
            CMP [DI],'V'
            JE LP6
            JMP INVALIDE
          ;I peut suivre V 
LP2:      CMP [SI],'V'
          JNE LP3 
          CMP [DI],'V'
          JE INVALIDE2
          CMP [DI],'I'
          JE LP6
          JMP INVALIDE2
          ;D,M peuvent suivre X 
LP3:      CMP [SI],'X'
          JNE LP4 
          CMP [DI],'X'
          JE LP7  
          CMP [DI],'V'
          JE LP8 
          CMP [DI],'I'
          JE LP8   
          CMP [DI],'D'
          JE INVALIDE2
          CMP [DI],'M'
          JE INVALIDE2
          ;X,V,I peuvent suivre L        
LP4:      CMP [SI],'L'
          JNE LP5 
          CMP [DI],'L'
          JE INVALIDE2 
          CMP [DI],'X'
          JE LP6  
          CMP [DI],'V'
          JE  LP6 
          CMP [DI],'I'
          JE LP6 
          JMP INVALIDE2             
          ;C peut suivre D    
LP5:      CMP [SI],'D'
          JNE LP6  
          CMP [DI],'D'
          JE INVALIDE2
          CMP [DI],'C'
          JE LP6 
          CMP [DI],'L'
          JE LP6 
          CMP [DI],'X'
          JE LP6
          CMP [DI],'V'
          JE LP6  
          CMP [DI],'I'
          JE LP6 
          JMP INVALIDE2
           
INVALIDE2: MOV SI,offset nombre
boucle3:      Cmp [SI],'$' 
           JNE efface 
           MOV [SI],0
           JMP INVALIDE
efface:    MOV [SI],0
           INC SI
           JMP boucle3 
LP8:       MOV cpt2,0
           ADD SI,2
           JMP boucle2     
LP7:       INC cpt2
           ADD SI,2
           JMP boucle2            
           
LP6:       CMP cpt2,1
           JGE INVALIDE2
           ADD SI,2
           JMP boucle2 
           
           RET 
VERIFICATION_ROMAIN      ENDP           

;------------------------------------------------------------------------------------------------     

;----------------------------- CONVERSION ROMAIN VERS ARABE ------------------------------------
    
Romain_Vers_Arabe PROC     
    
      MOV DX,offset MESSAGE03
      MOV AH,9
      INT 21H  
      CALL VERIFICATION_ROMAIN    
       

cvt:MOV [precedent],-1 
    MOV [Val_Ltr],0
    MOV [romain],0
    MOV SI,0 
    
TQP: CMP nombre[SI],'$' 
    JE FTQP
    
    MOV [cpt],0
    MOV DI,[cpt]
    MOV [trouv_bool],0
    
TQ:
    CMP DI,14
    JA FTQ
    CMP [trouv_bool],0
    JNE FTQ
    MOV BX,nombre[SI]
    CMP BX,chromain2[DI]
    JE ELSE
    ADD DI,2
    JMP TQ
    
ELSE:MOV [trouv_bool],1
     JMP TQ
     
     
     
FTQ:MOV DX,[precedent]
    CMP DX,DI
    JA test_sup
    MOV [superieur],1
    JMP test_op
     
test_sup:
    MOV [superieur],0
    
    test_op:
    
    CMP [superieur],1
    JNE ELSE2 
    
    MOV AX,[romain] 
    ADD AX,[Val_Ltr]
    JMP FSI
    
ELSE2: MOV AX,[romain ]
       SUB AX,[Val_Ltr]
    
FSI: MOV BX,charabe2[DI]
     MOV [Val_Ltr],BX
     MOV [precedent],DI
     add SI,2 
     mov [romain],AX 
     Jmp TQP
    
FTQP: 
    ADD AX,[Val_Ltr]
    MOV [romain],AX
                    
    CALL Affichage_Resultat
    
    RET 
    
Romain_Vers_Arabe ENDP

;------------------------------------------------------------------------------------------------

;----------------------------------- PROGRAMME PRINCIPAL ----------------------------------------  
           
DEBUT:         MOV AX, sdonnee
         MOV DS, AX               
         CALL Introduction     
 
          
 
       
        
         
QUITTER: MOV AH,4ch
         INT 21h   
       
scode ENDS
      END DEBUT
                       
;------------------------------------------------------------------------------------------------                       
                       
                                   
    