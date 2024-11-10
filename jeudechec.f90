PROGRAM Echec

!Module!

USE placement


IMPLICIT NONE


!Variable!
CHARACTER(LEN=18),DIMENSION(9,9):: echiquier
CHARACTER(LEN=10),DIMENSION(1,16):: mangeblanc
CHARACTER(LEN=10),DIMENSION(1,16):: mangenoir
CHARACTER(LEN=3),DIMENSION(16,1):: verifgood,verifgood2
CHARACTER(LEN=3),DIMENSION(16,1):: verif,verif2
CHARACTER(LEN=3),DIMENSION(16,1):: verifcavalier,verifcavalier2
CHARACTER(LEN=3),DIMENSION(16,1):: veriffou,veriffou2
CHARACTER(LEN=3),DIMENSION(16,1):: verifdamed, verifdamed2
CHARACTER(LEN=3),DIMENSION(16,1):: verifdamea, verifdamea2
CHARACTER(LEN=3),DIMENSION(16,1):: verifmvt, verifmvt2
CHARACTER(LEN=3),DIMENSION(16,1):: verifpion, verifpion2
CHARACTER(LEN=3),DIMENSION(16,1):: verifroi
INTEGER :: I,j,tour,w,compteurb,compteurn, ENDGAME, GAGNE,sortie, itest
INTEGER :: p,q,m,r,s,t,u,a,c,e,f,a1,a2,a3,a4,a5,a6,a7,a8,tour2,comp
CHARACTER (LEN=1) :: colpiecabouger,colpiece
CHARACTER (LEN=18) :: nompiece,nompiece1,nompiece2,nompiece3,nompiece4,nompiece5,nompiece6,nompiece7,nompiece8,nompiece9,nompiece10
CHARACTER (LEN=18) :: nompiece11
CHARACTER (LEN=18) :: piecesortie,changepion
CHARACTER (LEN=18) :: entree, mode
CHARACTER (LEN=3) :: good,good2,good3,good4,good5,good6,good7,good9,ouiounon,goodrapide,ok,bouger,bouger2
INTEGER :: lingpiecabouger, placementcol, lignepiece, placementcol2
INTEGER :: t1bis,t2bis,duree, dureespecifique,dureepartie,tfinal,tinitial,ttour,nombre
CHARACTER (LEN=10) :: joueur1, joueur2, blanc,noir,regles
REAL :: aleatoire,resultat


compteurb=1
compteurn=1


CALL execute_command_line('clear')


!Création de l'échiquier avec les pièces dessus au départ de la partie !

!Repère!
echiquier(9,1)='A'
echiquier(9,2)='B'
echiquier(9,3)='C'
echiquier(9,4)='D'
echiquier(9,5)='E'
echiquier(9,6)='F'
echiquier(9,7)='G'
echiquier(9,8)='H'

echiquier(1,9)='1'
echiquier(2,9)='2'
echiquier(3,9)='3'
echiquier(4,9)='4'
echiquier(5,9)='5'
echiquier(6,9)='6'
echiquier(7,9)='7'
echiquier(8,9)='8'

echiquier(9,9)='Repère'


!Case vide!
DO i=1,8
	DO j=1,8
		echiquier(i,j)='.'
	ENDDO
ENDDO


DO i=1,16
	mangeblanc(1,i)='.'
	mangenoir(1,i)='.'
ENDDO



!Pion!
echiquier(2,1)='PionB1'
echiquier(2,2)='PionB2'
echiquier(2,3)='PionB3'
echiquier(2,4)='PionB4'
echiquier(2,5)='PionB5'
echiquier(2,6)='PionB6'
echiquier(2,7)='PionB7'
echiquier(2,8)='PionB8'

echiquier(7,1)='PionN1'
echiquier(7,2)='PionN2'
echiquier(7,3)='PionN3'
echiquier(7,4)='PionN4'
echiquier(7,5)='PionN5'
echiquier(7,6)='PionN6'
echiquier(7,7)='PionN7'
echiquier(7,8)='PionN8'



!Dame/Roi!
echiquier(1,4)='DameB'
echiquier(1,5)='RoiB'

echiquier(8,5)='RoiN'
echiquier(8,4)='DameN'



!Fou!
echiquier(1,3)='FouB1'
echiquier(1,6)='FouB2'

echiquier(8,3)='FouN1'
echiquier(8,6)='FouN2'



!Cavalier!
echiquier(1,2)='CavalierB1'
echiquier(1,7)='CavalierB2'

echiquier(8,2)='CavalierN1'
echiquier(8,7)='CavalierN2'



!Tour!
echiquier(1,1)='TourB1'
echiquier(1,8)='TourB2'

echiquier(8,1)='TourN1'
echiquier(8,8)='TourN2'

CALL execute_command_line('clear')



!Affichage de départ ligne par ligne!
PRINT*, " BIENVENUE DANS CE JEU D'ÉCHEC !"
PRINT*,''

PRINT*, 'Savez vous qui veut prendre les pièces blanches ?'
PRINT*,'Entrez "OUI" ou "NON"'

READ(*,*)ouiounon

IF ( (ouiounon == 'oui') .OR. ( ouiounon == 'OUI') .OR. (ouiounon == 'Oui') ) THEN
	PRINT*, 'Entrer le nom du joueur 1 (Celui qui choisi les blancs) :'
	READ(*,*)joueur1
	blanc=joueur1
	PRINT*, 'Entrer le nom du joueur 2 (Celui qui choisi les noirs) :'
	READ(*,*)joueur2
	noir=joueur2
ELSEIF ( (ouiounon == 'non') .OR. (ouiounon == 'NON') .OR. (ouiounon == 'Non') ) THEN
	PRINT*, 'Entrer le nom du joueur 1 :'
	READ(*,*)joueur1
	PRINT*, 'Entrer le nom du joueur 2 :'
	READ(*,*)joueur2
	PRINT*,''

	PRINT*, 'Entrez un chiffre qui vous passe par la tête : '
	READ(*,*)itest

	resultat=aleatoire(itest)

	IF (resultat > 0.5) THEN
		sortie = 0
	ELSE 
		sortie = 1
	ENDIF

	IF ( sortie == 0 ) THEN
		CALL execute_command_line('clear')
		PRINT*, "Aléatoirement, c'est ", trim(joueur1)," qui jouera avec les blancs ! "
		PRINT*, " Et donc ", trim(joueur2), " qui a les noirs. "
		blanc=joueur1
		noir=joueur2
	ELSEIF ( sortie == 1 ) THEN
		CALL execute_command_line('clear')
		PRINT*, "Aléatoirement, c'est  ", trim(joueur2), " qui jouera avec les blancs ! "
		PRINT*, "Et donc ", trim(joueur1), " qui a les noirs. "
		blanc=joueur2
		noir=joueur1
		READ(*,*)
	ENDIF
ELSE 
	PRINT*, 'ERREUR !!  Relancer le jeu. En utlisant ^C (sur MAC : control+C) vous pouvez arreter le jeu.'
ENDIF

CALL execute_command_line('clear')

PRINT*,'Quel mode de jeu voulez-vous ?'
PRINT*,'"R" pour rapide'
PRINT*,''
PRINT*,'"N" pour normal'
READ(*,*)mode

IF ( (mode == 'R') .OR. (mode == 'r') .OR. (mode == 'Rapide') .OR. (mode == 'rapide') .OR. (mode == 'RAPIDE') ) THEN
	CALL execute_command_line('clear')
	PRINT*, 'Mode PARTIE RAPIDE'
	PRINT*,''
	PRINT*,'À noter : 1H = 3600 secondes'
	PRINT*,'	    1 min = 60 secondes'
	PRINT*,''
	PRINT*, ' Veuillez chosir la durée maximale du tour (en secondes) : ' 
	READ(*,*)dureespecifique
	PRINT*,'Veuillez chosir la durée maximale de la partie (en secondes) :'
	READ(*,*)dureepartie
ELSEIF ( (mode =='N') .OR. ( mode =='n') .OR. ( mode =='Normal') .OR. ( mode =='normal') .OR. ( mode =='NORMAL') ) THEN
	CALL execute_command_line('clear')
	PRINT*, 'Mode PARTIE NORMAL'
ENDIF

PRINT*,''
PRINT*, 'CLIQUEZ ENTRER'
READ(*,*)

CALL execute_command_line('clear')


PRINT*, 'CLIQUEZ ENTRER POUR CONTINUER'
PRINT*, ""
PRINT*, ' But du jeu : Infliger un échec et mat à son adversaire, c est à dire : "manger" le roi de la couleur adverse.'
PRINT*,""

PRINT*, 'Connaissez vous les règles du jeu ?'
READ(*,*)regles

IF ( (regles == 'Non') .OR. (regles == 'NON') .OR. (regles == 'non') ) THEN	
	PRINT*,'Règles :'
	PRINT*,'Jouer tour à tour.'
	READ(*,*)
	PRINT*, "Le PION peut se déplacer d'une case vers l'avant et peut manger uniquement d'une case sur la diagonale."
	READ(*,*)
	PRINT*, "La TOUR peut se déplacer et manger sur une même ligne ou sur une même colonne."
 	PRINT*, "Elle ne peut passer au dessus d'une pièce."
	PRINT*, "Elle peut manger en atterissant sur la case de la pièce d'une couleur adverse."
	READ(*,*)
	PRINT*, 'Le CAVALIER peut se déplacer sous forme de "L". Il peut passer au dessus d une pièce.'
	PRINT*, "Il peut manger en atterissant sur la case de la pièce d une couleur adverse."
	READ(*,*)
	PRINT*, "Le FOU peut se déplacer en diagonale. Il ne peut pas passer au dessus d'une pièce."
 	PRINT*, "Il peut manger en atterissant sur la case de la pièce d'une couleur adverse."
	READ(*,*)
	PRINT*, "La DAME est un mélange du fou et de la tour."
	PRINT*, "Elle peut se déplacer sur la diagonale, sur la même ligne ou sur la même colonne."
	PRINT*, "Elle ne peut pas passer au dessus d'une pièce. "
	PRINT*, "Elle peut manger en atterissant sur la case de la pièce d'une couleur adverse."
	READ(*,*)
	PRINT*, "Le Roi peut se déplacer uniquement d'une case mais peut bouger dans toutes les directions. "
	PRINT*,"Il peut manger en atterissant sur la case de la pièce d'une couleur adverse."
	READ(*,*)
ENDIF
CALL execute_command_line('clear')
PRINT*,'Faites attention à ne pas vous trompez dans la saisie du placement. Une erreur vous ferez arrêter la partie.'

READ(*,*)
CALL execute_command_line('clear')


PRINT*,''
PRINT*, 'BONNE CHANCE !'
PRINT*,''



DO i=1,9
	PRINT*, echiquier(i,:)
ENDDO
PRINT*,''

ENDGAME=0
GAGNE = 10



!MODE NORMAL!
IF ( (mode == 'N') .OR. (mode== 'n') ) THEN
tour=0
DO WHILE (ENDGAME == 0)
	tour=tour + 1
	PRINT*,''
	PRINT*,''
tour2=tour/2
IF ( tour2 * 2 == tour) THEN
	PRINT*,'Au tour de ',trim(noir), ' (Les noirs)'
ELSEIF ( tour2 * 2 +1 == tour) THEN
	PRINT*,'Au tour de ',trim(blanc), ' (Les blancs)'
ENDIF
PRINT*,''
	good='no'
	ok='no'
	changepion='no'
	DO WHILE (good=='no') 
	bouger='no'
		DO WHILE (bouger == 'no')
			PRINT*,"Entrez la colonne de la pièce à bouger (exemple RoiN = E) :"
			READ(*,*)colpiecabouger

			PRINT*,"Entrez la ligne de la pièce à bouger (exemple RoiN = 8) :"
			READ(*,*)lingpiecabouger

			PRINT*,''
			PRINT*,''

			!Entrée du placement d'arrivée de la pièce!	
			PRINT*,"Entrez la colonne du placement de la pièce voulue :"
			READ(*,*)colpiece

			PRINT*,"Entrez la ligne du placement de la pièce voulue :"
			READ(*,*)lignepiece

			CALL place(colpiecabouger,placementcol)

			entree=echiquier(lingpiecabouger,placementcol)

			CALL place(colpiece,placementcol2)
	
			DO i=1,16
				verifmvt(i,1)='.'
				verifmvt2(i,1)='.'
			ENDDO
			comp=1
			tour2=tour/2
			IF ( tour2 * 2 == tour) THEN !TOUR NOIR!
				OPEN(UNIT=10, FILE='liste_piece_blanche.txt', STATUS = 'OLD', ACTION ='READ')
				DO i = 1,16
					READ(10,*)nompiece11
					IF ( entree == nompiece11) THEN
						bouger2='no'
						verifmvt(i,1)=bouger2
					ENDIF
				ENDDO
				CLOSE(UNIT=10)

				DO WHILE (verifmvt(comp,1)=='.')
					IF (comp==17) THEN
						EXIT
					ELSE
						comp=comp+1
					ENDIF
				ENDDO
				IF ( comp-1 == 16 ) THEN
					bouger='yes'
				ELSE
					bouger='no'
					PRINT*,''
					PRINT*,"Erreur : C'est au tour des noirs !"
					PRINT*,''
					CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)

				ENDIF
			ELSEIF ( tour2 * 2 +1 == tour) THEN !TOUR BLANC!
				OPEN(UNIT=20, FILE='liste_piece_noir.txt', STATUS = 'OLD', ACTION ='READ')
				DO i = 1,16
					READ(20,*)nompiece11
					IF ( entree == nompiece11) THEN
						bouger2='no'
						verifmvt2(i,1)=bouger2
					ENDIF
				ENDDO
				CLOSE(UNIT=20)
				DO WHILE (verifmvt2(comp,1)=='.')
					IF (comp==17) THEN
						EXIT
					ELSE
						comp=comp+1
					ENDIF
				ENDDO
				IF ( comp-1 == 16 ) THEN
					bouger='yes'
				ELSE
					bouger='no'
					PRINT*,''
					PRINT*,"Erreur : C'est au tour des blancs !"
					PRINT*,''
					CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
				ENDIF
			ENDIF
		ENDDO
	m=1
	p=1
	q=1
	r=1
	s=1
	t=1
	u=1
	a=1
	c=1
	e=1
	a1=1
	a2=1
	a3=1
	a4=1
	a5=1
	a6=1
	a7=1
	a8=1

	DO i=1,16
		verifgood(i,1)='.'
		verifgood2(i,1)='.'
	ENDDO

	DO i=1,16
		verif(i,1)='.'
		verif2(i,1)='.'
	ENDDO
	
	DO i=1,16
		verifcavalier(i,1)='.'
		verifcavalier2(i,1)='.'
	ENDDO

	DO i=1,16
		veriffou(i,1)='.'
		veriffou2(i,1)='.'
	ENDDO
	
	DO i=1,16
		verifdamea(i,1)='.'
		verifdamea2(i,1)='.'
	ENDDO

	DO i=1,16
		verifdamed(i,1)='.'
		verifdamed2(i,1)='.'
	ENDDO

	DO i=1,16
		verifroi	(i,1)='.'
	ENDDO

	DO i=1,16
		verifpion(i,1)='.'
		verifpion2(i,1)='.'
	ENDDO



!TEST CASE VIDE ?!
		IF (entree == '.') THEN
			good='no'
			PRINT*, 'ERREUR : La case sélectionée ne détient aucune pièce à bouger. Veuillez entrer une case contenant une piece.' 
			PRINT*, ''
			CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
		ENDIF 


!MOUVEMENT PIONS BLANCS!
		OPEN(UNIT=30, FILE='liste_pions_blanc.txt', STATUS = 'OLD', ACTION ='READ')
			DO w=1,8
				READ(30,*)nompiece
				IF ( entree == nompiece ) THEN
					IF ( lignepiece == lingpiecabouger + 1) THEN	
						IF ( (placementcol2 ==  placementcol) ) THEN
							IF ( echiquier(lignepiece, placementcol2) == '.') THEN
								good='yes'
							ELSE
								good='no'
								PRINT*,' ERREUR : Le pion blanc ne peut pas avancer. Il y a une pièce devant lui.'
								PRINT*, 'Entrer un mouvement correct.'
								PRINT*, ''
								PRINT*," Règle : Le pion se déplace vers l'avant d'une seule case."	
								CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)						
							ENDIF
						ELSEIF ( (placementcol2 ==  placementcol-1) .OR. (placementcol2 ==  placementcol+1) ) THEN
							OPEN(UNIT=10, FILE='liste_piece_blanche.txt', STATUS = 'OLD', ACTION ='READ')
							DO i=1,16
								READ(10,*)nompiece2
								IF ( echiquier(lignepiece, placementcol2) == nompiece2 ) THEN
									good2='no'
									verifpion(i,1)=good2
									PRINT*, "Le pion peut manger uniquement sur sa diagonale d'une seule case."
								ELSEIF ( echiquier(lignepiece, placementcol2) == '.' ) THEN
									good2='no'
									verifpion(i,1)=good2
								ELSE
									good2='yes'
								ENDIF
									
							ENDDO
							CLOSE (UNIT=10)
							DO WHILE (verifpion(r,1)=='.')
								IF (r==17) THEN
									EXIT
								ELSE
									r=r+1
								ENDIF
							ENDDO
							IF ( r-1 == 16 ) THEN
								good='yes'
							ELSE
								good='no'
								PRINT*,'ERREUR, le pion ne peut effectuer le mouvement voulu.'
								CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
							ENDIF
						ELSE
							good='no' 
							PRINT*, 'Le pion ne peut pas effectuer le mouvement voulu.'
							PRINT*," Règle : Le pion se déplace vers l'avant d'une seule case."	
							CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
						ENDIF
						
					ELSE 
						PRINT*, ' ERREUR :  Le pion choisi ne peut pas effectuer le déplacement voulu.'
						PRINT*,''
						good='no'
						PRINT*, " Règle : Le pion se déplace vers l'avant d'une seule case et peut manger sur sa diagonale d'une seule case aussi."
						PRINT*,''
						CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
					ENDIF
				END IF
			ENDDO
		CLOSE (UNIT=30) 

 
!MOUVEMENT PION NOIRS!
		OPEN(UNIT=40, FILE='liste_pions_noir.txt', STATUS = 'OLD', ACTION ='READ')
			DO w=1,8
				READ(40,*)nompiece
				IF ( entree == nompiece ) THEN
					IF ( lignepiece == lingpiecabouger - 1) THEN	
						IF ( (placementcol2 ==  placementcol) ) THEN
							IF ( echiquier(lignepiece, placementcol2) == '.') THEN
								good='yes'
							ELSE
								good='no'
								PRINT*,' ERREUR : Le pion noir ne peut pas avancer. Il y a une pièce devant lui.'
								PRINT*, 'Entrer un mouvement correct.'
								PRINT*, ''
								PRINT*," Règle : Le pion se déplace vers l'avant d'une seule case."		
								CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)					
							ENDIF
						ELSEIF ( (placementcol2 ==  placementcol-1) .OR. (placementcol2 ==  placementcol+1) ) THEN
							OPEN(UNIT=20, FILE='liste_piece_noir.txt', STATUS = 'OLD', ACTION ='READ')
							DO i=1,16
								READ(20,*)nompiece2
								IF ( echiquier(lignepiece, placementcol2) == nompiece2 ) THEN
									good2='no'
									verifpion(i,1)=good2
									PRINT*, "Le pion peut manger uniquement sur sa diagonale d'une seule case."
									CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
								ELSEIF ( echiquier(lignepiece, placementcol2) == '.' ) THEN
									good2='no'
										verifpion(i,1)=good2
								ELSE
									good2='yes'
								ENDIF
									
							ENDDO
							CLOSE (UNIT=20)
							DO WHILE (verifpion(r,1)=='.')
								IF (r==17) THEN
									EXIT
								ELSE
									r=r+1
								ENDIF
							ENDDO
							IF ( r-1 == 16 ) THEN
								good='yes'
							ELSE
								good='no'
								PRINT*,'ERREUR, le pion ne peut effectuer le mouvement voulu.'
								CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
							ENDIF
						ELSE
							good='no' 
							PRINT*, 'Le pion ne peut pas effectuer le mouvement voulu.'
							PRINT*," Règle : Le pion se déplace vers l'avant d'une seule case."	
							CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
						ENDIF
					ELSE 
						PRINT*, ' ERREUR :  Le pion choisi ne peut pas effectuer le déplacement voulu.'
						PRINT*,''
						good='no'
						PRINT*, " Règle : Le pion se déplace vers l'avant d'une seule case et peut manger sur sa diagonale d'une seule case aussi."
						PRINT*,''
						CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
					ENDIF
				END IF
			ENDDO
		CLOSE (UNIT=40)


	OPEN(UNIT=40, FILE='liste_pions_noir.txt', STATUS = 'OLD', ACTION ='READ')
			DO w=1,8
				READ(40,*)nompiece
				IF ( entree == nompiece ) THEN
					IF ( lignepiece == 1) THEN
						CALL pionnoir(piecesortie)
						OPEN(UNIT=10, FILE='liste_piece_blanche.txt', STATUS = 'OLD', ACTION ='READ')
						DO i= 1,16
							READ(10,*)nompiece9
							IF (echiquier(lignepiece,placementcol2)==nompiece9) THEN
								mangeblanc(1,compteurb)=nompiece9
								compteurb=compteurb+1
							END IF
						ENDDO
						CLOSE(UNIT=10)
						echiquier(lignepiece,placementcol2)= piecesortie
						echiquier(lingpiecabouger,placementcol)='.'
						changepion='ok'
					ENDIF
				ENDIF
			ENDDO
	CLOSE(unit=40)

	OPEN(UNIT=30, FILE='liste_pions_blanc.txt', STATUS = 'OLD', ACTION ='READ')		
			DO w=1,8
				READ(30,*)nompiece
				IF ( entree == nompiece ) THEN
					IF ( lignepiece == 8) THEN
						CALL pionblanc(piecesortie)
						OPEN(UNIT=20, FILE='liste_piece_noir.txt', STATUS = 'OLD', ACTION ='READ')
						DO i= 1,16
							READ(10,*)nompiece9
							IF (echiquier(lignepiece,placementcol2)==nompiece9) THEN
								mangeblanc(1,compteurb)=nompiece9
								compteurb=compteurb+1
							END IF
						ENDDO
						CLOSE(UNIT=20)
						echiquier(lignepiece,placementcol2)= piecesortie
						echiquier(lingpiecabouger,placementcol)='.'
						changepion='ok'
					ENDIF
				ENDIF
			ENDDO
	CLOSE(unit=30)
				

!Mouvement Tour!
			OPEN(UNIT=10, FILE='liste_piece_blanche.txt', STATUS = 'OLD', ACTION ='READ')
			OPEN(UNIT=20, FILE='liste_piece_noir.txt', STATUS = 'OLD', ACTION ='READ')
			DO w=1,16
				READ(10,*)nompiece7
				READ(20,*)nompiece8
				IF ( (entree == 'TourB1') .OR. (entree == 'TourB2') ) THEN 
!tour blanche ne peut pas manger de piece blanc (ICI VERIFICATION DE SI C'EST UNE PIECE BLANCHE)!
						IF ( (echiquier(lignepiece,placementcol2) == nompiece7) ) THEN !si le placement d'arrivée est une piece blanche
							good2='no'
							verif(w,1)=good2
							PRINT*,''
							PRINT*, 'ERREUR : Une tour blanche ne peut pas manger une pièce blanche.'
							PRINT*,''
							
						ELSE
							good2='yes'
						ENDIF
				ELSEIF ( (entree == 'TourN1') .OR. (entree == 'TourN2') ) THEN 		
!tour noir ne peut pas manger de piece blanc (ICI VERIFICATION DE SI C'EST UNE TOUR NOIR)!
						IF ( (echiquier(lignepiece,placementcol2) == nompiece8) ) THEN !si le placement d'arrivée est une piece Noire
							good2='no'
							verif2(w,1)=good2
							PRINT*,''
							PRINT*, 'ERREUR : Une tour noire ne peut pas manger une pièce noire.'
							PRINT*,''
						ELSE
							good2='yes'
						ENDIF
					ENDIF
				ENDDO
				CLOSE (UNIT=10)
				CLOSE (UNIT=20)
				DO WHILE (verif(r,1)=='.')
					IF (r==17) THEN
						EXIT
					ELSE
						r=r+1
					ENDIF
				ENDDO
				DO WHILE (verif2(s,1)=='.')
					IF (s==17) THEN
						EXIT
					ELSE
						s=s+1
					ENDIF
				ENDDO
				IF ( s+r-2 == 32 ) THEN
					good3='yes'
				ELSE 
					good3='no'
				ENDIF
	IF ( good3 == 'no') THEN
		good='no'
		CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
	ELSEIF ( good3 == 'yes') THEN
		IF ( (entree == 'TourB1') .OR. (entree == 'TourB2') .OR. (entree == 'TourN1') .OR. (entree == 'TourN2') ) THEN
			IF ( lingpiecabouger == lignepiece ) THEN 				!Si la ligne est la même la tour change uniquement de colonne!
			!Mouvement possible si aucune piece sur la trajectoire!
				DO i = 1, abs((placementcol2 - placementcol))
					IF ( (placementcol2 - placementcol) < 0 ) THEN  		!Si colonne final - colonne initiale <0 => Droite vers gauche
						OPEN(UNIT=10, FILE='liste_piece_blanche.txt', STATUS = 'OLD', ACTION ='READ')
						OPEN(UNIT=20, FILE='liste_piece_noir.txt', STATUS = 'OLD', ACTION ='READ')
						DO w=1,16
							READ(10,*)nompiece5
							READ(20,*)nompiece6
							IF ( (echiquier(lignepiece, placementcol-i) == nompiece5) ) THEN
								IF ( i == abs((placementcol2 - placementcol)) ) THEN 
									
								ELSE
									good2='no'
									verifgood(m,1)=good2
									PRINT*,''
									PRINT*, 'ERREUR : La tour que vous avez choisi ne peut pas effectuer un mouvement vers la gauche,'
									PRINT*, 'il y a une pièce sur sa trajectoire.'
									PRINT*, "Veuillez entrer une position d'arrivée différente."
									PRINT*,''
								ENDIF

							ELSE
								good2='yes'
							ENDIF
							IF ( echiquier(lignepiece, placementcol-i) == nompiece6 ) THEN
								IF ( i == abs((placementcol2 - placementcol)) ) THEN 
									
								ELSE
									good2='no'
									verifgood2(m,1)=good2
									PRINT*,''
									PRINT*, 'ERREUR : La tour que vous avez choisi ne peut pas effectuer un mouvement vers la gauche,'
									PRINT*, 'il y a une pièce sur sa trajectoire.'
									PRINT*, "Veuillez entrer une position d'arrivée différente."
									PRINT*,''
								ENDIF
							ELSE
								good2='yes'
							ENDIF
						ENDDO
						CLOSE (UNIT=10)
						CLOSE (UNIT=20)
						DO WHILE (verifgood(m,1)=='.')
							IF (m==17) THEN
								EXIT
							ELSE
								m=m+1
							ENDIF
						ENDDO
						DO WHILE (verifgood2(p,1)=='.')
							IF (p==17) THEN
								EXIT
							ELSE
								p=p+1
							ENDIF
						ENDDO
						IF ( p+m-2 == 32 ) THEN
							good='yes'
						ELSE 
							good='no'
							CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
						ENDIF
					ELSEIF ( (placementcol2 - placementcol) == 0 ) THEN 	!Si colonne final - colonne initiale =0 => même place
						PRINT*,''
						PRINT*, "La tour que vous avez choisi ne peut pas rester à sa même place."
						PRINT*, "Veuillez effectuer un déplacement avec cette pièce."
						PRINT*,''
					ELSEIF ( (placementcol2 - placementcol) > 0 ) THEN  	!Si colonne final - colonne initiale >0 => gauche vers droite
						OPEN(UNIT=10, FILE='liste_piece_blanche.txt', STATUS = 'OLD', ACTION ='READ')
						OPEN(UNIT=20, FILE='liste_piece_noir.txt', STATUS = 'OLD', ACTION ='READ')
						DO w=1,16
							READ(10,*)nompiece5
							READ(20,*)nompiece6
							IF ( (echiquier(lignepiece, placementcol+i) == nompiece5) ) THEN
								IF ( i == abs((placementcol2 - placementcol)) ) THEN 
									
								ELSE
									good2='no'
									verifgood(m,1)=good2
									PRINT*, ''
									PRINT*, 'ERREUR : La tour que vous avez choisi ne peut pas effectuer de mouvement vers la droite,'
									PRINT*, 'il y a une pièce sur sa trajectoire.'
									PRINT*, "Veuillez entrer une position d'arrivée différente."
									PRINT*,''
								ENDIF
							ELSE
								good2='yes'
							ENDIF
							IF ( echiquier(lignepiece,placementcol+i) == nompiece6 ) THEN
								IF ( i == abs((placementcol2 - placementcol)) ) THEN 
									
								ELSE
									good2='no'
									verifgood2(m,1)=good2
									PRINT*, ''
									PRINT*, 'ERREUR : La tour que vous avez choisi ne peut pas effectuer de mouvement vers la droite,'
									PRINT*, 'il y a une pièce sur sa trajectoire.'
									PRINT*, "Veuillez entrer une position d'arrivée différente."
									PRINT*,''
								ENDIF
							ELSE
								good2='yes'
							ENDIF
						ENDDO
						CLOSE (UNIT=10)
						CLOSE (UNIT=20)
						DO WHILE (verifgood(m,1)=='.')
							IF (m==17) THEN
								EXIT
							ELSE
								m=m+1
							ENDIF
						ENDDO
						DO WHILE (verifgood2(p,1)=='.')
							IF (p==17) THEN
								EXIT
							ELSE
								p=p+1
							ENDIF
						ENDDO
						IF ( p+m-2 == 32 ) THEN
							good='yes'
						ELSE 
							good='no'
							CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
						ENDIF
					ENDIF
				ENDDO
			
			ELSE IF ( placementcol == placementcol2 ) THEN 	!Si la colonne est la même la tour change uniquement de ligne!
				DO i = 1,abs((lignepiece - lingpiecabouger))
					p=1
					m=1
					IF ( (lignepiece - lingpiecabouger) < 0 ) THEN 		
						OPEN(UNIT=10, FILE='liste_piece_blanche.txt', STATUS = 'OLD', ACTION ='READ')
						OPEN(UNIT=20, FILE='liste_piece_noir.txt', STATUS = 'OLD', ACTION ='READ')
						DO w=1,16
							READ(10,*)nompiece7
							READ(20,*)nompiece8
							IF ( (echiquier(lingpiecabouger-i,placementcol) == nompiece7) ) THEN
								IF ( i == abs((lignepiece - lingpiecabouger)) ) THEN 
									
								ELSE
									good2='no'
									verifgood(w,1)=good2
									PRINT*, ''
									PRINT*, 'ERREUR : La tour que vous avez choisi ne peut pas effectuer de mouvement vers le haut,'
									PRINT*, 'il y a une pièce sur sa trajectoire.'
									PRINT*, "Veuillez entrer une position d'arrivée différente."
									PRINT*,''
								ENDIF
							ENDIF
							IF ( echiquier(lingpiecabouger-i,placementcol) == nompiece8 ) THEN
								IF ( i == abs((lignepiece - lingpiecabouger)) ) THEN 
									
								ELSE
									good2='no'
									verifgood2(w,1)=good2
									PRINT*, ''
									PRINT*, 'ERREUR : La tour que vous avez choisi ne peut pas effectuer de mouvement vers le haut,'
									PRINT*, 'il y a une pièce sur sa trajectoire.'
									PRINT*, "Veuillez entrer une position d'arrivée différente."
									PRINT*,''
								ENDIF
							ENDIF 
						ENDDO
						CLOSE (UNIT=10)
						CLOSE (UNIT=20)
						DO WHILE (verifgood(m,1)=='.')
							IF (m==17) THEN
								EXIT
							ELSE
								m=m+1
							ENDIF
						ENDDO
						DO WHILE (verifgood2(p,1)=='.')
							IF (p==17) THEN
								EXIT
							ELSE
								p=p+1
							ENDIF
						ENDDO
						IF ( p+m-2 == 32 ) THEN
							good='yes'
						ELSE 
							good='no'
							CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
						ENDIF
					ELSEIF ( (lignepiece - lingpiecabouger) == 0 ) THEN
						PRINT*,''
						PRINT*, "La tour que vous avez choisi ne peut pas rester à sa même place."
						PRINT*, "Veuillez effectuer un déplacement avec cette pièce."
						PRINT*,''
					ELSEIF ( (lignepiece - lingpiecabouger) > 0 ) THEN !Si ligne final - ligne depart > 0 = descend
						OPEN(UNIT=10, FILE='liste_piece_blanche.txt', STATUS = 'OLD', ACTION ='READ')
						OPEN(UNIT=20, FILE='liste_piece_noir.txt', STATUS = 'OLD', ACTION ='READ')
						DO w=1,16
							READ(10,*)nompiece7
							READ(20,*)nompiece8
							IF ( (echiquier(lingpiecabouger+i,placementcol) == nompiece7) ) THEN
								IF ( i == abs((lignepiece - lingpiecabouger)) ) THEN 
									
								ELSE
									good2='no'
									verifgood(w,1)=good2
									PRINT*, ''
									PRINT*, 'ERREUR : La tour que vous avez choisi ne peut pas effectuer de mouvement vers le bas,'
									PRINT*, 'il y a une pièce sur sa trajectoire.'
									PRINT*, "Veuillez entrer une position d'arrivée différente."
									PRINT*,''
								ENDIF
			
							ELSE
								good2='yes'
							ENDIF
							IF ( echiquier(lingpiecabouger+i,placementcol) == nompiece8 ) THEN
								IF ( i == abs((lignepiece - lingpiecabouger)) ) THEN 
									
								ELSE
									good2='no'
									verifgood2(w,1)=good2
									PRINT*, ''
									PRINT*, 'ERREUR : La tour que vous avez choisi ne peut pas effectuer de mouvement vers le bas,'
									PRINT*, 'il y a une pièce sur sa trajectoire.'
									PRINT*, "Veuillez entrer une position d'arrivée différente."
									PRINT*,''
								ENDIF
							ELSE
								good2='yes'
							ENDIF
						ENDDO
						CLOSE (UNIT=10)
						CLOSE (UNIT=20)
						DO WHILE (verifgood(m,1)=='.')
							IF (m==17) THEN
								EXIT
							ELSE
								m=m+1
							ENDIF
						ENDDO
						DO WHILE (verifgood2(p,1)=='.')
							IF (p==17) THEN
								EXIT
							ELSE
								p=p+1
							ENDIF
						ENDDO
						IF ( p+m-2 == 32 ) THEN
							good='yes'
						ELSE 
							good='no'
							CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
						ENDIF
					ENDIF
				ENDDO
			ELSE
				PRINT*,"ERREUR : La tour ne peut effectuer que des mouvements sur la même ligne ou la même colonne. "
				PRINT*,"Entrer un mouvement correct."
				PRINT*,''
			END IF
		ENDIF
	ENDIF


!MOUVEMENT CAVALIERS BLANCS!
	IF ( (entree == 'CavalierB1') .OR. (entree == 'CavalierB2') ) THEN
		OPEN(UNIT=10, FILE='liste_piece_blanche.txt', STATUS = 'OLD', ACTION ='READ')
		DO i =1,16
			READ(10,*)nompiece
			IF ( echiquier(lignepiece, placementcol2) == nompiece ) THEN
				good4='no'
				verifcavalier(i,1)=good4
				PRINT*,''
				PRINT*," ERREUR : Le cavalier blanc choisi ne peut pas manger une pièce blanche."
				PRINT*,"Entrez un mouvement correct."
				PRINT*,''
			ELSE
				good4='yes'
			ENDIF
		ENDDO
		CLOSE (UNIT=10)
		DO WHILE (verifcavalier(t,1)=='.')
			IF (t==17) THEN
				EXIT
			ELSE
				t=t+1
			ENDIF
		ENDDO
		IF ( t-1 == 16 ) THEN
			good4='yes'
		ELSE 
			good4='no'
		ENDIF
		IF ( good4 == 'no' ) THEN 
			good='no'
			CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
		ELSEIF ( good4 == 'yes') THEN
			IF ( (lignepiece == lingpiecabouger - 2) .AND. (placementcol2 == placementcol - 1) ) THEN  		!Mouvement du cavalier blanc vers la gauche de 1 et vers le haut de 2
				good='yes'
			ELSEIF ( (lignepiece == lingpiecabouger -1) .AND. (placementcol2 == placementcol - 2) ) THEN
				good='yes'
			ELSEIF ( (lignepiece == lingpiecabouger + 1) .AND. (placementcol2 == placementcol - 2) ) THEN
				good='yes'
			ELSEIF ( (lignepiece == lingpiecabouger + 2) .AND. (placementcol2 == placementcol - 1) ) THEN
				good='yes'
			ELSEIF ( (lignepiece == lingpiecabouger + 2) .AND. (placementcol2 == placementcol +1 ) ) THEN
				good='yes'
			ELSEIF ( (lignepiece == lingpiecabouger + 1) .AND. (placementcol2 == placementcol + 2) ) THEN
				good='yes'
			ELSEIF ( (lignepiece == lingpiecabouger - 1) .AND. (placementcol2 == placementcol + 2) ) THEN
				good='yes'
			ELSEIF ( (lignepiece == lingpiecabouger - 2) .AND. (placementcol2 == placementcol + 1) ) THEN
				good='yes'
			ELSE 
				PRINT*,''
				PRINT*,"ERREUR : Le cavalier blanc choisi ne peut pas effectuer le mouvement choisi."
				PRINT*,'Règle : Le cavalier ne peut bouger qu avec des mouvements de "L".'
				PRINT*,''
				CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
			ENDIF
		ENDIF
	ENDIF


!MOUVEMENT CAVALIERS NOIRS!

IF ( (entree == 'CavalierN1') .OR. (entree == 'CavalierN2') ) THEN
		OPEN(UNIT=20, FILE='liste_piece_noir.txt', STATUS = 'OLD', ACTION ='READ')
		DO i =1,16
			READ(20,*)nompiece
			IF ( echiquier(lignepiece, placementcol2) == nompiece ) THEN
				good4='no'
				verifcavalier(i,1)=good4
				PRINT*, 'nompiece=',nompiece
				PRINT*,''
				PRINT*,"ERREUR : Le cavalier noir choisi ne peut pas manger une pièce noire."
				PRINT*,"Entrez un mouvement correct."
				PRINT*,''
			ELSE
				good4='yes'
			ENDIF
		ENDDO
		CLOSE (UNIT=20)
		DO WHILE (verifcavalier(t,1)=='.')
			IF (t==17) THEN
				EXIT
			ELSE
				t=t+1
			ENDIF
		ENDDO
		IF ( t-1 == 16 ) THEN
			good4='yes'
		ELSE 
			good4='no'
		ENDIF
		IF ( good4 == 'no' ) THEN 
			good='no'
			CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
		ELSEIF ( good4 == 'yes') THEN
			IF ( (lignepiece == lingpiecabouger - 2) .AND. (placementcol2 == placementcol - 1) ) THEN  		!Mouvement du cavalier blanc vers la gauche de 1 et vers le haut de 2
				good='yes'
			ELSEIF ( (lignepiece == lingpiecabouger -1) .AND. (placementcol2 == placementcol - 2) ) THEN
				good='yes'
			ELSEIF ( (lignepiece == lingpiecabouger + 1) .AND. (placementcol2 == placementcol - 2) ) THEN
				good='yes'
			ELSEIF ( (lignepiece == lingpiecabouger + 2) .AND. (placementcol2 == placementcol - 1) ) THEN
				good='yes'
			ELSEIF ( (lignepiece == lingpiecabouger + 2) .AND. (placementcol2 == placementcol +1 ) ) THEN
				good='yes'
			ELSEIF ( (lignepiece == lingpiecabouger + 1) .AND. (placementcol2 == placementcol + 2) ) THEN
				good='yes'
			ELSEIF ( (lignepiece == lingpiecabouger - 1) .AND. (placementcol2 == placementcol + 2) ) THEN
				good='yes'
			ELSEIF ( (lignepiece == lingpiecabouger - 2) .AND. (placementcol2 == placementcol + 1) ) THEN
				good='yes'
			ELSE 
				PRINT*,''
				PRINT*,"ERREUR : Le cavalier noir choisi ne peut pas effectuer le mouvement choisi."
				PRINT*,'Règle : Le cavalier ne peut bouger qu avec des mouvements de "L".'
				PRINT*,''
				CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
			ENDIF
		ENDIF
	ENDIF


!Mouvement fou!

	IF ( (entree == 'FouB1') .OR. (entree == 'FouB2') ) THEN
		OPEN(UNIT=10, FILE='liste_piece_blanche.txt', STATUS = 'OLD', ACTION ='READ')
		DO i =1,16
			READ(10,*)nompiece
			IF ( echiquier(lignepiece, placementcol2) == nompiece ) THEN
				good2='no'
				verif(i,1)=good2
				PRINT*,''
				PRINT*," ERREUR : Le fou blanc choisi ne peut pas manger une pièce blanche."
				PRINT*,"Entrez un mouvement correct."
				PRINT*,''
			ELSE
				good2='yes'
			ENDIF
		ENDDO
		CLOSE (UNIT=10)
		DO WHILE (verif(u,1)=='.')
			IF (u==17) THEN
				EXIT
			ELSE
				u=u+1
			ENDIF
		ENDDO
		IF ( u-1 == 16 ) THEN
			good5='yes'
		ELSE 
			good5='no'
		ENDIF
	ELSEIF ( (entree == 'FouN1') .OR. (entree == 'FouN2') ) THEN
		OPEN(UNIT=20, FILE='liste_piece_noir.txt', STATUS = 'OLD', ACTION ='READ')
		DO i =1,16
			READ(20,*)nompiece
			IF ( echiquier(lignepiece, placementcol2) == nompiece ) THEN
				good2='no'
				verif(i,1)=good2
				PRINT*,''
				PRINT*," ERREUR : Le fou noir choisi ne peut pas manger une pièce noir."
				PRINT*,"Entrez un mouvement correct."
				PRINT*,''
			ELSE
				good2='yes'
			ENDIF
		ENDDO
		CLOSE (UNIT=20)
		DO WHILE (verif(u,1)=='.')
			IF (u==17) THEN
				EXIT
			ELSE
				u=u+1
			ENDIF
		ENDDO
		IF ( u-1 == 16 ) THEN
			good5='yes'
		ELSE 
			good5='no'
		ENDIF
	ENDIF
	IF ((entree == 'FouB1') .OR. (entree == 'FouB2') .OR.(entree == 'FouN1') .OR. (entree == 'FouN2') ) THEN
		IF ( good5 == 'no' ) THEN 
			good='no'
			CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
		ELSEIF ( good5 == 'yes') THEN
			IF ( abs(lignepiece - lingpiecabouger) == abs(placementcol2 - placementcol) ) THEN  !Déplacement du fou en diagonale si bouge de X en ligne et de X en colonne
				IF ( (lignepiece - lingpiecabouger < 0) .AND. (placementcol2 - placementcol > 0) ) THEN
					DO i=1,abs(lignepiece - lingpiecabouger)
						OPEN(UNIT=10, FILE='liste_piece_blanche.txt', STATUS = 'OLD', ACTION ='READ')
						OPEN(UNIT=20, FILE='liste_piece_noir.txt', STATUS = 'OLD', ACTION ='READ')
						DO w = 1,16
							READ(10,*)nompiece1
							READ(20,*)nompiece2
							IF ( echiquier(lingpiecabouger-i, placementcol + i) == nompiece1 ) THEN
								IF ( i == abs((lignepiece - lingpiecabouger)) ) THEN 

								ELSE
									good6='no'
									veriffou(w,1)=good6
									PRINT*,''
									PRINT*," ERREUR : Le fou choisi ne effectuer le mouvement choisi, il y a une pièce blanche sur sa trajectoire."
									PRINT*,"Entrez un mouvement correct."
									PRINT*,''
								ENDIF
							ELSE
								good6='yes'
							ENDIF
							IF ( echiquier(lingpiecabouger-i, placementcol + i) == nompiece2 ) THEN
								IF ( i == abs((lignepiece - lingpiecabouger)) ) THEN 
									
								ELSE
									good6='no'
									veriffou2(w,1)=good6
									PRINT*,''
									PRINT*," Le fou choisi ne effectuer le mouvement choisi, il y a une pièce noire sur sa trajectoire."
									PRINT*,"Entrez un mouvement correct."
									PRINT*,''
								ENDIF
							ELSE
								good6='yes'
							ENDIF
						ENDDO
						CLOSE (UNIT=10)
						CLOSE (UNIT=20)
						DO WHILE (veriffou(a,1)=='.')
							IF (a==17) THEN
								EXIT
							ELSE
								a=a+1
							ENDIF
						ENDDO
						DO WHILE (veriffou2(c,1)=='.')
							IF (c==17) THEN
								EXIT
							ELSE
								c=c+1
							ENDIF
						ENDDO
						IF ( a+c-2 == 32 ) THEN
							good='yes'
						ELSE 
							good='no'
							CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
						ENDIF
					ENDDO
				ELSEIF ( (lignepiece - lingpiecabouger < 0) .AND. (placementcol2 - placementcol < 0) ) THEN
					DO i=1,abs(lignepiece - lingpiecabouger)
						OPEN(UNIT=10, FILE='liste_piece_blanche.txt', STATUS = 'OLD', ACTION ='READ')
						OPEN(UNIT=20, FILE='liste_piece_noir.txt', STATUS = 'OLD', ACTION ='READ')
						DO w = 1,16
							READ(10,*)nompiece1
							READ(20,*)nompiece2
							IF ( echiquier(lingpiecabouger-i, placementcol-i) == nompiece1 ) THEN
								IF ( i == abs((lignepiece - lingpiecabouger)) ) THEN 
									
								ELSE
									good6='no'
									veriffou(w,1)=good6
									PRINT*,''
									PRINT*," ERREUR : Le fou choisi ne effectuer le mouvement choisi, il y a une pièce blanche sur sa trajectoire."
									PRINT*,"Entrez un mouvement correct."
									PRINT*,''
								ENDIF
							ELSE
								good6='yes'
							ENDIF
							IF ( echiquier(lingpiecabouger-i, placementcol - i) == nompiece2 ) THEN
								IF ( i == abs((lignepiece - lingpiecabouger)) ) THEN 
									
								ELSE
									good6='no'
									veriffou2(w,1)=good6
									PRINT*,''
									PRINT*," Le fou choisi ne effectuer le mouvement choisi, il y a une pièce noire sur sa trajectoire."
									PRINT*,"Entrez un mouvement correct."
									PRINT*,''
								ENDIF
							ELSE
								good6='yes'
							ENDIF
						ENDDO
						CLOSE (UNIT=10)
						CLOSE (UNIT=20)
		
						DO WHILE (veriffou(a,1)=='.')
							IF (a==17) THEN
								EXIT
							ELSE
								a=a+1
							ENDIF
						ENDDO
						DO WHILE (veriffou2(c,1)=='.')
							IF (c==17) THEN
								EXIT
							ELSE
								c=c+1
							ENDIF
						ENDDO
						IF ( a+c-2 == 32 ) THEN
							good='yes'
						ELSE 
							good='no'
							CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
						ENDIF
					ENDDO
				ELSEIF ( (lignepiece - lingpiecabouger > 0) .AND. (placementcol2 - placementcol < 0) ) THEN
					DO i=1,abs(lignepiece - lingpiecabouger)
						OPEN(UNIT=10, FILE='liste_piece_blanche.txt', STATUS = 'OLD', ACTION ='READ')
						OPEN(UNIT=20, FILE='liste_piece_noir.txt', STATUS = 'OLD', ACTION ='READ')
						DO w = 1,16
							READ(10,*)nompiece1
							READ(20,*)nompiece2
							IF ( echiquier(lingpiecabouger+i, placementcol-i) == nompiece1 ) THEN
								IF ( i == abs((lignepiece - lingpiecabouger)) ) THEN 
									
								ELSE
									good6='no'
									veriffou(w,1)=good6
									PRINT*,''
									PRINT*," ERREUR : Le fou choisi ne effectuer le mouvement choisi, il y a une pièce blanche sur sa trajectoire."
									PRINT*,"Entrez un mouvement correct."
									PRINT*,''
								ENDIF
							ELSE
								good6='yes'
							ENDIF
							IF ( echiquier(lingpiecabouger+i, placementcol - i) == nompiece2 ) THEN
								IF ( i == abs((lignepiece - lingpiecabouger)) ) THEN 
									
								ELSE
									good6='no'
									veriffou2(w,1)=good6
									PRINT*,''
									PRINT*," Le fou choisi ne effectuer le mouvement choisi, il y a une pièce noire sur sa trajectoire."
									PRINT*,"Entrez un mouvement correct."
									PRINT*,''
								ENDIF
							ELSE
								good6='yes'
							ENDIF
						ENDDO
						CLOSE (UNIT=10)
						CLOSE (UNIT=20)
						DO WHILE (veriffou(a,1)=='.')
							IF (a==17) THEN
								EXIT
							ELSE
								a=a+1
							ENDIF
						ENDDO
						DO WHILE (veriffou2(c,1)=='.')
							IF (c==17) THEN
								EXIT
							ELSE
								c=c+1
							ENDIF
						ENDDO
						IF ( a+c-2 == 32 ) THEN
							good='yes'
						ELSE 
							good='no'
							CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
						ENDIF
					ENDDO
				ELSEIF ( (lignepiece - lingpiecabouger > 0) .AND. (placementcol2 - placementcol > 0) ) THEN
					DO i=1,abs(lignepiece - lingpiecabouger)
						OPEN(UNIT=10, FILE='liste_piece_blanche.txt', STATUS = 'OLD', ACTION ='READ')
						OPEN(UNIT=20, FILE='liste_piece_noir.txt', STATUS = 'OLD', ACTION ='READ')
						DO w = 1,16
							READ(10,*)nompiece1
							READ(20,*)nompiece2
							IF ( echiquier(lingpiecabouger+i, placementcol+i) == nompiece1 ) THEN
								IF ( i == abs((lignepiece - lingpiecabouger)) ) THEN 
									
								ELSE
									good6='no'
									veriffou(w,1)=good6
									PRINT*,''
									PRINT*," ERREUR : Le fou choisi ne effectuer le mouvement choisi, il y a une pièce blanche sur sa trajectoire."
									PRINT*,"Entrez un mouvement correct."
									PRINT*,''
								ENDIF
							ELSE
								good6='yes'
							ENDIF
							IF ( echiquier(lingpiecabouger+i, placementcol + i) == nompiece2 ) THEN
								IF ( i == abs((lignepiece - lingpiecabouger)) ) THEN 
									
								ELSE
									good6='no'
									veriffou2(w,1)=good6
									PRINT*,''
									PRINT*," Le fou choisi ne effectuer le mouvement choisi, il y a une pièce noire sur sa trajectoire."
									PRINT*,"Entrez un mouvement correct."
									PRINT*,''
								ENDIF
							ELSE
								good6='yes'
							ENDIF
						ENDDO
						CLOSE (UNIT=10)
						CLOSE (UNIT=20)
						DO WHILE (veriffou(a,1)=='.')
							IF (a==17) THEN
								EXIT
							ELSE
								a=a+1
							ENDIF
						ENDDO
						DO WHILE (veriffou2(c,1)=='.')
							IF (c==17) THEN
								EXIT
							ELSE
								c=c+1
							ENDIF
						ENDDO
						IF ( a+c-2 == 32 ) THEN
							good='yes'
						ELSE 
							good='no'
							CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
						ENDIF
					ENDDO
				ENDIF
			ELSE
				good='no'
				PRINT*,''
				PRINT*," ERREUR : Le fou choisi ne peut pas effectuer le mouvement choisi."
				PRINT*,"Règle : Le fou se déplace en diagonale."
				PRINT*,''
				CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
			ENDIF

		ENDIF
	ENDIF

!Mouvement de la Dame

	IF ( (entree == 'DameB') ) THEN
		OPEN(UNIT=10, FILE='liste_piece_blanche.txt', STATUS = 'OLD', ACTION ='READ')
		DO i =1,16
			READ(10,*)nompiece
			IF ( echiquier(lignepiece, placementcol2) == nompiece ) THEN
				good2='no'
				verif(i,1)=good2
				PRINT*,''
				PRINT*," ERREUR : La dame blanche choisi ne peut pas manger une pièce blanche."
				PRINT*,"Entrez un mouvement correct."
				PRINT*,''
			ELSE
				good2='yes'
			ENDIF
		ENDDO
		CLOSE (UNIT=10)
		DO WHILE (verif(u,1)=='.')
			IF (u==17) THEN
				EXIT
			ELSE
				u=u+1
			ENDIF
		ENDDO
		IF ( u-1 == 16 ) THEN
			good7='yes'
		ELSE 
			good7='no'
		ENDIF
	ELSEIF ( (entree == 'DameN')) THEN
		OPEN(UNIT=20, FILE='liste_piece_noir.txt', STATUS = 'OLD', ACTION ='READ')
		DO i =1,16
			READ(20,*)nompiece
			IF ( echiquier(lignepiece, placementcol2) == nompiece ) THEN
				good2='no'
				verif(i,1)=good2
				PRINT*,''
				PRINT*," ERREUR : La dame noire choisi ne peut pas manger une pièce noir."
				PRINT*,"Entrez un mouvement correct."
				PRINT*,''
			ELSE
				good2='yes'
			ENDIF
		ENDDO
		CLOSE (UNIT=20)
		DO WHILE (verif(e,1)=='.')
			IF (e==17) THEN
				EXIT
			ELSE
				e=e+1
			ENDIF
		ENDDO
	
		IF ( e-1 == 16 ) THEN
			good7='yes'
		ELSE 
			good7='no'
		ENDIF
	ENDIF
	IF ((entree == 'DameB') .OR. (entree == 'DameN') ) THEN
		IF ( good7 == 'no' ) THEN 
			good='no'
			CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
		ELSEIF ( good7 == 'yes') THEN
			IF ( abs(lignepiece - lingpiecabouger) == abs(placementcol2 - placementcol) ) THEN  !Déplacement en diagonale 
				IF ( (lignepiece - lingpiecabouger < 0) .AND. (placementcol2 - placementcol > 0) ) THEN
					DO i=1,abs(lignepiece - lingpiecabouger)
						OPEN(UNIT=10, FILE='liste_piece_blanche.txt', STATUS = 'OLD', ACTION ='READ')
						OPEN(UNIT=20, FILE='liste_piece_noir.txt', STATUS = 'OLD', ACTION ='READ')
						DO w = 1,16
							READ(10,*)nompiece1
							READ(20,*)nompiece2
							IF ( echiquier(lingpiecabouger-i, placementcol + i) == nompiece1 ) THEN
								IF ( i == abs((lignepiece - lingpiecabouger)) ) THEN 

								ELSE
									good6='no'
									verifdamed(w,1)=good6
									PRINT*,''
									PRINT*," ERREUR : La dame choisi ne effectuer le mouvement choisi, il y a une pièce blanche sur sa trajectoire."
									PRINT*,"Entrez un mouvement correct."
									PRINT*,''
								ENDIF
							ELSE
								good6='yes'
							ENDIF
							IF ( echiquier(lingpiecabouger-i, placementcol + i) == nompiece2 ) THEN
								IF ( i == abs((lignepiece - lingpiecabouger)) ) THEN 
									
								ELSE
									good6='no'
									verifdamed2(w,1)=good6
									PRINT*,''
									PRINT*," La dame choisi ne effectuer le mouvement choisi, il y a une pièce noire sur sa trajectoire."
									PRINT*,"Entrez un mouvement correct."
									PRINT*,''
								ENDIF
							ELSE
								good6='yes'
							ENDIF
						ENDDO
						CLOSE (UNIT=10)
						CLOSE (UNIT=20)
						DO WHILE (verifdamed(a1,1)=='.')
							IF (a1==17) THEN
								EXIT
							ELSE
								a1=a1+1
							ENDIF
						ENDDO
						DO WHILE (verifdamed2(a2,1)=='.')
							IF (a2==17) THEN
								EXIT
							ELSE
								a2=a2+1
							ENDIF
						ENDDO
						IF ( a1+a2-2 == 32 ) THEN
							good='yes'
						ELSE 
							good='no'
							CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
						ENDIF
					ENDDO
				ELSEIF ( (lignepiece - lingpiecabouger < 0) .AND. (placementcol2 - placementcol < 0) ) THEN
					DO i=1,abs(lignepiece - lingpiecabouger)
						OPEN(UNIT=10, FILE='liste_piece_blanche.txt', STATUS = 'OLD', ACTION ='READ')
						OPEN(UNIT=20, FILE='liste_piece_noir.txt', STATUS = 'OLD', ACTION ='READ')
						DO w = 1,16
							READ(10,*)nompiece1
							READ(20,*)nompiece2
							IF ( echiquier(lingpiecabouger-i, placementcol-i) == nompiece1 ) THEN
								IF ( i == abs((lignepiece - lingpiecabouger)) ) THEN 
									
								ELSE
									good6='no'
									verifdamed(w,1)=good6
									PRINT*,''
									PRINT*," ERREUR : La dame choisi ne effectuer le mouvement choisi, il y a une pièce blanche sur sa trajectoire."
									PRINT*,"Entrez un mouvement correct."
									PRINT*,''
								ENDIF
							ELSE
								good6='yes'
							ENDIF
							IF ( echiquier(lingpiecabouger-i, placementcol - i) == nompiece2 ) THEN
								IF ( i == abs((lignepiece - lingpiecabouger)) ) THEN 
									
								ELSE
									good6='no'
									verifdamed2(w,1)=good6
									PRINT*,''
									PRINT*," La dame choisi ne effectuer le mouvement choisi, il y a une pièce noire sur sa trajectoire."
									PRINT*,"Entrez un mouvement correct."
									PRINT*,''
								ENDIF
							ELSE
								good6='yes'
							ENDIF
						ENDDO
						CLOSE (UNIT=10)
						CLOSE (UNIT=20)
		
						DO WHILE (verifdamed(a3,1)=='.')
							IF (a3==17) THEN
								EXIT
							ELSE
								a3=a3+1
							ENDIF
						ENDDO
						DO WHILE (verifdamed2(a4,1)=='.')
							IF (a4==17) THEN
								EXIT
							ELSE
								a4=a4+1
							ENDIF
						ENDDO
						IF ( a3+a4-2 == 32 ) THEN
							good='yes'
						ELSE 
							good='no'
							CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
						ENDIF
					ENDDO
				
				ELSEIF ( (lignepiece - lingpiecabouger > 0) .AND. (placementcol2 - placementcol < 0) ) THEN
					DO i=1,abs(lignepiece - lingpiecabouger)
						OPEN(UNIT=10, FILE='liste_piece_blanche.txt', STATUS = 'OLD', ACTION ='READ')
						OPEN(UNIT=20, FILE='liste_piece_noir.txt', STATUS = 'OLD', ACTION ='READ')
						DO w = 1,16
							READ(10,*)nompiece1
							READ(20,*)nompiece2
							IF ( echiquier(lingpiecabouger+i, placementcol-i) == nompiece1 ) THEN
								IF ( i == abs((lignepiece - lingpiecabouger)) ) THEN 
									
								ELSE
									good6='no'
									verifdamed(w,1)=good6
									PRINT*,''
									PRINT*," ERREUR : La dame choisi ne effectuer le mouvement choisi, il y a une pièce blanche sur sa trajectoire."
									PRINT*,"Entrez un mouvement correct."
									PRINT*,''
								ENDIF
							ELSE
								good6='yes'
							ENDIF
							IF ( echiquier(lingpiecabouger+i, placementcol - i) == nompiece2 ) THEN
								IF ( i == abs((lignepiece - lingpiecabouger)) ) THEN 
									
								ELSE
									good6='no'
									verifdamed(w,1)=good6
									PRINT*,''
									PRINT*," Le dame choisi ne effectuer le mouvement choisi, il y a une pièce noire sur sa trajectoire."
									PRINT*,"Entrez un mouvement correct."
									PRINT*,''
								ENDIF
							ELSE
								good6='yes'
							ENDIF
						ENDDO
						CLOSE (UNIT=10)
						CLOSE (UNIT=20)
						DO WHILE (verifdamed(a5,1)=='.')
							IF (a5==17) THEN
								EXIT
							ELSE
								a5=a5+1
							ENDIF
						ENDDO
						DO WHILE (verifdamed(a6,1)=='.')
							IF (a6==17) THEN
								EXIT
							ELSE
								a6=a6+1
							ENDIF
						ENDDO
						IF ( a6+a5-2 == 32 ) THEN
							good='yes'
						ELSE 
							good='no'
							CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
						ENDIF
					ENDDO
				ELSEIF ( (lignepiece - lingpiecabouger > 0) .AND. (placementcol2 - placementcol > 0) ) THEN
					DO i=1,abs(lignepiece - lingpiecabouger)
						OPEN(UNIT=10, FILE='liste_piece_blanche.txt', STATUS = 'OLD', ACTION ='READ')
						OPEN(UNIT=20, FILE='liste_piece_noir.txt', STATUS = 'OLD', ACTION ='READ')
						DO w = 1,16
							READ(10,*)nompiece1
							READ(20,*)nompiece2
							IF ( echiquier(lingpiecabouger+i, placementcol+i) == nompiece1 ) THEN
								IF ( i == abs((lignepiece - lingpiecabouger)) ) THEN 
									
								ELSE
									good6='no'
									verifdamed(w,1)=good6
									PRINT*,''
									PRINT*," ERREUR : La dame choisi ne effectuer le mouvement choisi, il y a une pièce blanche sur sa trajectoire."
									PRINT*,"Entrez un mouvement correct."
									PRINT*,''
								ENDIF
							ELSE
								good6='yes'
							ENDIF
							IF ( echiquier(lingpiecabouger+i, placementcol + i) == nompiece2 ) THEN
								IF ( i == abs((lignepiece - lingpiecabouger)) ) THEN 
									
								ELSE
									good6='no'
									verifdamed2(w,1)=good6
									PRINT*,''
									PRINT*," La dame choisi ne effectuer le mouvement choisi, il y a une pièce noire sur sa trajectoire."
									PRINT*,"Entrez un mouvement correct."
									PRINT*,''
								ENDIF
							ELSE
								good6='yes'
							ENDIF
						ENDDO
						CLOSE (UNIT=10)
						CLOSE (UNIT=20)
						DO WHILE (verifdamed(a7,1)=='.')
							IF (a7==17) THEN
								EXIT
							ELSE
								a7=a7+1
							ENDIF
						ENDDO
						DO WHILE (verifdamed2(a8,1)=='.')
							IF (a8==17) THEN
								EXIT
							ELSE
								a8=a8+1
							ENDIF
						ENDDO
						IF ( a8+a7-2 == 32 ) THEN
							good='yes'
						ELSE 
							good='no'
							CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
						ENDIF
					ENDDO
				ENDIF
			ELSEIF ( lingpiecabouger == lignepiece ) THEN 				!Si la ligne est la même la dame change uniquement de colonne!
					!Mouvement possible si aucune piece sur la trajectoire!
					DO i = 1, abs((placementcol2 - placementcol))
						IF ( (placementcol2 - placementcol) < 0 ) THEN  		!Si colonne final - colonne initiale <0 => Droite vers gauche
							OPEN(UNIT=10, FILE='liste_piece_blanche.txt', STATUS = 'OLD', ACTION ='READ')
							OPEN(UNIT=20, FILE='liste_piece_noir.txt', STATUS = 'OLD', ACTION ='READ')
							DO w=1,16
								READ(10,*)nompiece5
								READ(20,*)nompiece6
								IF ( (echiquier(lignepiece, placementcol-i) == nompiece5) ) THEN
									IF ( i == abs((placementcol2 - placementcol)) ) THEN 
									
									ELSE
										good2='no'
										verifdamea(m,1)=good2
										PRINT*,''
										PRINT*, 'ERREUR : La dame que vous avez choisi ne peut pas effectuer un mouvement vers la gauche,'
										PRINT*, 'il y a une pièce sur sa trajectoire.'
										PRINT*, "Veuillez entrer une position d'arrivée différente."
										PRINT*,''
									ENDIF

								ELSE
									good2='yes'
								ENDIF
								IF ( echiquier(lignepiece, placementcol-i) == nompiece6 ) THEN
									IF ( i == abs((placementcol2 - placementcol)) ) THEN 
									
									ELSE
										good2='no'
										verifdamea2(m,1)=good2
										PRINT*,''
										PRINT*, 'ERREUR : La dame que vous avez choisi ne peut pas effectuer un mouvement vers la gauche,'
										PRINT*, 'il y a une pièce sur sa trajectoire.'
										PRINT*, "Veuillez entrer une position d'arrivée différente."
										PRINT*,''
									ENDIF
								ELSE
									good2='yes'
								ENDIF
							ENDDO
							CLOSE (UNIT=10)
							CLOSE (UNIT=20)
							DO WHILE (verifdamea(m,1)=='.')
								IF (m==17) THEN
									EXIT
								ELSE
									m=m+1
								ENDIF
							ENDDO
							DO WHILE (verifdamea2(p,1)=='.')
								IF (p==17) THEN
									EXIT
								ELSE
									p=p+1
								ENDIF
							ENDDO
							IF ( p+m-2 == 32 ) THEN
								good='yes'
							ELSE 
								good='no'
								CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
							ENDIF
						ELSEIF ( (placementcol2 - placementcol) == 0 ) THEN 	!Si colonne final - colonne initiale =0 => même place
							PRINT*,''
							PRINT*, "La dame que vous avez choisi ne peut pas rester à sa même place."
							PRINT*, "Veuillez effectuer un déplacement avec cette pièce."
							PRINT*,''
						ELSEIF ( (placementcol2 - placementcol) > 0 ) THEN  	!Si colonne final - colonne initiale >0 => gauche vers droite
							OPEN(UNIT=10, FILE='liste_piece_blanche.txt', STATUS = 'OLD', ACTION ='READ')
							OPEN(UNIT=20, FILE='liste_piece_noir.txt', STATUS = 'OLD', ACTION ='READ')
							DO w=1,16
								READ(10,*)nompiece5
								READ(20,*)nompiece6
								IF ( (echiquier(lignepiece, placementcol+i) == nompiece5) ) THEN
									IF ( i == abs((placementcol2 - placementcol)) ) THEN 
									
									ELSE
										good2='no'
										verifdamea(m,1)=good2
										PRINT*, ''
										PRINT*, 'ERREUR : La dame que vous avez choisi ne peut pas effectuer de mouvement vers la droite,'
										PRINT*, 'il y a une pièce sur sa trajectoire.'
										PRINT*, "Veuillez entrer une position d'arrivée différente."
										PRINT*,''
									ENDIF
								ELSE
									good2='yes'
								ENDIF
								IF ( echiquier(lignepiece,placementcol+i) == nompiece6 ) THEN
									IF ( i == abs((placementcol2 - placementcol)) ) THEN 
									
									ELSE
										good2='no'
										verifdamea2(m,1)=good2
										PRINT*, ''
										PRINT*, 'ERREUR : La dame que vous avez choisi ne peut pas effectuer de mouvement vers la droite,'
										PRINT*, 'il y a une pièce sur sa trajectoire.'
										PRINT*, "Veuillez entrer une position d'arrivée différente."
										PRINT*,''
									ENDIF
								ELSE
									good2='yes'
								ENDIF
							ENDDO
						CLOSE (UNIT=10)
						CLOSE (UNIT=20)
						DO WHILE (verifdamea(m,1)=='.')
							IF (m==17) THEN
								EXIT
							ELSE
								m=m+1
							ENDIF
						ENDDO
						DO WHILE (verifdamea2(p,1)=='.')
							IF (p==17) THEN
								EXIT
							ELSE
								p=p+1
							ENDIF
						ENDDO
						IF ( p+m-2 == 32 ) THEN
							good='yes'
						ELSE 
							good='no'
							CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
						ENDIF
					ENDIF
				ENDDO
			
			ELSEIF ( placementcol == placementcol2 ) THEN 				!Si la colonne est la même la tour change uniquement de ligne!
				DO i = 1,abs((lignepiece - lingpiecabouger))
					IF ( (lignepiece - lingpiecabouger) < 0 ) THEN 		
						OPEN(UNIT=10, FILE='liste_piece_blanche.txt', STATUS = 'OLD', ACTION ='READ')
						OPEN(UNIT=20, FILE='liste_piece_noir.txt', STATUS = 'OLD', ACTION ='READ')
						DO w=1,16
							READ(10,*)nompiece7
							READ(20,*)nompiece8
							IF ( (echiquier(lingpiecabouger-i,placementcol) == nompiece7) ) THEN
								IF ( i == abs((lignepiece - lingpiecabouger)) ) THEN 
									
								ELSE
									good2='no'
									verifdamea(w,1)=good2
									PRINT*, ''
									PRINT*, 'ERREUR : La dame que vous avez choisi ne peut pas effectuer de mouvement vers le haut,'
									PRINT*, 'il y a une pièce sur sa trajectoire.'
									PRINT*, "Veuillez entrer une position d'arrivée différente."
									PRINT*,''
								ENDIF
							ENDIF
							IF ( echiquier(lingpiecabouger-i,placementcol) == nompiece8 ) THEN
								IF ( i == abs((lignepiece - lingpiecabouger)) ) THEN 
									
								ELSE
									good2='no'
									verifdamea2(w,1)=good2
									PRINT*, ''
									PRINT*, 'ERREUR : La dame que vous avez choisi ne peut pas effectuer de mouvement vers le haut,'
									PRINT*, 'il y a une pièce sur sa trajectoire.'
									PRINT*, "Veuillez entrer une position d'arrivée différente."
									PRINT*,''
								ENDIF
							ENDIF 
						ENDDO
						CLOSE (UNIT=10)
						CLOSE (UNIT=20)
						DO WHILE (verifdamea(m,1)=='.')
							IF (m==17) THEN
								EXIT
							ELSE
								m=m+1
							ENDIF
						ENDDO
						DO WHILE (verifdamea2(p,1)=='.')
							IF (p==17) THEN
								EXIT
							ELSE
								p=p+1
							ENDIF
						ENDDO
						IF ( p+m-2 == 32 ) THEN
							good='yes'
						ELSE 
							good='no'
							CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
						ENDIF
					ELSEIF ( (lignepiece - lingpiecabouger) == 0 ) THEN
						PRINT*,''
						PRINT*, "La dame que vous avez choisi ne peut pas rester à sa même place."
						PRINT*, "Veuillez effectuer un déplacement avec cette pièce."
						PRINT*,''
					ELSEIF ( (lignepiece - lingpiecabouger) > 0 ) THEN !Si ligne final - ligne depart > 0 = descend
						OPEN(UNIT=10, FILE='liste_piece_blanche.txt', STATUS = 'OLD', ACTION ='READ')
						OPEN(UNIT=20, FILE='liste_piece_noir.txt', STATUS = 'OLD', ACTION ='READ')
						DO w=1,16
							READ(10,*)nompiece7
							READ(20,*)nompiece8
							IF ( (echiquier(lingpiecabouger+i,placementcol) == nompiece7) ) THEN
								IF ( i == abs((lignepiece - lingpiecabouger)) ) THEN 
									
								ELSE
									good2='no'
									verifdamea(w,1)=good2
									PRINT*, ''
									PRINT*, 'ERREUR : La dame que vous avez choisi ne peut pas effectuer de mouvement vers le bas,'
									PRINT*, 'il y a une pièce sur sa trajectoire.'
									PRINT*, "Veuillez entrer une position d'arrivée différente."
									PRINT*,''
								ENDIF
			
							ELSE
								good2='yes'
							ENDIF
							IF ( echiquier(lingpiecabouger+i,placementcol) == nompiece8 ) THEN
								IF ( i == abs((lignepiece - lingpiecabouger)) ) THEN 
									
								ELSE
									good2='no'
									verifdamea2(w,1)=good2
									PRINT*, ''
									PRINT*, 'ERREUR : La tour que vous avez choisi ne peut pas effectuer de mouvement vers le bas,'
									PRINT*, 'il y a une pièce sur sa trajectoire.'
									PRINT*, "Veuillez entrer une position d'arrivée différente."
									PRINT*,''
								ENDIF
							ELSE
								good2='yes'
							ENDIF
						ENDDO
						CLOSE (UNIT=10)
						CLOSE (UNIT=20)
						DO WHILE (verifdamea(m,1)=='.')
							IF (m==17) THEN
								EXIT
							ELSE
								m=m+1
							ENDIF
						ENDDO
						DO WHILE (verifdamea2(p,1)=='.')
							IF (p==17) THEN
								EXIT
							ELSE
								p=p+1
							ENDIF
						ENDDO
						IF ( p+m-2 == 32 ) THEN
							good='yes'
						ELSE 
							good='no'
							CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
						ENDIF
					ENDIF
				ENDDO
			ENDIF
		ELSE
				good='no'
				PRINT*,''
				PRINT*," ERREUR : Le dame choisi ne peut pas effectuer le mouvement choisi."
				PRINT*,"Règle : Le dame se déplace en diagonale, selon une ligne ou selon une colonne."
				PRINT*,''
				CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
		ENDIF
	ENDIF


!Mouvement roi noir!

	IF ( (entree == 'RoiN') ) THEN
		IF ( placementcol2 == placementcol ) THEN 
			IF ( (lignepiece == lingpiecabouger +1) .OR. (lignepiece == lingpiecabouger - 1)  ) THEN
				IF ( echiquier(lignepiece,placementcol2) == '.' ) THEN
						good='yes'
				ENDIF
				OPEN(UNIT=20, FILE='liste_piece_noir.txt', STATUS = 'OLD', ACTION ='READ')
				DO i = 1,16
					READ(20,*)nompiece
					IF ( echiquier(lignepiece,placementcol2) == nompiece ) THEN
						good2='no'
						verifroi(i,1)=good2
						PRINT*,'Le roi noir choisi ne peut effectuer le mouvement voulu.'
						PRINT*,"Sa position d'arrivée est une pièce noire."
						PRINT*,''
						PRINT*,'Règle : Le roi noir ne peut manger que des pièces blanches.'
					ENDIF
				ENDDO
				CLOSE (UNIT=20)
				DO WHILE (verifroi(m,1)=='.')
					IF (m==17) THEN
						EXIT
					ELSE
						m=m+1
					ENDIF
				ENDDO
				IF ( m-1 == 16) THEN
					good='yes'
				ELSE 
					good='no'
					CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
				ENDIF
			ELSE
				PRINT*,'Le roi noir choisi ne peut effectuer le mouvement voulu.'
				PRINT*,'Règle : Le roi noir ne peut bouger que d une case dans les 4 directions. Il ne peut bouger en diagonale.'
				PRINT*,''
			ENDIF
		ELSEIF ( lignepiece == lingpiecabouger ) THEN 
			IF ( (placementcol2 == placementcol+1) .OR. (placementcol2 == placementcol-1)) THEN
				IF ( echiquier(lignepiece,placementcol2) == '.' ) THEN
						good='yes'
				ENDIF
				OPEN(UNIT=20, FILE='liste_piece_noir.txt', STATUS = 'OLD', ACTION ='READ')
				DO i = 1,16
					READ(20,*)nompiece
					IF ( echiquier(lignepiece,placementcol2) == nompiece ) THEN
						good2='no'
						verifroi(i,1)=good2
						PRINT*,'Le roi noir choisi ne peut effectuer le mouvement voulu.'
						PRINT*,"Sa position d'arrivée est une pièce noire."
						PRINT*,''
						PRINT*,'Règle : Le roi noir ne peut manger que des pièces blanches.'
					ENDIF
				ENDDO
				CLOSE (UNIT=20)
				DO WHILE (verifroi(m,1)=='.')
					IF (m==17) THEN
						EXIT
					ELSE
						m=m+1
					ENDIF
				ENDDO
				IF ( m-1 == 16) THEN
					good='yes'
				ELSE 
					good='no'
					CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
				ENDIF
			ELSE 
				PRINT*,'Le roi noir choisi ne peut effectuer le mouvement voulu.'
				PRINT*,'Règle : Le roi noir ne peut bouger que d une case dans les 4 directions. Il ne peut bouger en diagonale.'
				PRINT*,''
			ENDIF
		
		ELSEIF ( (lignepiece == lingpiecabouger+1) .AND. (placementcol2 == placementcol+1) ) THEN
				IF ( echiquier(lignepiece,placementcol2) == '.' ) THEN
						good='yes'
				ENDIF
				OPEN(UNIT=20, FILE='liste_piece_noir.txt', STATUS = 'OLD', ACTION ='READ')
				DO i = 1,16
					READ(20,*)nompiece
					IF ( echiquier(lignepiece,placementcol2) == nompiece ) THEN
						good2='no'
						verifroi(i,1)=good2
						PRINT*,'Le roi noir choisi ne peut effectuer le mouvement voulu.'
						PRINT*,"Sa position d'arrivée est une pièce noire."
						PRINT*,''
						PRINT*,'Règle : Le roi noir ne peut manger que des pièces blanches.'
					ENDIF
				ENDDO
				CLOSE (UNIT=20)
				DO WHILE (verifroi(m,1)=='.')
					IF (m==17) THEN
						EXIT
					ELSE
						m=m+1
					ENDIF
				ENDDO
				IF ( m-1 == 16) THEN
					good='yes'
				ELSE 
					good='no'
					CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
				ENDIF

		ELSEIF ( (lignepiece == lingpiecabouger-1) .AND. (placementcol2 == placementcol+1) ) THEN
				IF ( echiquier(lignepiece,placementcol2) == '.' ) THEN
						good='yes'
				ENDIF
				OPEN(UNIT=20, FILE='liste_piece_noir.txt', STATUS = 'OLD', ACTION ='READ')
				DO i = 1,16
					READ(20,*)nompiece
					IF ( echiquier(lignepiece,placementcol2) == nompiece ) THEN
						good2='no'
						verifroi(i,1)=good2
						PRINT*,'Le roi noir choisi ne peut effectuer le mouvement voulu.'
						PRINT*,"Sa position d'arrivée est une pièce noire."
						PRINT*,''
						PRINT*,'Règle : Le roi noir ne peut manger que des pièces blanches.'
					ENDIF
				ENDDO
				CLOSE (UNIT=20)
				DO WHILE (verifroi(m,1)=='.')
					IF (m==17) THEN
						EXIT
					ELSE
						m=m+1
					ENDIF
				ENDDO
				IF ( m-1 == 16) THEN
					good='yes'
				ELSE 
					good='no'
					CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
				ENDIF

		ELSEIF ( (lignepiece == lingpiecabouger-1) .AND. (placementcol2 == placementcol-1) ) THEN
				IF ( echiquier(lignepiece,placementcol2) == '.' ) THEN
						good='yes'
				ENDIF
				OPEN(UNIT=20, FILE='liste_piece_noir.txt', STATUS = 'OLD', ACTION ='READ')
				DO i = 1,16
					READ(20,*)nompiece
					IF ( echiquier(lignepiece,placementcol2) == nompiece ) THEN
						good2='no'
						verifroi(i,1)=good2
						PRINT*,'Le roi noir choisi ne peut effectuer le mouvement voulu.'
						PRINT*,"Sa position d'arrivée est une pièce noire."
						PRINT*,''
						PRINT*,'Règle : Le roi noir ne peut manger que des pièces blanches.'
					ENDIF
				ENDDO
				CLOSE (UNIT=20)
				DO WHILE (verifroi(m,1)=='.')
					IF (m==17) THEN
						EXIT
					ELSE
						m=m+1
					ENDIF
				ENDDO
				IF ( m-1 == 16) THEN
					good='yes'
				ELSE 
					good='no'
					CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
				ENDIF

		ELSEIF ( (lignepiece == lingpiecabouger+1) .AND. (placementcol2 == placementcol-1) ) THEN
				IF ( echiquier(lignepiece,placementcol2) == '.' ) THEN
						good='yes'
				ENDIF
				OPEN(UNIT=20, FILE='liste_piece_noir.txt', STATUS = 'OLD', ACTION ='READ')
				DO i = 1,16
					READ(20,*)nompiece
					IF ( echiquier(lignepiece,placementcol2) == nompiece ) THEN
						good2='no'
						verifroi(i,1)=good2
						PRINT*,'Le roi noir choisi ne peut effectuer le mouvement voulu.'
						PRINT*,"Sa position d'arrivée est une pièce noire."
						PRINT*,''
						PRINT*,'Règle : Le roi noir ne peut manger que des pièces blanches.'
					ENDIF
				ENDDO
				CLOSE (UNIT=20)
				DO WHILE (verifroi(m,1)=='.')
					IF (m==17) THEN
						EXIT
					ELSE
						m=m+1
					ENDIF
				ENDDO
				IF ( m-1 == 16) THEN
					good='yes'
				ELSE 
					good='no'
					CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
				ENDIF
		ELSE
			PRINT*,'Le roi noir choisi ne peut effectuer le mouvement voulu.'
			PRINT*,'Règle : Le roi noir ne peut bouger que d une case dans les 4 directions. Il ne peut bouger en diagonale.'
			PRINT*,''
		ENDIF
	ENDIF


!Mouvement roi blanc!	 
	IF ( (entree == 'RoiB') ) THEN
		IF ( placementcol2 == placementcol ) THEN 
			IF ( (lignepiece == lingpiecabouger +1) .OR. (lignepiece == lingpiecabouger - 1)  ) THEN
				IF ( echiquier(lignepiece,placementcol2) == '.' ) THEN
						good='yes'
				ENDIF
				OPEN(UNIT=10, FILE='liste_piece_blanche.txt', STATUS = 'OLD', ACTION ='READ')
				DO i = 1,16
					READ(10,*)nompiece
					IF ( echiquier(lignepiece,placementcol2) == nompiece ) THEN
						good2='no'
						verifroi(i,1)=good2
						PRINT*,'Le roi blanc choisi ne peut effectuer le mouvement voulu.'
						PRINT*,"Sa position d'arrivée est une pièce blanche."
						PRINT*,''
						PRINT*,'Règle : Le roi blanc ne peut manger que des pièces noires.'
					ENDIF
				ENDDO
				CLOSE (UNIT=10)

				DO WHILE (verifroi(m,1)=='.')
					IF (m==17) THEN
						EXIT
					ELSE
						m=m+1
					ENDIF
				ENDDO

				IF ( m-1 == 16) THEN
					good='yes'
				ELSE 
					good='no'
					CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
				ENDIF
			ELSE
				PRINT*,'Le roi blanc choisi ne peut effectuer le mouvement voulu.'
				PRINT*,'Règle : Le roi ne peut bouger que d une case dans les 4 directions. Il ne peut bouger en diagonale.'
				PRINT*,''
			ENDIF
		ELSEIF ( lignepiece == lingpiecabouger ) THEN 
			IF ( (placementcol2 == placementcol+1) .OR. (placementcol2 == placementcol-1)) THEN
				IF ( echiquier(lignepiece,placementcol2) == '.' ) THEN
						good='yes'
				ENDIF
				OPEN(UNIT=10, FILE='liste_piece_blanche.txt', STATUS = 'OLD', ACTION ='READ')
				DO i = 1,16
					READ(10,*)nompiece
					IF ( echiquier(lignepiece,placementcol2) == nompiece ) THEN
						good2='no'
						verifroi(i,1)=good2
						PRINT*,'Le roi blanc choisi ne peut effectuer le mouvement voulu.'
						PRINT*,"Sa position d'arrivée est une pièce noir."
						PRINT*,''
						PRINT*,'Règle : Le roi blanc ne peut manger que des pièces noires.'
					ENDIF
				ENDDO
				CLOSE (UNIT=10)
				DO WHILE (verifroi(m,1)=='.')
					IF (m==17) THEN
						EXIT
					ELSE
						m=m+1
					ENDIF
				ENDDO
				IF ( m-1 == 16) THEN
					good='yes'
				ELSE 
					good='no'
					CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
				ENDIF
			ELSE 
				PRINT*,'Le roi blanc choisi ne peut effectuer le mouvement voulu.'
				PRINT*,'Règle : Le roi noir ne peut bouger que d une case dans les 4 directions. Il ne peut bouger en diagonale.'
				PRINT*,''
			ENDIF
		ELSEIF ( (lignepiece == lingpiecabouger+1) .AND. (placementcol2 == placementcol+1) ) THEN
				IF ( echiquier(lignepiece,placementcol2) == '.' ) THEN
						good='yes'
				ENDIF
				OPEN(UNIT=10, FILE='liste_piece_blanche.txt', STATUS = 'OLD', ACTION ='READ')
				DO i = 1,16
					READ(10,*)nompiece
					IF ( echiquier(lignepiece,placementcol2) == nompiece ) THEN
						good2='no'
						verifroi(i,1)=good2
						PRINT*,'Le roi blanc choisi ne peut effectuer le mouvement voulu.'
						PRINT*,"Sa position d'arrivée est une pièce blanche."
						PRINT*,''
						PRINT*,'Règle : Le roi blanc ne peut manger que des pièces noires.'
					ENDIF
				ENDDO
				CLOSE (UNIT=10)
				DO WHILE (verifroi(m,1)=='.')
					IF (m==17) THEN
						EXIT
					ELSE
						m=m+1
					ENDIF
				ENDDO
				IF ( m-1 == 16) THEN
					good='yes'
				ELSE 
					good='no'
					CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
				ENDIF

		ELSEIF ( (lignepiece == lingpiecabouger-1) .AND. (placementcol2 == placementcol+1) ) THEN
				IF ( echiquier(lignepiece,placementcol2) == '.' ) THEN
						good='yes'
				ENDIF
				OPEN(UNIT=10, FILE='liste_piece_blanche.txt', STATUS = 'OLD', ACTION ='READ')
				DO i = 1,16
					READ(10,*)nompiece
					IF ( echiquier(lignepiece,placementcol2) == nompiece ) THEN
						good2='no'
						verifroi(i,1)=good2
						PRINT*,'Le roi blanc choisi ne peut effectuer le mouvement voulu.'
						PRINT*,"Sa position d'arrivée est une pièce blanche."
						PRINT*,''
						PRINT*,'Règle : Le roi blanc ne peut manger que des pièces noires.'
					ENDIF
				ENDDO
				CLOSE (UNIT=10)
				DO WHILE (verifroi(m,1)=='.')
					IF (m==17) THEN
						EXIT
					ELSE
						m=m+1
					ENDIF
				ENDDO
				IF ( m-1 == 16) THEN
					good='yes'
				ELSE 
					good='no'
					CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
				ENDIF

		ELSEIF ( (lignepiece == lingpiecabouger-1) .AND. (placementcol2 == placementcol-1) ) THEN
				IF ( echiquier(lignepiece,placementcol2) == '.' ) THEN
						good='yes'
				ENDIF
				OPEN(UNIT=10, FILE='liste_piece_blanche.txt', STATUS = 'OLD', ACTION ='READ')
				DO i = 1,16
					READ(10,*)nompiece
					IF ( echiquier(lignepiece,placementcol2) == nompiece ) THEN
						good2='no'
						verifroi(i,1)=good2
						PRINT*,'Le roi blanc choisi ne peut effectuer le mouvement voulu.'
						PRINT*,"Sa position d'arrivée est une pièce blanche."
						PRINT*,''
						PRINT*,'Règle : Le roi blanc ne peut manger que des pièces noires.'
					ENDIF
				ENDDO
				CLOSE (UNIT=10)
				DO WHILE (verifroi(m,1)=='.')
					IF (m==17) THEN
						EXIT
					ELSE
						m=m+1
					ENDIF
				ENDDO
				IF ( m-1 == 16) THEN
					good='yes'
				ELSE 
					good='no'
					CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
				ENDIF

		ELSEIF ( (lignepiece == lingpiecabouger+1) .AND. (placementcol2 == placementcol-1) ) THEN
				IF ( echiquier(lignepiece,placementcol2) == '.' ) THEN
						good='yes'
				ENDIF
				OPEN(UNIT=10, FILE='liste_piece_blanche.txt', STATUS = 'OLD', ACTION ='READ')
				DO i = 1,16
					READ(10,*)nompiece
					IF ( echiquier(lignepiece,placementcol2) == nompiece ) THEN
						good2='no'
						verifroi(i,1)=good2
						PRINT*,'Le roi blanc choisi ne peut effectuer le mouvement voulu.'
						PRINT*,"Sa position d'arrivée est une pièce blanc."
						PRINT*,''
						PRINT*,'Règle : Le roi blanc ne peut manger que des pièces noires.'
					ENDIF
				ENDDO
				CLOSE (UNIT=10)
				DO WHILE (verifroi(m,1)=='.')
					IF (m==17) THEN
						EXIT
					ELSE
						m=m+1
					ENDIF
				ENDDO
				IF ( m-1 == 16) THEN
					good='yes'
				ELSE 
					good='no'
					CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
				ENDIF
		ELSE
			PRINT*,'Le roi blanc choisi ne peut effectuer le mouvement voulu.'
			PRINT*,'Règle : Le roi blanc ne peut bouger que d une case dans les 4 directions. Il ne peut pas bouger en diagonale.'
			PRINT*,''
			CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
		ENDIF
	ENDIF



	END DO
!Fin de boucle TANT QUE pour le mouvement de la pièce!



!COMPTEUR DE PIECES MANGE!

!PAR LES BLANCS!	
	IF (changepion == 'ok') THEN
		
	ELSE
		OPEN(UNIT=10, FILE='liste_piece_noir.txt')
		DO w=1,16
			READ(10,*)nompiece9
			IF (echiquier(lignepiece,placementcol2)==nompiece9) THEN
				mangeblanc(1,compteurb)=nompiece9
				compteurb=compteurb+1
			END IF
		ENDDO
		CLOSE (UNIT=10)
	ENDIF

!PAR LES NOIRS!	
	IF (changepion == 'ok') THEN
		
	ELSE
		OPEN(UNIT=20, FILE='liste_piece_blanche.txt')
		DO w=1,16
			READ(20,*)nompiece10
			IF (echiquier(lignepiece,placementcol2)==nompiece10) THEN !S il y a une pièce blanche placement d'arrivée!
				mangenoir(1,compteurn)=nompiece10
				compteurn=compteurn+1
			END IF
		ENDDO
		CLOSE (UNIT=20)
	ENDIF

	IF (changepion == 'ok') THEN
		
	ELSE
		IF (good=='yes') THEN
			echiquier(lignepiece,placementcol2)=echiquier(lingpiecabouger,placementcol)
			echiquier(lingpiecabouger,placementcol)='.'
		ENDIF
	ENDIF
PRINT*,''
PRINT*,''

	DO i=1,20
		IF ( mangenoir(i,1) == 'RoiB' ) THEN
			ENDGAME=1
			GAGNE=1
		ENDIF
		IF ( mangeblanc(i,1) == 'RoiN') THEN
			ENDGAME=1
			GAGNE=0
		ENDIF
	ENDDO

PRINT*," CLIQUER ENTRER POUR CONTINUER"
READ(*,*)
CALL execute_command_line('clear')

	
!AFFICHAGE!

	DO i=1,9
		PRINT*, echiquier(i,:)
	ENDDO

	PRINT*,''
	PRINT*,'Pièce mangé par les noirs :' 
	PRINT*, mangenoir(1,:)
	PRINT*,''
	PRINT*,'Pièce mangé par les blancs :'
	PRINT*, mangeblanc(1,:)

ENDDO 
CALL execute_command_line('clear')
CALL system_clock(t2bis)

duree=(t2bis-t1bis)/1000

PRINT*,'FIN DE LA PARTIE EN MODE NORMAL!'
PRINT*,''

IF ( GAGNE == 0) THEN !blanc gagne
	PRINT*,'BRAVO !'
	PRINT*,trim(blanc)," GAGNE !"
	PRINT*,'' 
	PRINT*,trim(noir),", DOmmage pour toi..."
	PRINT*,''
	PRINT*," Tu le battras la prochaine fois !! ;))"
ELSEIF (GAGNE == 1) THEN  !noir gagne
	PRINT*,'BRAVO !'
	PRINT*,noir," GAGNE !"
	PRINT*,'' 
	PRINT*,blanc,", DOmmage pour toi..."
	PRINT*," Tu le battras la prochaine fois !! ;))"
ENDIF

PRINT*,''
tour=tour+1
PRINT*,'La partie a duré :', duree ,'secondes,'," et n'a compté pas moins de ", tour, "coups joués !"
PRINT*,''

CALL ecriture(duree,tour,'high_scoreMN.txt')




!MODE RAPIDE!


ELSEIF ( (mode == 'R') .OR. (mode == 'r') .OR. (mode == 'Rapide') .OR. (mode == 'rapide') .OR. (mode == 'RAPIDE') ) THEN
tour=0
CALL system_clock(t1bis) !Instant de début de partie
	DO WHILE (ENDGAME == 0)
	tour=tour + 1
	PRINT*,''
	PRINT*,''
	tour2=tour/2
	IF ( tour2 * 2 == tour) THEN
		PRINT*,'Au tour de ',trim(noir), ' (Les noirs)'
	ELSEIF ( tour2 * 2 +1 == tour) THEN
		PRINT*,'Au tour de ',trim(blanc), ' (Les blancs)'
	ENDIF
	PRINT*,''
	good='no'
	changepion='no'
	bouger='no'
	CALL system_clock(ttour)
	IF ( (ttour - t1bis)/1000 < dureepartie ) THEN
		!Tout va bien
	ELSEIF ( (ttour - t1bis)/1000 > dureepartie ) THEN
		ENDGAME = 1
		good='yes'
		PRINT*, 'LE TEMPS DE PARTIE INDIQUÉ AU DEBUT DE LA PARTIE EST ÉCOULÉ.'
		PRINT*,' Les ', dureepartie, ' ont été dépassé.'
		PRINT*,''
	ENDIF
	DO WHILE (good=='no') 
		CALL system_clock(tinitial)
		bouger='no'
		DO WHILE ( bouger =='no')
			PRINT*,"Entrez la colonne de la pièce à bouger (exemple RoiN = E) :"
			READ(*,*)colpiecabouger

			PRINT*,"Entrez la ligne de la pièce à bouger (exemple RoiN = 8) :"
			READ(*,*)lingpiecabouger

			PRINT*,''
			PRINT*,''

			!Entrée du placement d'arrivée de la pièce!	
			PRINT*,"Entrez la colonne du placement de la pièce voulue :"
			READ(*,*)colpiece

			PRINT*,"Entrez la ligne du placement de la pièce voulue :"
			READ(*,*)lignepiece
			
			CALL place(colpiecabouger,placementcol)
			entree=echiquier(lingpiecabouger,placementcol)
			CALL place(colpiece,placementcol2)

			DO i=1,16
				verifmvt(i,1)='.'
				verifmvt2(i,1)='.'
			ENDDO
			comp=1
			tour2=tour/2
			IF ( tour2 * 2 == tour) THEN !TOUR NOIR!
				OPEN(UNIT=10, FILE='liste_piece_blanche.txt', STATUS = 'OLD', ACTION ='READ')
				DO i = 1,16
					READ(10,*)nompiece11
					IF ( entree == nompiece11) THEN
						bouger2='no'
						verifmvt(i,1)=bouger2
					ENDIF
				ENDDO
				CLOSE(UNIT=10)

				DO WHILE (verifmvt(comp,1)=='.')
					IF (comp==17) THEN
						EXIT
					ELSE
						comp=comp+1
					ENDIF
				ENDDO
				IF ( comp-1 == 16 ) THEN
					bouger='yes'
				ELSE
					bouger='no'
					PRINT*,''
					PRINT*,"Erreur : C'est au tour des noirs !"
					PRINT*,''
					CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
				ENDIF
			ELSEIF ( tour2 * 2 +1 == tour) THEN !TOUR BLANC!
				OPEN(UNIT=20, FILE='liste_piece_noir.txt', STATUS = 'OLD', ACTION ='READ')
				DO i = 1,16
					READ(20,*)nompiece11
					IF ( entree == nompiece11) THEN
						bouger2='no'
						verifmvt2(i,1)=bouger2
					ENDIF
				ENDDO
				CLOSE(UNIT=20)
				DO WHILE (verifmvt2(comp,1)=='.')
					IF (comp==17) THEN
						EXIT
					ELSE
						comp=comp+1
					ENDIF
				ENDDO
				IF ( comp-1 == 16 ) THEN
					bouger='yes'
				ELSE
					bouger='no'
					PRINT*,''
					PRINT*,"Erreur : C'est au tour des blancs !"
					PRINT*,''
					CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
				ENDIF
			ENDIF
		ENDDO
CALL system_clock(tfinal)
IF ( (tfinal - tinitial)/1000 < dureespecifique ) THEN
	goodrapide='yes'
	m=1
	p=1
	q=1
	r=1
	s=1
	t=1
	u=1
	a=1
	c=1
	e=1
	a1=1
	a2=1
	a3=1
	a4=1
	a5=1
	a6=1
	a7=1
	a8=1

	DO i=1,16
		verifgood(i,1)='.'
		verifgood2(i,1)='.'
	ENDDO

	DO i=1,16
		verif(i,1)='.'
		verif2(i,1)='.'
	ENDDO
	
	DO i=1,16
		verifcavalier(i,1)='.'
		verifcavalier2(i,1)='.'
	ENDDO

	DO i=1,16
		veriffou(i,1)='.'
		veriffou2(i,1)='.'
	ENDDO
	
	DO i=1,16
		verifdamea(i,1)='.'
		verifdamea2(i,1)='.'
	ENDDO

	DO i=1,16
		verifdamed(i,1)='.'
		verifdamed2(i,1)='.'
	ENDDO
	
	DO i=1,16
		verifpion(i,1)='.'
		verifpion2(i,1)='.'
	ENDDO
	
	DO i=1,16
		verifroi(i,1)='.'
	ENDDO




!TEST CASE VIDE ?!
		IF (entree == '.') THEN
			good='no'
			PRINT*, 'ERREUR : La case sélectionée ne détient aucune pièce à bouger. Veuillez entrer une case contenant une piece.' 
			PRINT*, ''
			CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,tour,blanc,noir)
		ENDIF 



!MOUVEMENT PIONS BLANCS!
			OPEN(UNIT=30, FILE='liste_pions_blanc.txt', STATUS = 'OLD', ACTION ='READ')
			DO w=1,8
				READ(30,*)nompiece
				IF ( entree == nompiece ) THEN
					IF ( lignepiece == lingpiecabouger + 1) THEN	
						IF ( (placementcol2 ==  placementcol) ) THEN
							IF ( echiquier(lignepiece, placementcol2) == '.') THEN
								good='yes'
							ELSE
								good='no'
								PRINT*,' ERREUR : Le pion blanc ne peut pas avancer. Il y a une pièce devant lui.'
								PRINT*, 'Entrer un mouvement correct.'
								PRINT*, ''
								PRINT*," Règle : Le pion se déplace vers l'avant d'une seule case."	
								PRINT*,''						
							ENDIF
						ELSEIF ( (placementcol2 ==  placementcol-1) .OR. (placementcol2 ==  placementcol+1) ) THEN
							OPEN(UNIT=10, FILE='liste_piece_blanche.txt', STATUS = 'OLD', ACTION ='READ')
							DO i=1,16
								READ(10,*)nompiece2
								IF ( echiquier(lignepiece, placementcol2) == nompiece2 ) THEN
									good2='no'
									verifpion(i,1)=good2
									PRINT*, "Le pion peut manger uniquement sur sa diagonale d'une seule case."
								ELSEIF ( echiquier(lignepiece, placementcol2) == '.' ) THEN
									good2='no'
									verifpion(i,1)=good2
								ELSE
									good2='yes'
								ENDIF
									
							ENDDO
							CLOSE (UNIT=10)
							DO WHILE (verifpion(r,1)=='.')
								IF (r==17) THEN
									EXIT
								ELSE
									r=r+1
								ENDIF
							ENDDO
							IF ( r-1 == 16 ) THEN
								good='yes'
							ELSE
								good='no'
								PRINT*,'ERREUR, le pion ne peut effectuer le mouvement voulu.'
								PRINT*,''
								CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
							ENDIF
						ELSE
							good='no' 
							PRINT*, 'Le pion ne peut pas effectuer le mouvement voulu.'
							PRINT*," Règle : Le pion se déplace vers l'avant d'une seule case."	
							PRINT*,''
							CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
						ENDIF
						
					ELSE 
						PRINT*, ' ERREUR :  Le pion choisi ne peut pas effectuer le déplacement voulu.'
						PRINT*,''
						good='no'
						PRINT*, " Règle : Le pion se déplace vers l'avant d'une seule case et peut manger sur sa diagonale d'une seule case aussi."
						PRINT*,''
						CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
					ENDIF
				END IF
			ENDDO
		CLOSE (UNIT=30) 

 
!MOUVEMENT PION NOIRS!
		OPEN(UNIT=40, FILE='liste_pions_noir.txt', STATUS = 'OLD', ACTION ='READ')
			DO w=1,8
				READ(40,*)nompiece
				IF ( entree == nompiece ) THEN
					IF ( lignepiece == lingpiecabouger - 1) THEN	
						IF ( (placementcol2 ==  placementcol) ) THEN
							IF ( echiquier(lignepiece, placementcol2) == '.') THEN
								good='yes'
							ELSE
								good='no'
								PRINT*,' ERREUR : Le pion noir ne peut pas avancer. Il y a une pièce devant lui.'
								PRINT*, 'Entrer un mouvement correct.'
								PRINT*, ''
								PRINT*," Règle : Le pion se déplace vers l'avant d'une seule case."		
								PRINT*,''					
							ENDIF
						ELSEIF ( (placementcol2 ==  placementcol-1) .OR. (placementcol2 ==  placementcol+1) ) THEN
							OPEN(UNIT=20, FILE='liste_piece_noir.txt', STATUS = 'OLD', ACTION ='READ')
							DO i=1,16
								READ(20,*)nompiece2
								IF ( echiquier(lignepiece, placementcol2) == nompiece2 ) THEN
									good2='no'
									verifpion(i,1)=good2
									PRINT*, "Le pion peut manger uniquement sur sa diagonale d'une seule case."
									PRINT*,''
								ELSEIF ( echiquier(lignepiece, placementcol2) == '.' ) THEN
									good2='no'
									verifpion(i,1)=good2
								ELSE
									good2='yes'
								ENDIF
									
							ENDDO
							CLOSE (UNIT=20)
							DO WHILE (verifpion(r,1)=='.')
								IF (r==17) THEN
									EXIT
								ELSE
									r=r+1
								ENDIF
							ENDDO
							IF ( r-1 == 16 ) THEN
								good='yes'
							ELSE
								good='no'
								PRINT*, "ERREUR, le pion chosi ne peut effectuer ce que vous voulez."
								PRINT*,''
								CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
							ENDIF
						ELSE
							good='no' 
							PRINT*, 'Le pion ne peut pas effectuer le mouvement voulu.'
							PRINT*," Règle : Le pion se déplace vers l'avant d'une seule case."	
							PRINT*,''
						ENDIF
					ELSE 
						PRINT*, ' ERREUR :  Le pion choisi ne peut pas effectuer le déplacement voulu.'
						PRINT*,''
						good='no'
						PRINT*, " Règle : Le pion se déplace vers l'avant d'une seule case et peut manger sur sa diagonale d'une seule case aussi."
						PRINT*,''
						CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
					ENDIF
				END IF
			ENDDO
		CLOSE (UNIT=40) 



	OPEN(UNIT=40, FILE='liste_pions_noir.txt', STATUS = 'OLD', ACTION ='READ')
			DO w=1,8
				READ(40,*)nompiece
				IF ( entree == nompiece ) THEN
					IF ( lignepiece == 1) THEN
						CALL pionnoir(piecesortie)
						echiquier(lignepiece,placementcol2)= piecesortie
						echiquier(lingpiecabouger,placementcol)='.'
						changepion='ok'
					ENDIF
				ENDIF
			ENDDO
	CLOSE(unit=40)

	OPEN(UNIT=30, FILE='liste_pions_blanc.txt', STATUS = 'OLD', ACTION ='READ')		
			DO w=1,8
				READ(30,*)nompiece
				IF ( entree == nompiece ) THEN
					IF ( lignepiece == 8) THEN
						CALL pionblanc(piecesortie)
						echiquier(lignepiece,placementcol2)= piecesortie
						echiquier(lingpiecabouger,placementcol)='.'
						changepion='ok'
					ENDIF
				ENDIF
			ENDDO
	CLOSE(unit=30)


				

!Mouvement Tour!
			OPEN(UNIT=10, FILE='liste_piece_blanche.txt', STATUS = 'OLD', ACTION ='READ')
			OPEN(UNIT=20, FILE='liste_piece_noir.txt', STATUS = 'OLD', ACTION ='READ')
			DO w=1,16
				READ(10,*)nompiece7
				READ(20,*)nompiece8
				IF ( (entree == 'TourB1') .OR. (entree == 'TourB2') ) THEN 
!tour blanche ne peut pas manger de piece blanc (ICI VERIFICATION DE SI C'EST UNE PIECE BLANCHE)!
						IF ( (echiquier(lignepiece,placementcol2) == nompiece7) ) THEN !si le placement d'arrivée est une piece blanche
							good2='no'
							verif(w,1)=good2
							PRINT*,''
							PRINT*, 'ERREUR : Une tour blanche ne peut pas manger une pièce blanche.'
							PRINT*,''
							
						ELSE
							good2='yes'
						ENDIF
				ELSEIF ( (entree == 'TourN1') .OR. (entree == 'TourN2') ) THEN 		
!tour noir ne peut pas manger de piece blanc (ICI VERIFICATION DE SI C'EST UNE TOUR NOIR)!
						IF ( (echiquier(lignepiece,placementcol2) == nompiece8) ) THEN !si le placement d'arrivée est une piece Noire
							good2='no'
							verif2(w,1)=good2
							PRINT*,''
							PRINT*, 'ERREUR : Une tour noire ne peut pas manger une pièce noire.'
							PRINT*,''
						ELSE
							good2='yes'
						ENDIF
					ENDIF
				ENDDO
				CLOSE (UNIT=10)
				CLOSE (UNIT=20)
				DO WHILE (verif(r,1)=='.')
					IF (r==17) THEN
						EXIT
					ELSE
						r=r+1
					ENDIF
				ENDDO
				DO WHILE (verif2(s,1)=='.')
					IF (s==17) THEN
						EXIT
					ELSE
						s=s+1
					ENDIF
				ENDDO
				IF ( s+r-2 == 32 ) THEN
					good3='yes'
				ELSE 
					good3='no'
				ENDIF
	IF ( good3 == 'no') THEN
		good='no'
		CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
	ELSEIF ( good3 == 'yes') THEN
		IF ( (entree == 'TourB1') .OR. (entree == 'TourB2') .OR. (entree == 'TourN1') .OR. (entree == 'TourN2') ) THEN
			IF ( lingpiecabouger == lignepiece ) THEN 				!Si la ligne est la même la tour change uniquement de colonne!
			!Mouvement possible si aucune piece sur la trajectoire!
				DO i = 1, abs((placementcol2 - placementcol))
					IF ( (placementcol2 - placementcol) < 0 ) THEN  		!Si colonne final - colonne initiale <0 => Droite vers gauche
						OPEN(UNIT=10, FILE='liste_piece_blanche.txt', STATUS = 'OLD', ACTION ='READ')
						OPEN(UNIT=20, FILE='liste_piece_noir.txt', STATUS = 'OLD', ACTION ='READ')
						DO w=1,16
							READ(10,*)nompiece5
							READ(20,*)nompiece6
							IF ( (echiquier(lignepiece, placementcol-i) == nompiece5) ) THEN
								IF ( i == abs((placementcol2 - placementcol)) ) THEN 
									
								ELSE
									good2='no'
									verifgood(m,1)=good2
									PRINT*,''
									PRINT*, 'ERREUR : La tour que vous avez choisi ne peut pas effectuer un mouvement vers la gauche,'
									PRINT*, 'il y a une pièce sur sa trajectoire.'
									PRINT*, "Veuillez entrer une position d'arrivée différente."
									PRINT*,''
								ENDIF

							ELSE
								good2='yes'
							ENDIF
							IF ( echiquier(lignepiece, placementcol-i) == nompiece6 ) THEN
								IF ( i == abs((placementcol2 - placementcol)) ) THEN 
									
								ELSE
									good2='no'
									verifgood2(m,1)=good2
									PRINT*,''
									PRINT*, 'ERREUR : La tour que vous avez choisi ne peut pas effectuer un mouvement vers la gauche,'
									PRINT*, 'il y a une pièce sur sa trajectoire.'
									PRINT*, "Veuillez entrer une position d'arrivée différente."
									PRINT*,''
								ENDIF
							ELSE
								good2='yes'
							ENDIF
						ENDDO
						CLOSE (UNIT=10)
						CLOSE (UNIT=20)
						DO WHILE (verifgood(m,1)=='.')
							IF (m==17) THEN
								EXIT
							ELSE
								m=m+1
							ENDIF
						ENDDO
						DO WHILE (verifgood2(p,1)=='.')
							IF (p==17) THEN
								EXIT
							ELSE
								p=p+1
							ENDIF
						ENDDO
						IF ( p+m-2 == 32 ) THEN
							good='yes'
						ELSE 
							good='no'
							CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
						ENDIF
					ELSEIF ( (placementcol2 - placementcol) == 0 ) THEN 	!Si colonne final - colonne initiale =0 => même place
						PRINT*,''
						PRINT*, "La tour que vous avez choisi ne peut pas rester à sa même place."
						PRINT*, "Veuillez effectuer un déplacement avec cette pièce."
						PRINT*,''
					ELSEIF ( (placementcol2 - placementcol) > 0 ) THEN  	!Si colonne final - colonne initiale >0 => gauche vers droite
						OPEN(UNIT=10, FILE='liste_piece_blanche.txt', STATUS = 'OLD', ACTION ='READ')
						OPEN(UNIT=20, FILE='liste_piece_noir.txt', STATUS = 'OLD', ACTION ='READ')
						DO w=1,16
							READ(10,*)nompiece5
							READ(20,*)nompiece6
							IF ( (echiquier(lignepiece, placementcol+i) == nompiece5) ) THEN
								IF ( i == abs((placementcol2 - placementcol)) ) THEN 
									
								ELSE
									good2='no'
									verifgood(m,1)=good2
									PRINT*, ''
									PRINT*, 'ERREUR : La tour que vous avez choisi ne peut pas effectuer de mouvement vers la droite,'
									PRINT*, 'il y a une pièce sur sa trajectoire.'
									PRINT*, "Veuillez entrer une position d'arrivée différente."
									PRINT*,''
								ENDIF
							ELSE
								good2='yes'
							ENDIF
							IF ( echiquier(lignepiece,placementcol+i) == nompiece6 ) THEN
								IF ( i == abs((placementcol2 - placementcol)) ) THEN 
									
								ELSE
									good2='no'
									verifgood2(m,1)=good2
									PRINT*, ''
									PRINT*, 'ERREUR : La tour que vous avez choisi ne peut pas effectuer de mouvement vers la droite,'
									PRINT*, 'il y a une pièce sur sa trajectoire.'
									PRINT*, "Veuillez entrer une position d'arrivée différente."
									PRINT*,''
								ENDIF
							ELSE
								good2='yes'
							ENDIF
						ENDDO
						CLOSE (UNIT=10)
						CLOSE (UNIT=20)
						DO WHILE (verifgood(m,1)=='.')
							IF (m==17) THEN
								EXIT
							ELSE
								m=m+1
							ENDIF
						ENDDO
						DO WHILE (verifgood2(p,1)=='.')
							IF (p==17) THEN
								EXIT
							ELSE
								p=p+1
							ENDIF
						ENDDO
						IF ( p+m-2 == 32 ) THEN
							good='yes'
						ELSE 
							good='no'
							CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
						ENDIF
					ENDIF
				ENDDO
			
			ELSE IF ( placementcol == placementcol2 ) THEN 				!Si la colonne est la même la tour change uniquement de ligne!
				DO i = 1,abs((lignepiece - lingpiecabouger))
					p=1
					m=1
					IF ( (lignepiece - lingpiecabouger) < 0 ) THEN 		
						OPEN(UNIT=10, FILE='liste_piece_blanche.txt', STATUS = 'OLD', ACTION ='READ')
						OPEN(UNIT=20, FILE='liste_piece_noir.txt', STATUS = 'OLD', ACTION ='READ')
						DO w=1,16
							READ(10,*)nompiece7
							READ(20,*)nompiece8
							IF ( (echiquier(lingpiecabouger-i,placementcol) == nompiece7) ) THEN
								IF ( i == abs((lignepiece - lingpiecabouger)) ) THEN 
									
								ELSE
									good2='no'
									verifgood(w,1)=good2
									PRINT*, ''
									PRINT*, 'ERREUR : La tour que vous avez choisi ne peut pas effectuer de mouvement vers le haut,'
									PRINT*, 'il y a une pièce sur sa trajectoire.'
									PRINT*, "Veuillez entrer une position d'arrivée différente."
									PRINT*,''
								ENDIF
							ENDIF
							IF ( echiquier(lingpiecabouger-i,placementcol) == nompiece8 ) THEN
								IF ( i == abs((lignepiece - lingpiecabouger)) ) THEN 
									
								ELSE
									good2='no'
									verifgood2(w,1)=good2
									PRINT*, ''
									PRINT*, 'ERREUR : La tour que vous avez choisi ne peut pas effectuer de mouvement vers le haut,'
									PRINT*, 'il y a une pièce sur sa trajectoire.'
									PRINT*, "Veuillez entrer une position d'arrivée différente."
									PRINT*,''
								ENDIF
							ENDIF 
						ENDDO
						CLOSE (UNIT=10)
						CLOSE (UNIT=20)
						DO WHILE (verifgood(m,1)=='.')
							IF (m==17) THEN
								EXIT
							ELSE
								m=m+1
							ENDIF
						ENDDO
						DO WHILE (verifgood2(p,1)=='.')
							IF (p==17) THEN
								EXIT
							ELSE
								p=p+1
							ENDIF
						ENDDO
						IF ( p+m-2 == 32 ) THEN
							good='yes'
						ELSE 
							good='no'
							CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
						ENDIF
					ELSEIF ( (lignepiece - lingpiecabouger) == 0 ) THEN
						PRINT*,''
						PRINT*, "La tour que vous avez choisi ne peut pas rester à sa même place."
						PRINT*, "Veuillez effectuer un déplacement avec cette pièce."
						PRINT*,''
					ELSEIF ( (lignepiece - lingpiecabouger) > 0 ) THEN !Si ligne final - ligne depart > 0 = descend
						OPEN(UNIT=10, FILE='liste_piece_blanche.txt', STATUS = 'OLD', ACTION ='READ')
						OPEN(UNIT=20, FILE='liste_piece_noir.txt', STATUS = 'OLD', ACTION ='READ')
						DO w=1,16
							READ(10,*)nompiece7
							READ(20,*)nompiece8
							IF ( (echiquier(lingpiecabouger+i,placementcol) == nompiece7) ) THEN
								IF ( i == abs((lignepiece - lingpiecabouger)) ) THEN 
									
								ELSE
									good2='no'
									verifgood(w,1)=good2
									PRINT*, ''
									PRINT*, 'ERREUR : La tour que vous avez choisi ne peut pas effectuer de mouvement vers le bas,'
									PRINT*, 'il y a une pièce sur sa trajectoire.'
									PRINT*, "Veuillez entrer une position d'arrivée différente."
									PRINT*,''
								ENDIF
			
							ELSE
								good2='yes'
							ENDIF
							IF ( echiquier(lingpiecabouger+i,placementcol) == nompiece8 ) THEN
								IF ( i == abs((lignepiece - lingpiecabouger)) ) THEN 
									
								ELSE
									good2='no'
									verifgood2(w,1)=good2
									PRINT*, ''
									PRINT*, 'ERREUR : La tour que vous avez choisi ne peut pas effectuer de mouvement vers le bas,'
									PRINT*, 'il y a une pièce sur sa trajectoire.'
									PRINT*, "Veuillez entrer une position d'arrivée différente."
									PRINT*,''
								ENDIF
							ELSE
								good2='yes'
							ENDIF
						ENDDO
						CLOSE (UNIT=10)
						CLOSE (UNIT=20)
						DO WHILE (verifgood(m,1)=='.')
							IF (m==17) THEN
								EXIT
							ELSE
								m=m+1
							ENDIF
						ENDDO
						DO WHILE (verifgood2(p,1)=='.')
							IF (p==17) THEN
								EXIT
							ELSE
								p=p+1
							ENDIF
						ENDDO
						IF ( p+m-2 == 32 ) THEN
							good='yes'
						ELSE 
							good='no'
							CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
						ENDIF
					ENDIF
				ENDDO
			ELSE
				PRINT*,"ERREUR : La tour ne peut effectuer que des mouvements sur la même ligne ou la même colonne. "
				PRINT*,"Entrer un mouvement correct."
				PRINT*,''
				CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
			END IF
		ENDIF
	ENDIF


!MOUVEMENT CAVALIERS BLANCS!
	IF ( (entree == 'CavalierB1') .OR. (entree == 'CavalierB2') ) THEN
		OPEN(UNIT=10, FILE='liste_piece_blanche.txt', STATUS = 'OLD', ACTION ='READ')
		DO i =1,16
			READ(10,*)nompiece
			IF ( echiquier(lignepiece, placementcol2) == nompiece ) THEN
				good4='no'
				verifcavalier(i,1)=good4
				PRINT*,''
				PRINT*," ERREUR : Le cavalier blanc choisi ne peut pas manger une pièce blanche."
				PRINT*,"Entrez un mouvement correct."
				PRINT*,''
			ELSE
				good4='yes'
			ENDIF
		ENDDO
		CLOSE (UNIT=10)
		DO WHILE (verifcavalier(t,1)=='.')
			IF (t==17) THEN
				EXIT
			ELSE
				t=t+1
			ENDIF
		ENDDO
		IF ( t-1 == 16 ) THEN
			good4='yes'
		ELSE 
			good4='no'
		ENDIF
		IF ( good4 == 'no' ) THEN 
			good='no'
			CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
		ELSEIF ( good4 == 'yes') THEN
			IF ( (lignepiece == lingpiecabouger - 2) .AND. (placementcol2 == placementcol - 1) ) THEN  		!Mouvement du cavalier blanc vers la gauche de 1 et vers le haut de 2
				good='yes'
			ELSEIF ( (lignepiece == lingpiecabouger -1) .AND. (placementcol2 == placementcol - 2) ) THEN
				good='yes'
			ELSEIF ( (lignepiece == lingpiecabouger + 1) .AND. (placementcol2 == placementcol - 2) ) THEN
				good='yes'
			ELSEIF ( (lignepiece == lingpiecabouger + 2) .AND. (placementcol2 == placementcol - 1) ) THEN
				good='yes'
			ELSEIF ( (lignepiece == lingpiecabouger + 2) .AND. (placementcol2 == placementcol +1 ) ) THEN
				good='yes'
			ELSEIF ( (lignepiece == lingpiecabouger + 1) .AND. (placementcol2 == placementcol + 2) ) THEN
				good='yes'
			ELSEIF ( (lignepiece == lingpiecabouger - 1) .AND. (placementcol2 == placementcol + 2) ) THEN
				good='yes'
			ELSEIF ( (lignepiece == lingpiecabouger - 2) .AND. (placementcol2 == placementcol + 1) ) THEN
				good='yes'
			ELSE 
				PRINT*,''
				PRINT*,"ERREUR : Le cavalier blanc choisi ne peut pas effectuer le mouvement choisi."
				PRINT*,'Règle : Le cavalier ne peut bouger qu avec des mouvements de "L".'
				PRINT*,''
				CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
			ENDIF
		ENDIF
	ENDIF


!MOUVEMENT CAVALIERS NOIRS!

IF ( (entree == 'CavalierN1') .OR. (entree == 'CavalierN2') ) THEN
		OPEN(UNIT=20, FILE='liste_piece_noir.txt', STATUS = 'OLD', ACTION ='READ')
		DO i =1,16
			READ(20,*)nompiece
			IF ( echiquier(lignepiece, placementcol2) == nompiece ) THEN
				good4='no'
				verifcavalier(i,1)=good4
				PRINT*, 'nompiece=',nompiece
				PRINT*,''
				PRINT*,"ERREUR : Le cavalier noir choisi ne peut pas manger une pièce noire."
				PRINT*,"Entrez un mouvement correct."
				PRINT*,''
			ELSE
				good4='yes'
			ENDIF
		ENDDO
		CLOSE (UNIT=20)
		DO WHILE (verifcavalier(t,1)=='.')
			IF (t==17) THEN
				EXIT
			ELSE
				t=t+1
			ENDIF
		ENDDO
		IF ( t-1 == 16 ) THEN
			good4='yes'
		ELSE 
			good4='no'
		ENDIF
		IF ( good4 == 'no' ) THEN 
			good='no'
			CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
		ELSEIF ( good4 == 'yes') THEN
			IF ( (lignepiece == lingpiecabouger - 2) .AND. (placementcol2 == placementcol - 1) ) THEN  		!Mouvement du cavalier blanc vers la gauche de 1 et vers le haut de 2
				good='yes'
			ELSEIF ( (lignepiece == lingpiecabouger -1) .AND. (placementcol2 == placementcol - 2) ) THEN
				good='yes'
			ELSEIF ( (lignepiece == lingpiecabouger + 1) .AND. (placementcol2 == placementcol - 2) ) THEN
				good='yes'
			ELSEIF ( (lignepiece == lingpiecabouger + 2) .AND. (placementcol2 == placementcol - 1) ) THEN
				good='yes'
			ELSEIF ( (lignepiece == lingpiecabouger + 2) .AND. (placementcol2 == placementcol +1 ) ) THEN
				good='yes'
			ELSEIF ( (lignepiece == lingpiecabouger + 1) .AND. (placementcol2 == placementcol + 2) ) THEN
				good='yes'
			ELSEIF ( (lignepiece == lingpiecabouger - 1) .AND. (placementcol2 == placementcol + 2) ) THEN
				good='yes'
			ELSEIF ( (lignepiece == lingpiecabouger - 2) .AND. (placementcol2 == placementcol + 1) ) THEN
				good='yes'
			ELSE 
				PRINT*,''
				PRINT*,"ERREUR : Le cavalier noir choisi ne peut pas effectuer le mouvement choisi."
				PRINT*,''
				PRINT*,'Règle : Le cavalier ne peut bouger qu avec des mouvements de "L".'
				PRINT*,''
				CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
			ENDIF
		ENDIF
	ENDIF


!Mouvement fou!

	IF ( (entree == 'FouB1') .OR. (entree == 'FouB2') ) THEN
		OPEN(UNIT=10, FILE='liste_piece_blanche.txt', STATUS = 'OLD', ACTION ='READ')
		DO i =1,16
			READ(10,*)nompiece
			IF ( echiquier(lignepiece, placementcol2) == nompiece ) THEN
				good2='no'
				verif(i,1)=good2
				PRINT*,''
				PRINT*," ERREUR : Le fou blanc choisi ne peut pas manger une pièce blanche."
				PRINT*,''
				PRINT*,"Entrez un mouvement correct."
				PRINT*,''
			ELSE
				good2='yes'
			ENDIF
		ENDDO
		CLOSE (UNIT=10)
		DO WHILE (verif(u,1)=='.')
			IF (u==17) THEN
				EXIT
			ELSE
				u=u+1
			ENDIF
		ENDDO
		IF ( u-1 == 16 ) THEN
			good5='yes'
		ELSE 
			good5='no'
		ENDIF
	ELSEIF ( (entree == 'FouN1') .OR. (entree == 'FouN2') ) THEN
		OPEN(UNIT=20, FILE='liste_piece_noir.txt', STATUS = 'OLD', ACTION ='READ')
		DO i =1,16
			READ(20,*)nompiece
			IF ( echiquier(lignepiece, placementcol2) == nompiece ) THEN
				good2='no'
				verif(i,1)=good2
				PRINT*,''
				PRINT*," ERREUR : Le fou noir choisi ne peut pas manger une pièce noir."
				PRINT*,''
				PRINT*,"Entrez un mouvement correct."
				PRINT*,''
			ELSE
				good2='yes'
			ENDIF
		ENDDO
		CLOSE (UNIT=20)
		DO WHILE (verif(u,1)=='.')
			IF (u==17) THEN
				EXIT
			ELSE
				u=u+1
			ENDIF
		ENDDO
		IF ( u-1 == 16 ) THEN
			good5='yes'
		ELSE 
			good5='no'
		ENDIF
	ENDIF
	IF ((entree == 'FouB1') .OR. (entree == 'FouB2') .OR.(entree == 'FouN1') .OR. (entree == 'FouN2') ) THEN
		IF ( good5 == 'no' ) THEN 
			good='no'
			CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
		ELSEIF ( good5 == 'yes') THEN
			IF ( abs(lignepiece - lingpiecabouger) == abs(placementcol2 - placementcol) ) THEN  !Déplacement du fou en diagonale si bouge de X en ligne et de X en colonne
				IF ( (lignepiece - lingpiecabouger < 0) .AND. (placementcol2 - placementcol > 0) ) THEN
					DO i=1,abs(lignepiece - lingpiecabouger)
						OPEN(UNIT=10, FILE='liste_piece_blanche.txt', STATUS = 'OLD', ACTION ='READ')
						OPEN(UNIT=20, FILE='liste_piece_noir.txt', STATUS = 'OLD', ACTION ='READ')
						DO w = 1,16
							READ(10,*)nompiece1
							READ(20,*)nompiece2
							IF ( echiquier(lingpiecabouger-i, placementcol + i) == nompiece1 ) THEN
								IF ( i == abs((lignepiece - lingpiecabouger)) ) THEN 

								ELSE
									good6='no'
									veriffou(w,1)=good6
									PRINT*,''
									PRINT*," ERREUR : Le fou choisi ne effectuer le mouvement choisi, il y a une pièce blanche sur sa trajectoire."
									PRINT*,''
									PRINT*,"Entrez un mouvement correct."
									PRINT*,''
								ENDIF
							ELSE
								good6='yes'
							ENDIF
							IF ( echiquier(lingpiecabouger-i, placementcol + i) == nompiece2 ) THEN
								IF ( i == abs((lignepiece - lingpiecabouger)) ) THEN 
									
								ELSE
									good6='no'
									veriffou2(w,1)=good6
									PRINT*,''
									PRINT*," Le fou choisi ne effectuer le mouvement choisi, il y a une pièce noire sur sa trajectoire."
									PRINT*,''
									PRINT*,"Entrez un mouvement correct."
									PRINT*,''
								ENDIF
							ELSE
								good6='yes'
							ENDIF
						ENDDO
						CLOSE (UNIT=10)
						CLOSE (UNIT=20)
						DO WHILE (veriffou(a,1)=='.')
							IF (a==17) THEN
								EXIT
							ELSE
								a=a+1
							ENDIF
						ENDDO
						DO WHILE (veriffou2(c,1)=='.')
							IF (c==17) THEN
								EXIT
							ELSE
								c=c+1
							ENDIF
						ENDDO
						IF ( a+c-2 == 32 ) THEN
							good='yes'
						ELSE 
							good='no'
							CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
						ENDIF
					ENDDO
				ELSEIF ( (lignepiece - lingpiecabouger < 0) .AND. (placementcol2 - placementcol < 0) ) THEN
					DO i=1,abs(lignepiece - lingpiecabouger)
						OPEN(UNIT=10, FILE='liste_piece_blanche.txt', STATUS = 'OLD', ACTION ='READ')
						OPEN(UNIT=20, FILE='liste_piece_noir.txt', STATUS = 'OLD', ACTION ='READ')
						DO w = 1,16
							READ(10,*)nompiece1
							READ(20,*)nompiece2
							IF ( echiquier(lingpiecabouger-i, placementcol-i) == nompiece1 ) THEN
								IF ( i == abs((lignepiece - lingpiecabouger)) ) THEN 
									
								ELSE
									good6='no'
									veriffou(w,1)=good6
									PRINT*,''
									PRINT*," ERREUR : Le fou choisi ne effectuer le mouvement choisi, il y a une pièce blanche sur sa trajectoire."
									PRINT*,"Entrez un mouvement correct."
									PRINT*,''
								ENDIF
							ELSE
								good6='yes'
							ENDIF
							IF ( echiquier(lingpiecabouger-i, placementcol - i) == nompiece2 ) THEN
								IF ( i == abs((lignepiece - lingpiecabouger)) ) THEN 
									
								ELSE
									good6='no'
									veriffou2(w,1)=good6
									PRINT*,''
									PRINT*," Le fou choisi ne effectuer le mouvement choisi, il y a une pièce noire sur sa trajectoire."
									PRINT*,"Entrez un mouvement correct."
									PRINT*,''
								ENDIF
							ELSE
								good6='yes'
							ENDIF
						ENDDO
						CLOSE (UNIT=10)
						CLOSE (UNIT=20)
		
						DO WHILE (veriffou(a,1)=='.')
							IF (a==17) THEN
								EXIT
							ELSE
								a=a+1
							ENDIF
						ENDDO
						DO WHILE (veriffou2(c,1)=='.')
							IF (c==17) THEN
								EXIT
							ELSE
								c=c+1
							ENDIF
						ENDDO
						IF ( a+c-2 == 32 ) THEN
							good='yes'
						ELSE 
							good='no'
							CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
						ENDIF
					ENDDO
				ELSEIF ( (lignepiece - lingpiecabouger > 0) .AND. (placementcol2 - placementcol < 0) ) THEN
					DO i=1,abs(lignepiece - lingpiecabouger)
						OPEN(UNIT=10, FILE='liste_piece_blanche.txt', STATUS = 'OLD', ACTION ='READ')
						OPEN(UNIT=20, FILE='liste_piece_noir.txt', STATUS = 'OLD', ACTION ='READ')
						DO w = 1,16
							READ(10,*)nompiece1
							READ(20,*)nompiece2
							IF ( echiquier(lingpiecabouger+i, placementcol-i) == nompiece1 ) THEN
								IF ( i == abs((lignepiece - lingpiecabouger)) ) THEN 
									
								ELSE
									good6='no'
									veriffou(w,1)=good6
									PRINT*,''
									PRINT*," ERREUR : Le fou choisi ne effectuer le mouvement choisi, il y a une pièce blanche sur sa trajectoire."
									PRINT*,"Entrez un mouvement correct."
									PRINT*,''
								ENDIF
							ELSE
								good6='yes'
							ENDIF
							IF ( echiquier(lingpiecabouger+i, placementcol - i) == nompiece2 ) THEN
								IF ( i == abs((lignepiece - lingpiecabouger)) ) THEN 
									
								ELSE
									good6='no'
									veriffou2(w,1)=good6
									PRINT*,''
									PRINT*," Le fou choisi ne effectuer le mouvement choisi, il y a une pièce noire sur sa trajectoire."
									PRINT*,"Entrez un mouvement correct."
									PRINT*,''
								ENDIF
							ELSE
								good6='yes'
							ENDIF
						ENDDO
						CLOSE (UNIT=10)
						CLOSE (UNIT=20)
						DO WHILE (veriffou(a,1)=='.')
							IF (a==17) THEN
								EXIT
							ELSE
								a=a+1
							ENDIF
						ENDDO
						DO WHILE (veriffou2(c,1)=='.')
							IF (c==17) THEN
								EXIT
							ELSE
								c=c+1
							ENDIF
						ENDDO
						IF ( a+c-2 == 32 ) THEN
							good='yes'
						ELSE 
							good='no'
							CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
						ENDIF
					ENDDO
				ELSEIF ( (lignepiece - lingpiecabouger > 0) .AND. (placementcol2 - placementcol > 0) ) THEN
					DO i=1,abs(lignepiece - lingpiecabouger)
						OPEN(UNIT=10, FILE='liste_piece_blanche.txt', STATUS = 'OLD', ACTION ='READ')
						OPEN(UNIT=20, FILE='liste_piece_noir.txt', STATUS = 'OLD', ACTION ='READ')
						DO w = 1,16
							READ(10,*)nompiece1
							READ(20,*)nompiece2
							IF ( echiquier(lingpiecabouger+i, placementcol+i) == nompiece1 ) THEN
								IF ( i == abs((lignepiece - lingpiecabouger)) ) THEN 
									
								ELSE
									good6='no'
									veriffou(w,1)=good6
									PRINT*,''
									PRINT*," ERREUR : Le fou choisi ne effectuer le mouvement choisi, il y a une pièce blanche sur sa trajectoire."
									PRINT*,"Entrez un mouvement correct."
									PRINT*,''
								ENDIF
							ELSE
								good6='yes'
							ENDIF
							IF ( echiquier(lingpiecabouger+i, placementcol + i) == nompiece2 ) THEN
								IF ( i == abs((lignepiece - lingpiecabouger)) ) THEN 
									
								ELSE
									good6='no'
									veriffou2(w,1)=good6
									PRINT*,''
									PRINT*," Le fou choisi ne effectuer le mouvement choisi, il y a une pièce noire sur sa trajectoire."
									PRINT*,"Entrez un mouvement correct."
									PRINT*,''
								ENDIF
							ELSE
								good6='yes'
							ENDIF
						ENDDO
						CLOSE (UNIT=10)
						CLOSE (UNIT=20)
						DO WHILE (veriffou(a,1)=='.')
							IF (a==17) THEN
								EXIT
							ELSE
								a=a+1
							ENDIF
						ENDDO
						DO WHILE (veriffou2(c,1)=='.')
							IF (c==17) THEN
								EXIT
							ELSE
								c=c+1
							ENDIF
						ENDDO
						IF ( a+c-2 == 32 ) THEN
							good='yes'
						ELSE 
							good='no'
							CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
						ENDIF
					ENDDO
				ENDIF
			ELSE
				good='no'
				PRINT*,''
				PRINT*," ERREUR : Le fou choisi ne peut pas effectuer le mouvement choisi."
				PRINT*,"Règle : Le fou se déplace en diagonale."
				PRINT*,''
				CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
			ENDIF

		ENDIF
	ENDIF

!Mouvement de la Dame

	IF ( (entree == 'DameB') ) THEN
		OPEN(UNIT=10, FILE='liste_piece_blanche.txt', STATUS = 'OLD', ACTION ='READ')
		DO i =1,16
			READ(10,*)nompiece
			IF ( echiquier(lignepiece, placementcol2) == nompiece ) THEN
				good2='no'
				verif(i,1)=good2
				PRINT*,''
				PRINT*," ERREUR : La dame blanche choisi ne peut pas manger une pièce blanche."
				PRINT*,"Entrez un mouvement correct."
				PRINT*,''
			ELSE
				good2='yes'
			ENDIF
		ENDDO
		CLOSE (UNIT=10)
		DO WHILE (verif(u,1)=='.')
			IF (u==17) THEN
				EXIT
			ELSE
				u=u+1
			ENDIF
		ENDDO
		IF ( u-1 == 16 ) THEN
			good7='yes'
		ELSE 
			good7='no'
		ENDIF
	ELSEIF ( (entree == 'DameN')) THEN
		OPEN(UNIT=20, FILE='liste_piece_noir.txt', STATUS = 'OLD', ACTION ='READ')
		DO i =1,16
			READ(20,*)nompiece
			IF ( echiquier(lignepiece, placementcol2) == nompiece ) THEN
				good2='no'
				verif(i,1)=good2
				PRINT*,''
				PRINT*," ERREUR : La dame noire choisi ne peut pas manger une pièce noir."
				PRINT*,"Entrez un mouvement correct."
				PRINT*,''
			ELSE
				good2='yes'
			ENDIF
		ENDDO
		CLOSE (UNIT=20)
		DO WHILE (verif(e,1)=='.')
			IF (e==17) THEN
				EXIT
			ELSE
				e=e+1
			ENDIF
		ENDDO
	
		IF ( e-1 == 16 ) THEN
			good7='yes'
		ELSE 
			good7='no'
		ENDIF
	ENDIF
	IF ((entree == 'DameB') .OR. (entree == 'DameN') ) THEN
		IF ( good7 == 'no' ) THEN 
			good='no'
			CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
		ELSEIF ( good7 == 'yes') THEN
			IF ( abs(lignepiece - lingpiecabouger) == abs(placementcol2 - placementcol) ) THEN  !Déplacement en diagonale 
				IF ( (lignepiece - lingpiecabouger < 0) .AND. (placementcol2 - placementcol > 0) ) THEN
					DO i=1,abs(lignepiece - lingpiecabouger)
						OPEN(UNIT=10, FILE='liste_piece_blanche.txt', STATUS = 'OLD', ACTION ='READ')
						OPEN(UNIT=20, FILE='liste_piece_noir.txt', STATUS = 'OLD', ACTION ='READ')
						DO w = 1,16
							READ(10,*)nompiece1
							READ(20,*)nompiece2
							IF ( echiquier(lingpiecabouger-i, placementcol + i) == nompiece1 ) THEN
								IF ( i == abs((lignepiece - lingpiecabouger)) ) THEN 

								ELSE
									good6='no'
									verifdamed(w,1)=good6
									PRINT*,''
									PRINT*," ERREUR : La dame choisi ne effectuer le mouvement choisi, il y a une pièce blanche sur sa trajectoire."
									PRINT*,"Entrez un mouvement correct."
									PRINT*,''
								ENDIF
							ELSE
								good6='yes'
							ENDIF
							IF ( echiquier(lingpiecabouger-i, placementcol + i) == nompiece2 ) THEN
								IF ( i == abs((lignepiece - lingpiecabouger)) ) THEN 
									
								ELSE
									good6='no'
									verifdamed2(w,1)=good6
									PRINT*,''
									PRINT*," La dame choisi ne effectuer le mouvement choisi, il y a une pièce noire sur sa trajectoire."
									PRINT*,"Entrez un mouvement correct."
									PRINT*,''
								ENDIF
							ELSE
								good6='yes'
							ENDIF
						ENDDO
						CLOSE (UNIT=10)
						CLOSE (UNIT=20)
						DO WHILE (verifdamed(a1,1)=='.')
							IF (a1==17) THEN
								EXIT
							ELSE
								a1=a1+1
							ENDIF
						ENDDO
						DO WHILE (verifdamed2(a2,1)=='.')
							IF (a2==17) THEN
								EXIT
							ELSE
								a2=a2+1
							ENDIF
						ENDDO
						IF ( a1+a2-2 == 32 ) THEN
							good='yes'
						ELSE 
							good='no'
							CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
						ENDIF
					ENDDO
				ELSEIF ( (lignepiece - lingpiecabouger < 0) .AND. (placementcol2 - placementcol < 0) ) THEN
					DO i=1,abs(lignepiece - lingpiecabouger)
						OPEN(UNIT=10, FILE='liste_piece_blanche.txt', STATUS = 'OLD', ACTION ='READ')
						OPEN(UNIT=20, FILE='liste_piece_noir.txt', STATUS = 'OLD', ACTION ='READ')
						DO w = 1,16
							READ(10,*)nompiece1
							READ(20,*)nompiece2
							IF ( echiquier(lingpiecabouger-i, placementcol-i) == nompiece1 ) THEN
								IF ( i == abs((lignepiece - lingpiecabouger)) ) THEN 
									
								ELSE
									good6='no'
									verifdamed(w,1)=good6
									PRINT*,''
									PRINT*," ERREUR : La dame choisi ne effectuer le mouvement choisi, il y a une pièce blanche sur sa trajectoire."
									PRINT*,"Entrez un mouvement correct."
									PRINT*,''
								ENDIF
							ELSE
								good6='yes'
							ENDIF
							IF ( echiquier(lingpiecabouger-i, placementcol - i) == nompiece2 ) THEN
								IF ( i == abs((lignepiece - lingpiecabouger)) ) THEN 
									
								ELSE
									good6='no'
									verifdamed2(w,1)=good6
									PRINT*,''
									PRINT*," La dame choisi ne effectuer le mouvement choisi, il y a une pièce noire sur sa trajectoire."
									PRINT*,"Entrez un mouvement correct."
									PRINT*,''
								ENDIF
							ELSE
								good6='yes'
							ENDIF
						ENDDO
						CLOSE (UNIT=10)
						CLOSE (UNIT=20)
		
						DO WHILE (verifdamed(a3,1)=='.')
							IF (a3==17) THEN
								EXIT
							ELSE
								a3=a3+1
							ENDIF
						ENDDO
						DO WHILE (verifdamed2(a4,1)=='.')
							IF (a4==17) THEN
								EXIT
							ELSE
								a4=a4+1
							ENDIF
						ENDDO
						IF ( a3+a4-2 == 32 ) THEN
							good='yes'
						ELSE 
							good='no'
							CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
						ENDIF
					ENDDO
				
				ELSEIF ( (lignepiece - lingpiecabouger > 0) .AND. (placementcol2 - placementcol < 0) ) THEN
					DO i=1,abs(lignepiece - lingpiecabouger)
						OPEN(UNIT=10, FILE='liste_piece_blanche.txt', STATUS = 'OLD', ACTION ='READ')
						OPEN(UNIT=20, FILE='liste_piece_noir.txt', STATUS = 'OLD', ACTION ='READ')
						DO w = 1,16
							READ(10,*)nompiece1
							READ(20,*)nompiece2
							IF ( echiquier(lingpiecabouger+i, placementcol-i) == nompiece1 ) THEN
								IF ( i == abs((lignepiece - lingpiecabouger)) ) THEN 
									
								ELSE
									good6='no'
									verifdamed(w,1)=good6
									PRINT*,''
									PRINT*," ERREUR : La dame choisi ne effectuer le mouvement choisi, il y a une pièce blanche sur sa trajectoire."
									PRINT*,"Entrez un mouvement correct."
									PRINT*,''
								ENDIF
							ELSE
								good6='yes'
							ENDIF
							IF ( echiquier(lingpiecabouger+i, placementcol - i) == nompiece2 ) THEN
								IF ( i == abs((lignepiece - lingpiecabouger)) ) THEN 
									
								ELSE
									good6='no'
									verifdamed(w,1)=good6
									PRINT*,''
									PRINT*," Le dame choisi ne effectuer le mouvement choisi, il y a une pièce noire sur sa trajectoire."
									PRINT*,"Entrez un mouvement correct."
									PRINT*,''
								ENDIF
							ELSE
								good6='yes'
							ENDIF
						ENDDO
						CLOSE (UNIT=10)
						CLOSE (UNIT=20)
						DO WHILE (verifdamed(a5,1)=='.')
							IF (a5==17) THEN
								EXIT
							ELSE
								a5=a5+1
							ENDIF
						ENDDO
						DO WHILE (verifdamed(a6,1)=='.')
							IF (a6==17) THEN
								EXIT
							ELSE
								a6=a6+1
							ENDIF
						ENDDO
						IF ( a6+a5-2 == 32 ) THEN
							good='yes'
						ELSE 
							good='no'
							CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
						ENDIF
					ENDDO
				ELSEIF ( (lignepiece - lingpiecabouger > 0) .AND. (placementcol2 - placementcol > 0) ) THEN
					DO i=1,abs(lignepiece - lingpiecabouger)
						OPEN(UNIT=10, FILE='liste_piece_blanche.txt', STATUS = 'OLD', ACTION ='READ')
						OPEN(UNIT=20, FILE='liste_piece_noir.txt', STATUS = 'OLD', ACTION ='READ')
						DO w = 1,16
							READ(10,*)nompiece1
							READ(20,*)nompiece2
							IF ( echiquier(lingpiecabouger+i, placementcol+i) == nompiece1 ) THEN
								IF ( i == abs((lignepiece - lingpiecabouger)) ) THEN 
									
								ELSE
									good6='no'
									verifdamed(w,1)=good6
									PRINT*,''
									PRINT*," ERREUR : La dame choisi ne effectuer le mouvement choisi, il y a une pièce blanche sur sa trajectoire."
									PRINT*,"Entrez un mouvement correct."
									PRINT*,''
								ENDIF
							ELSE
								good6='yes'
							ENDIF
							IF ( echiquier(lingpiecabouger+i, placementcol + i) == nompiece2 ) THEN
								IF ( i == abs((lignepiece - lingpiecabouger)) ) THEN 
									
								ELSE
									good6='no'
									verifdamed2(w,1)=good6
									PRINT*,''
									PRINT*," La dame choisi ne effectuer le mouvement choisi, il y a une pièce noire sur sa trajectoire."
									PRINT*,"Entrez un mouvement correct."
									PRINT*,''
								ENDIF
							ELSE
								good6='yes'
							ENDIF
						ENDDO
						CLOSE (UNIT=10)
						CLOSE (UNIT=20)
						DO WHILE (verifdamed(a7,1)=='.')
							IF (a7==17) THEN
								EXIT
							ELSE
								a7=a7+1
							ENDIF
						ENDDO
						DO WHILE (verifdamed2(a8,1)=='.')
							IF (a8==17) THEN
								EXIT
							ELSE
								a8=a8+1
							ENDIF
						ENDDO
						IF ( a8+a7-2 == 32 ) THEN
							good='yes'
						ELSE 
							good='no'
							CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
						ENDIF
					ENDDO
				ENDIF
			ELSEIF ( lingpiecabouger == lignepiece ) THEN 				!Si la ligne est la même la dame change uniquement de colonne!
					!Mouvement possible si aucune piece sur la trajectoire!
					DO i = 1, abs((placementcol2 - placementcol))
						IF ( (placementcol2 - placementcol) < 0 ) THEN  		!Si colonne final - colonne initiale <0 => Droite vers gauche
							OPEN(UNIT=10, FILE='liste_piece_blanche.txt', STATUS = 'OLD', ACTION ='READ')
							OPEN(UNIT=20, FILE='liste_piece_noir.txt', STATUS = 'OLD', ACTION ='READ')
							DO w=1,16
								READ(10,*)nompiece5
								READ(20,*)nompiece6
								IF ( (echiquier(lignepiece, placementcol-i) == nompiece5) ) THEN
									IF ( i == abs((placementcol2 - placementcol)) ) THEN 
									
									ELSE
										good2='no'
										verifdamea(m,1)=good2
										PRINT*,''
										PRINT*, 'ERREUR : La dame que vous avez choisi ne peut pas effectuer un mouvement vers la gauche,'
										PRINT*, 'il y a une pièce sur sa trajectoire.'
										PRINT*, "Veuillez entrer une position d'arrivée différente."
										PRINT*,''
									ENDIF

								ELSE
									good2='yes'
								ENDIF
								IF ( echiquier(lignepiece, placementcol-i) == nompiece6 ) THEN
									IF ( i == abs((placementcol2 - placementcol)) ) THEN 
									
									ELSE
										good2='no'
										verifdamea2(m,1)=good2
										PRINT*,''
										PRINT*, 'ERREUR : La dame que vous avez choisi ne peut pas effectuer un mouvement vers la gauche,'
										PRINT*, 'il y a une pièce sur sa trajectoire.'
										PRINT*, "Veuillez entrer une position d'arrivée différente."
										PRINT*,''
									ENDIF
								ELSE
									good2='yes'
								ENDIF
							ENDDO
							CLOSE (UNIT=10)
							CLOSE (UNIT=20)
							DO WHILE (verifdamea(m,1)=='.')
								IF (m==17) THEN
									EXIT
								ELSE
									m=m+1
								ENDIF
							ENDDO
							DO WHILE (verifdamea2(p,1)=='.')
								IF (p==17) THEN
									EXIT
								ELSE
									p=p+1
								ENDIF
							ENDDO
							IF ( p+m-2 == 32 ) THEN
								good='yes'
							ELSE 
								good='no'
								CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
							ENDIF
						ELSEIF ( (placementcol2 - placementcol) == 0 ) THEN 	!Si colonne final - colonne initiale =0 => même place
							PRINT*,''
							PRINT*, "La dame que vous avez choisi ne peut pas rester à sa même place."
							PRINT*, "Veuillez effectuer un déplacement avec cette pièce."
							PRINT*,''
						ELSEIF ( (placementcol2 - placementcol) > 0 ) THEN  	!Si colonne final - colonne initiale >0 => gauche vers droite
							OPEN(UNIT=10, FILE='liste_piece_blanche.txt', STATUS = 'OLD', ACTION ='READ')
							OPEN(UNIT=20, FILE='liste_piece_noir.txt', STATUS = 'OLD', ACTION ='READ')
							DO w=1,16
								READ(10,*)nompiece5
								READ(20,*)nompiece6
								IF ( (echiquier(lignepiece, placementcol+i) == nompiece5) ) THEN
									IF ( i == abs((placementcol2 - placementcol)) ) THEN 
									
									ELSE
										good2='no'
										verifdamea(m,1)=good2
										PRINT*, ''
										PRINT*, 'ERREUR : La dame que vous avez choisi ne peut pas effectuer de mouvement vers la droite,'
										PRINT*, 'il y a une pièce sur sa trajectoire.'
										PRINT*, "Veuillez entrer une position d'arrivée différente."
										PRINT*,''
									ENDIF
								ELSE
									good2='yes'
								ENDIF
								IF ( echiquier(lignepiece,placementcol+i) == nompiece6 ) THEN
									IF ( i == abs((placementcol2 - placementcol)) ) THEN 
									
									ELSE
										good2='no'
										verifdamea2(m,1)=good2
										PRINT*, ''
										PRINT*, 'ERREUR : La dame que vous avez choisi ne peut pas effectuer de mouvement vers la droite,'
										PRINT*, 'il y a une pièce sur sa trajectoire.'
										PRINT*, "Veuillez entrer une position d'arrivée différente."
										PRINT*,''
									ENDIF
								ELSE
									good2='yes'
								ENDIF
							ENDDO
						CLOSE (UNIT=10)
						CLOSE (UNIT=20)
						DO WHILE (verifdamea(m,1)=='.')
							IF (m==17) THEN
								EXIT
							ELSE
								m=m+1
							ENDIF
						ENDDO
						DO WHILE (verifdamea2(p,1)=='.')
							IF (p==17) THEN
								EXIT
							ELSE
								p=p+1
							ENDIF
						ENDDO
						IF ( p+m-2 == 32 ) THEN
							good='yes'
						ELSE 
							good='no'
							CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
						ENDIF
					ENDIF
				ENDDO
			
			ELSEIF ( placementcol == placementcol2 ) THEN 		!Si la colonne est la même la tour change uniquement de ligne!
				DO i = 1,abs((lignepiece - lingpiecabouger))
					IF ( (lignepiece - lingpiecabouger) < 0 ) THEN 		
						OPEN(UNIT=10, FILE='liste_piece_blanche.txt', STATUS = 'OLD', ACTION ='READ')
						OPEN(UNIT=20, FILE='liste_piece_noir.txt', STATUS = 'OLD', ACTION ='READ')
						DO w=1,16
							READ(10,*)nompiece7
							READ(20,*)nompiece8
							IF ( (echiquier(lingpiecabouger-i,placementcol) == nompiece7) ) THEN
								IF ( i == abs((lignepiece - lingpiecabouger)) ) THEN 
									
								ELSE
									good2='no'
									verifdamea(w,1)=good2
									PRINT*, ''
									PRINT*, 'ERREUR : La dame que vous avez choisi ne peut pas effectuer de mouvement vers le haut,'
									PRINT*, 'il y a une pièce sur sa trajectoire.'
									PRINT*, "Veuillez entrer une position d'arrivée différente."
									PRINT*,''
								ENDIF
							ENDIF
							IF ( echiquier(lingpiecabouger-i,placementcol) == nompiece8 ) THEN
								IF ( i == abs((lignepiece - lingpiecabouger)) ) THEN 
									
								ELSE
									good2='no'
									verifdamea2(w,1)=good2
									PRINT*, ''
									PRINT*, 'ERREUR : La dame que vous avez choisi ne peut pas effectuer de mouvement vers le haut,'
									PRINT*, 'il y a une pièce sur sa trajectoire.'
									PRINT*, "Veuillez entrer une position d'arrivée différente."
									PRINT*,''
								ENDIF
							ENDIF 
						ENDDO
						CLOSE (UNIT=10)
						CLOSE (UNIT=20)
						DO WHILE (verifdamea(m,1)=='.')
							IF (m==17) THEN
								EXIT
							ELSE
								m=m+1
							ENDIF
						ENDDO
						DO WHILE (verifdamea2(p,1)=='.')
							IF (p==17) THEN
								EXIT
							ELSE
								p=p+1
							ENDIF
						ENDDO
						IF ( p+m-2 == 32 ) THEN
							good='yes'
						ELSE 
							good='no'
							CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
						ENDIF
					ELSEIF ( (lignepiece - lingpiecabouger) == 0 ) THEN
						PRINT*,''
						PRINT*, "La dame que vous avez choisi ne peut pas rester à sa même place."
						PRINT*, "Veuillez effectuer un déplacement avec cette pièce."
						PRINT*,''
					ELSEIF ( (lignepiece - lingpiecabouger) > 0 ) THEN !Si ligne final - ligne depart > 0 = descend
						OPEN(UNIT=10, FILE='liste_piece_blanche.txt', STATUS = 'OLD', ACTION ='READ')
						OPEN(UNIT=20, FILE='liste_piece_noir.txt', STATUS = 'OLD', ACTION ='READ')
						DO w=1,16
							READ(10,*)nompiece7
							READ(20,*)nompiece8
							IF ( (echiquier(lingpiecabouger+i,placementcol) == nompiece7) ) THEN
								IF ( i == abs((lignepiece - lingpiecabouger)) ) THEN 
									
								ELSE
									good2='no'
									verifdamea(w,1)=good2
									PRINT*, ''
									PRINT*, 'ERREUR : La dame que vous avez choisi ne peut pas effectuer de mouvement vers le bas,'
									PRINT*, 'il y a une pièce sur sa trajectoire.'
									PRINT*, "Veuillez entrer une position d'arrivée différente."
									PRINT*,''
								ENDIF
			
							ELSE
								good2='yes'
							ENDIF
							IF ( echiquier(lingpiecabouger+i,placementcol) == nompiece8 ) THEN
								IF ( i == abs((lignepiece - lingpiecabouger)) ) THEN 
									
								ELSE
									good2='no'
									verifdamea2(w,1)=good2
									PRINT*, ''
									PRINT*, 'ERREUR : La tour que vous avez choisi ne peut pas effectuer de mouvement vers le bas,'
									PRINT*, 'il y a une pièce sur sa trajectoire.'
									PRINT*, "Veuillez entrer une position d'arrivée différente."
									PRINT*,''
								ENDIF
							ELSE
								good2='yes'
							ENDIF
						ENDDO
						CLOSE (UNIT=10)
						CLOSE (UNIT=20)
						DO WHILE (verifdamea(m,1)=='.')
							IF (m==17) THEN
								EXIT
							ELSE
								m=m+1
							ENDIF
						ENDDO
						DO WHILE (verifdamea2(p,1)=='.')
							IF (p==17) THEN
								EXIT
							ELSE
								p=p+1
							ENDIF
						ENDDO
						IF ( p+m-2 == 32 ) THEN
							good='yes'
						ELSE 
							good='no'
							CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
						ENDIF
					ENDIF
				ENDDO
			ENDIF
		ELSE
				good='no'
				PRINT*,''
				PRINT*," ERREUR : Le dame choisi ne peut pas effectuer le mouvement choisi."
				PRINT*,"Règle : Le dame se déplace en diagonale, selon une ligne ou selon une colonne."
				PRINT*,''
				CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
		ENDIF
	ENDIF
!Mouvement roi noir!

	IF ( (entree == 'RoiN') ) THEN
		IF ( placementcol2 == placementcol ) THEN 
			IF ( (lignepiece == lingpiecabouger +1) .OR. (lignepiece == lingpiecabouger - 1)  ) THEN
				IF ( echiquier(lignepiece,placementcol2) == '.' ) THEN
						good='yes'
				ENDIF
				OPEN(UNIT=20, FILE='liste_piece_noir.txt', STATUS = 'OLD', ACTION ='READ')
				DO i = 1,16
					READ(20,*)nompiece
					IF ( echiquier(lignepiece,placementcol2) == nompiece ) THEN
						good2='no'
						verifroi(i,1)=good2
						PRINT*,'Le roi noir choisi ne peut effectuer le mouvement voulu.'
						PRINT*,"Sa position d'arrivée est une pièce noire."
						PRINT*,''
						PRINT*,'Règle : Le roi noir ne peut manger que des pièces blanches.'
						PRINT*,''
					ENDIF
				ENDDO
				CLOSE (UNIT=20)
				DO WHILE (verifroi(m,1)=='.')
					IF (m==17) THEN
						EXIT
					ELSE
						m=m+1
					ENDIF
				ENDDO
				IF ( m-1 == 16) THEN
					good='yes'
				ELSE 
					good='no'
					CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
				ENDIF
			ELSE
				PRINT*,'Le roi noir choisi ne peut effectuer le mouvement voulu.'
				PRINT*,'Règle : Le roi noir ne peut bouger que d une case dans les 4 directions. Il ne peut bouger en diagonale.'
				PRINT*,''
				CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
			ENDIF
		ELSEIF ( lignepiece == lingpiecabouger ) THEN 
			IF ( (placementcol2 == placementcol+1) .OR. (placementcol2 == placementcol-1)) THEN
				IF ( echiquier(lignepiece,placementcol2) == '.' ) THEN
						good='yes'
				ENDIF
				OPEN(UNIT=20, FILE='liste_piece_noir.txt', STATUS = 'OLD', ACTION ='READ')
				DO i = 1,16
					READ(20,*)nompiece
					IF ( echiquier(lignepiece,placementcol2) == nompiece ) THEN
						good2='no'
						verifroi(i,1)=good2
						PRINT*,'Le roi noir choisi ne peut effectuer le mouvement voulu.'
						PRINT*,"Sa position d'arrivée est une pièce noire."
						PRINT*,''
						PRINT*,'Règle : Le roi noir ne peut manger que des pièces blanches.'
						PRINT*,''
					ENDIF
				ENDDO
				CLOSE (UNIT=20)
				DO WHILE (verifroi(m,1)=='.')
					IF (m==17) THEN
						EXIT
					ELSE
						m=m+1
					ENDIF
				ENDDO
				IF ( m-1 == 16) THEN
					good='yes'
				ELSE 
					good='no'
					CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
				ENDIF
			ELSE 
				PRINT*,'Le roi noir choisi ne peut effectuer le mouvement voulu.'
				PRINT*,'Règle : Le roi noir ne peut bouger que d une case dans les 4 directions. Il ne peut bouger en diagonale.'
				PRINT*,''
			ENDIF
		
		ELSEIF ( (lignepiece == lingpiecabouger+1) .AND. (placementcol2 == placementcol+1) ) THEN
				IF ( echiquier(lignepiece,placementcol2) == '.' ) THEN
						good='yes'
				ENDIF
				OPEN(UNIT=20, FILE='liste_piece_noir.txt', STATUS = 'OLD', ACTION ='READ')
				DO i = 1,16
					READ(20,*)nompiece
					IF ( echiquier(lignepiece,placementcol2) == nompiece ) THEN
						good2='no'
						verifroi(i,1)=good2
						PRINT*,'Le roi noir choisi ne peut effectuer le mouvement voulu.'
						PRINT*,"Sa position d'arrivée est une pièce noire."
						PRINT*,''
						PRINT*,'Règle : Le roi noir ne peut manger que des pièces blanches.'
						PRINT*,''
					ENDIF
				ENDDO
				CLOSE (UNIT=20)
				DO WHILE (verifroi(m,1)=='.')
					IF (m==17) THEN
						EXIT
					ELSE
						m=m+1
					ENDIF
				ENDDO
				IF ( m-1 == 16) THEN
					good='yes'
				ELSE 
					good='no'
					CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
				ENDIF

		ELSEIF ( (lignepiece == lingpiecabouger-1) .AND. (placementcol2 == placementcol+1) ) THEN
				IF ( echiquier(lignepiece,placementcol2) == '.' ) THEN
						good='yes'
				ENDIF
				OPEN(UNIT=20, FILE='liste_piece_noir.txt', STATUS = 'OLD', ACTION ='READ')
				DO i = 1,16
					READ(20,*)nompiece
					IF ( echiquier(lignepiece,placementcol2) == nompiece ) THEN
						good2='no'
						verifroi(i,1)=good2
						PRINT*,'Le roi noir choisi ne peut effectuer le mouvement voulu.'
						PRINT*,"Sa position d'arrivée est une pièce noire."
						PRINT*,''
						PRINT*,'Règle : Le roi noir ne peut manger que des pièces blanches.'
						PRINT*,''
					ENDIF
				ENDDO
				CLOSE (UNIT=20)
				DO WHILE (verifroi(m,1)=='.')
					IF (m==17) THEN
						EXIT
					ELSE
						m=m+1
					ENDIF
				ENDDO
				IF ( m-1 == 16) THEN
					good='yes'
				ELSE 
					good='no'
					CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
				ENDIF

		ELSEIF ( (lignepiece == lingpiecabouger-1) .AND. (placementcol2 == placementcol-1) ) THEN
				IF ( echiquier(lignepiece,placementcol2) == '.' ) THEN
						good='yes'
				ENDIF
				OPEN(UNIT=20, FILE='liste_piece_noir.txt', STATUS = 'OLD', ACTION ='READ')
				DO i = 1,16
					READ(20,*)nompiece
					IF ( echiquier(lignepiece,placementcol2) == nompiece ) THEN
						good2='no'
						verifroi(i,1)=good2
						PRINT*,'Le roi noir choisi ne peut effectuer le mouvement voulu.'
						PRINT*,"Sa position d'arrivée est une pièce noire."
						PRINT*,''
						PRINT*,'Règle : Le roi noir ne peut manger que des pièces blanches.'
						PRINT*,''
					ENDIF
				ENDDO
				CLOSE (UNIT=20)
				DO WHILE (verifroi(m,1)=='.')
					IF (m==17) THEN
						EXIT
					ELSE
						m=m+1
					ENDIF
				ENDDO
				IF ( m-1 == 16) THEN
					good='yes'
				ELSE 
					good='no'
					CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
				ENDIF

		ELSEIF ( (lignepiece == lingpiecabouger+1) .AND. (placementcol2 == placementcol-1) ) THEN
				IF ( echiquier(lignepiece,placementcol2) == '.' ) THEN
						good='yes'
				ENDIF
				OPEN(UNIT=20, FILE='liste_piece_noir.txt', STATUS = 'OLD', ACTION ='READ')
				DO i = 1,16
					READ(20,*)nompiece
					IF ( echiquier(lignepiece,placementcol2) == nompiece ) THEN
						good2='no'
						verifroi(i,1)=good2
						PRINT*,'Le roi noir choisi ne peut effectuer le mouvement voulu.'
						PRINT*,"Sa position d'arrivée est une pièce noire."
						PRINT*,''
						PRINT*,'Règle : Le roi noir ne peut manger que des pièces blanches.'
						PRINT*,''
					ENDIF
				ENDDO
				CLOSE (UNIT=20)
				DO WHILE (verifroi(m,1)=='.')
					IF (m==17) THEN
						EXIT
					ELSE
						m=m+1
					ENDIF
				ENDDO
				IF ( m-1 == 16) THEN
					good='yes'
				ELSE 
					good='no'
					CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
				ENDIF
		ELSE
			PRINT*,'Le roi noir choisi ne peut effectuer le mouvement voulu.'
			PRINT*,'Règle : Le roi noir ne peut bouger que d une case dans les 4 directions. Il ne peut bouger en diagonale.'
			PRINT*,''
			CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
		ENDIF
	ENDIF


!Mouvement roi blanc!	 
	IF ( (entree == 'RoiB') ) THEN
		IF ( placementcol2 == placementcol ) THEN 
			IF ( (lignepiece == lingpiecabouger +1) .OR. (lignepiece == lingpiecabouger - 1)  ) THEN
				IF ( echiquier(lignepiece,placementcol2) == '.' ) THEN
						good='yes'
				ENDIF
				OPEN(UNIT=10, FILE='liste_piece_blanche.txt', STATUS = 'OLD', ACTION ='READ')
				DO i = 1,16
					READ(10,*)nompiece
					IF ( echiquier(lignepiece,placementcol2) == nompiece ) THEN
						good2='no'
						verifroi(i,1)=good2
						PRINT*,'Le roi blanc choisi ne peut effectuer le mouvement voulu.'
						PRINT*,"Sa position d'arrivée est une pièce blanche."
						PRINT*,''
						PRINT*,'Règle : Le roi blanc ne peut manger que des pièces noires.'
						PRINT*,''
					ENDIF
				ENDDO
				CLOSE (UNIT=20)
				DO WHILE (verifroi(m,1)=='.')
					IF (m==17) THEN
						EXIT
					ELSE
						m=m+1
					ENDIF
				ENDDO
				IF ( m-1 == 16) THEN
					good='yes'
				ELSE 
					good='no'
					CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
				ENDIF
			ELSE
				PRINT*,'Le roi blanc choisi ne peut effectuer le mouvement voulu.'
				PRINT*,'Règle : Le roi ne peut bouger que d une case dans les 4 directions. Il ne peut bouger en diagonale.'
				PRINT*,''
				CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
			ENDIF
		ELSEIF ( lignepiece == lingpiecabouger ) THEN 
			IF ( (placementcol2 == placementcol+1) .OR. (placementcol2 == placementcol-1)) THEN
				IF ( echiquier(lignepiece,placementcol2) == '.' ) THEN
						good='yes'
				ENDIF
				OPEN(UNIT=10, FILE='liste_piece_blanche.txt', STATUS = 'OLD', ACTION ='READ')
				DO i = 1,16
					READ(10,*)nompiece
					IF ( echiquier(lignepiece,placementcol2) == nompiece ) THEN
						good2='no'
						verifroi(i,1)=good2
						PRINT*,'Le roi blanc choisi ne peut effectuer le mouvement voulu.'
						PRINT*,"Sa position d'arrivée est une pièce noir."
						PRINT*,''
						PRINT*,'Règle : Le roi blanc ne peut manger que des pièces noires.'
						PRINT*,''
					ENDIF
				ENDDO
				CLOSE (UNIT=10)
				DO WHILE (verifroi(m,1)=='.')
					IF (m==17) THEN
						EXIT
					ELSE
						m=m+1
					ENDIF
				ENDDO
				IF ( m-1 == 16) THEN
					good='yes'
				ELSE 
					good='no'
					CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
				ENDIF
			ELSE 
				PRINT*,'Le roi blanc choisi ne peut effectuer le mouvement voulu.'
				PRINT*,'Règle : Le roi noir ne peut bouger que d une case dans les 4 directions. Il ne peut bouger en diagonale.'
				PRINT*,''
			ENDIF
		ELSEIF ( (lignepiece == lingpiecabouger+1) .AND. (placementcol2 == placementcol+1) ) THEN
				IF ( echiquier(lignepiece,placementcol2) == '.' ) THEN
						good='yes'
				ENDIF
				OPEN(UNIT=10, FILE='liste_piece_blanche.txt', STATUS = 'OLD', ACTION ='READ')
				DO i = 1,16
					READ(10,*)nompiece
					IF ( echiquier(lignepiece,placementcol2) == nompiece ) THEN
						good2='no'
						verifroi(i,1)=good2
						PRINT*,'Le roi blanc choisi ne peut effectuer le mouvement voulu.'
						PRINT*,"Sa position d'arrivée est une pièce blanche."
						PRINT*,''
						PRINT*,'Règle : Le roi blanc ne peut manger que des pièces noires.'
						PRINT*,''
					ENDIF
				ENDDO
				CLOSE (UNIT=10)
				DO WHILE (verifroi(m,1)=='.')
					IF (m==17) THEN
						EXIT
					ELSE
						m=m+1
					ENDIF
				ENDDO
				IF ( m-1 == 16) THEN
					good='yes'
				ELSE 
					good='no'
					CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
				ENDIF

		ELSEIF ( (lignepiece == lingpiecabouger-1) .AND. (placementcol2 == placementcol+1) ) THEN
				IF ( echiquier(lignepiece,placementcol2) == '.' ) THEN
						good='yes'
				ENDIF
				OPEN(UNIT=10, FILE='liste_piece_blanche.txt', STATUS = 'OLD', ACTION ='READ')
				DO i = 1,16
					READ(10,*)nompiece
					IF ( echiquier(lignepiece,placementcol2) == nompiece ) THEN
						good2='no'
						verifroi(i,1)=good2
						PRINT*,'Le roi blanc choisi ne peut effectuer le mouvement voulu.'
						PRINT*,"Sa position d'arrivée est une pièce blanche."
						PRINT*,''
						PRINT*,'Règle : Le roi blanc ne peut manger que des pièces noires.'
					ENDIF
				ENDDO
				CLOSE (UNIT=10)
				DO WHILE (verifroi(m,1)=='.')
					IF (m==17) THEN
						EXIT
					ELSE
						m=m+1
					ENDIF
				ENDDO
				IF ( m-1 == 16) THEN
					good='yes'
				ELSE 
					good='no'
					CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
				ENDIF

		ELSEIF ( (lignepiece == lingpiecabouger-1) .AND. (placementcol2 == placementcol-1) ) THEN
				IF ( echiquier(lignepiece,placementcol2) == '.' ) THEN
						good='yes'
				ENDIF
				OPEN(UNIT=10, FILE='liste_piece_blanche.txt', STATUS = 'OLD', ACTION ='READ')
				DO i = 1,16
					READ(10,*)nompiece
					IF ( echiquier(lignepiece,placementcol2) == nompiece ) THEN
						good2='no'
						verifroi(i,1)=good2
						PRINT*,'Le roi blanc choisi ne peut effectuer le mouvement voulu.'
						PRINT*,"Sa position d'arrivée est une pièce blanche."
						PRINT*,''
						PRINT*,'Règle : Le roi blanc ne peut manger que des pièces noires.'
						PRINT*,''
					ENDIF
				ENDDO
				CLOSE (UNIT=10)
				DO WHILE (verifroi(m,1)=='.')
					IF (m==17) THEN
						EXIT
					ELSE
						m=m+1
					ENDIF
				ENDDO
				IF ( m-1 == 16) THEN
					good='yes'
				ELSE 
					good='no'
					CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
				ENDIF

		ELSEIF ( (lignepiece == lingpiecabouger+1) .AND. (placementcol2 == placementcol-1) ) THEN
				IF ( echiquier(lignepiece,placementcol2) == '.' ) THEN
						good='yes'
				ENDIF
				OPEN(UNIT=10, FILE='liste_piece_blanche.txt', STATUS = 'OLD', ACTION ='READ')
				DO i = 1,16
					READ(10,*)nompiece
					IF ( echiquier(lignepiece,placementcol2) == nompiece ) THEN
						good2='no'
						verifroi(i,1)=good2
						PRINT*,'Le roi blanc choisi ne peut effectuer le mouvement voulu.'
						PRINT*,"Sa position d'arrivée est une pièce blanc."
						PRINT*,''
						PRINT*,'Règle : Le roi blanc ne peut manger que des pièces noires.'
					ENDIF
				ENDDO
				CLOSE (UNIT=10)
				DO WHILE (verifroi(m,1)=='.')
					IF (m==17) THEN
						EXIT
					ELSE
						m=m+1
					ENDIF
				ENDDO
				IF ( m-1 == 16) THEN
					good='yes'
				ELSE 
					good='no'
					CALL lavage_affichage(echiquier,mangeblanc,mangenoir,compteurn,compteurb,Tour,blanc,noir)
				ENDIF
		ELSE
			PRINT*,'Le roi blanc choisi ne peut effectuer le mouvement voulu.'
			PRINT*,'Règle : Le roi blanc ne peut bouger que d une case dans les 4 directions. Il ne peut bouger en diagonale.'
			PRINT*,''
		ENDIF
	ENDIF
ELSE
	PRINT*,''
	PRINT*,"Votre tour n'a pas été comptabilisé, vous n'avez pas joué assez vite."
	goodrapide='no'
	good='yes'
ENDIF

ENDDO
!Fin de boucle TANT QUE pour le mouvement de la pièce!



!COMPTEUR DE PIECES MANGE!

!PAR LES BLANCS!	
	IF (changepion == 'ok') THEN
		
	ELSE
		IF ( (good=='yes') .AND. (goodrapide == 'yes') ) THEN
			OPEN(UNIT=10, FILE='liste_piece_noir.txt',STATUS = 'OLD', ACTION ='READ')
			DO i= 1,16
				READ(10,*)nompiece9
				IF (echiquier(lignepiece,placementcol2)==nompiece9) THEN
					mangeblanc(1,compteurb)=nompiece9
					compteurb=compteurb+1
				END IF
			ENDDO
			CLOSE (UNIT=10)
		ENDIF
	ENDIF

!PAR LES NOIRS!	
	IF (changepion == 'ok') THEN
		
	ELSE
		IF ( (good=='yes') .AND. (goodrapide == 'yes') ) THEN
			OPEN(UNIT=20, FILE='liste_piece_blanche.txt',STATUS = 'OLD', ACTION ='READ')
			DO w=1,16
				READ(20,*)nompiece10
				IF (echiquier(lignepiece,placementcol2)==nompiece10) THEN !S il y a une pièce blanche placement d'arrivée!
					mangenoir(1,compteurn)=nompiece10
					compteurn=compteurn+1
				END IF
			ENDDO
			CLOSE (UNIT=20)
		ENDIF
	ENDIF


	IF (changepion == 'ok') THEN
		
	ELSE
		IF ( (good=='yes') .AND. (goodrapide == 'yes') ) THEN
			echiquier(lignepiece,placementcol2)=echiquier(lingpiecabouger,placementcol)
			echiquier(lingpiecabouger,placementcol)='.'
		ENDIF
	ENDIF
PRINT*,''
PRINT*,''	

DO i=1,20
	IF ( mangenoir(1,i) == 'RoiB' ) THEN
		ENDGAME=1
		GAGNE=1
	ENDIF
	IF ( mangeblanc(1,i) == 'RoiN') THEN
		ENDGAME=1
		GAGNE=0
	ENDIF
ENDDO

PRINT*," CLIQUER ENTRER POUR CONTINUER"
READ(*,*)
CALL execute_command_line('clear')

	
!AFFICHAGE!

	DO i=1,9
		PRINT*, echiquier(i,:)
	ENDDO

	PRINT*,''
	PRINT*,'Pièce mangé par les noirs :' 
	PRINT*, mangenoir(1,:)
	PRINT*,''
	PRINT*,'Pièce mangé par les blancs :'
	PRINT*, mangeblanc(1,:)

ENDDO


CALL execute_command_line('clear')
CALL system_clock(t2bis)

duree=(t2bis-t1bis)/1000

PRINT*,'FIN DE LA PARTIE RAPIDE !'
PRINT*,''

IF ( GAGNE == 0) THEN !blanc gagne
	PRINT*,'BRAVO !'
	PRINT*,blanc," GAGNE !"
	PRINT*,'' 
	PRINT*,noir,", DOmmage pour toi..."
	PRINT*,''
	PRINT*," Tu le battras la prochaine fois !! ;))"
ELSEIF (GAGNE == 1) THEN  !noir gagne
	PRINT*,'BRAVO !'
	PRINT*,noir," GAGNE !"
	PRINT*,'' 
	PRINT*,blanc,", DOmmage pour toi..."
	PRINT*," Tu le battras la prochaine fois !! ;))"
ENDIF

PRINT*,''
tour=tour+1
PRINT*,'La partie a duré :', duree ,'secondes,'," et n'a compté pas moins de ",tour, "coups joués !"
PRINT*,''

CALL ecriture(duree,tour,'high_score_R.txt')

ELSE 
	CALL execute_command_line('clear')
	PRINT*,''
	PRINT*,'' 
	PRINT*,"ERREUR : Aucun mode (Rapide ou Normal) a été chosi, relancer le jeu."

ENDIF



!PROCEDURE

CONTAINS

SUBROUTINE ecriture(dureeentree,tourentree,fichier)
IMPLICIT NONE

INTEGER, INTENT(IN) :: dureeentree,tourentree
CHARACTER (LEN=16), INTENT(IN) :: fichier

	OPEN (UNIT=60, FILE=trim(fichier), STATUS = 'UNKNOWN', ACTION ='WRITE',POSITION = 'APPEND')
	write(60,*)dureeentree
	write(60,*)tourentree
	write(60,*)
	CLOSE(UNIT=60)

END SUBROUTINE 



SUBROUTINE lavage_affichage(echiquier1,mangeblanc1,mangenoir1,compteurn1,compteurb1,Tour1,blanc1,noir1)
IMPLICIT NONE

CHARACTER(LEN=18),DIMENSION(9,9),INTENT(IN) :: echiquier1
CHARACTER(LEN=10),DIMENSION(1,16),INTENT(IN) :: mangeblanc1,mangenoir1
CHARACTER (LEN=10),INTENT(IN) :: blanc1,noir1
INTEGER ,INTENT(IN) :: compteurn1,compteurb1,Tour1
INTEGER :: i
	
	PRINT*,''
	PRINT*,'CLIQUER ENTRER POUR CONTINUER'
	READ(*,*)
	CALL execute_command_line('clear')

	DO i=1,9
		PRINT*, echiquier1(i,:)
	ENDDO

	PRINT*,''

	PRINT*,'Pièce mangé par les noirs :' 
	PRINT*, mangenoir(1,:)

	PRINT*,''

	PRINT*,'Pièce mangé par les blancs :'
	PRINT*, mangeblanc(1,:)
	PRINT*,''
	PRINT*,''

	tour2=tour1/2
	IF ( tour2 * 2 == tour1) THEN
		PRINT*,'Au tour de ',trim(noir1), ' (Les noirs)'
	ELSEIF ( tour2 * 2 +1 == tour1) THEN
		PRINT*,'Au tour de ',trim(blanc1), ' (Les blancs)'
	ENDIF
	PRINT*,''

END SUBROUTINE



END PROGRAM Echec




SUBROUTINE pionnoir(piece)
IMPLICIT NONE

CHARACTER (LEN=18), INTENT(OUT) :: piece
INTEGER :: a

CALL execute_command_line('clear')
PRINT*,'Choisissez une pièce à échanger avec le pion :'
PRINT*,'Dame = 1		Fou = 2		Cavalier = 3		Tour = 4'
PRINT*,''
READ(*,*)a


IF (a == 1) THEN
	piece = 'DameN'
ELSEIF (a==2) THEN
	piece = 'FouN1'
ELSEIF ( a==3 ) THEN
	piece = 'CavalierN1'
ELSEIF ( a == 4) THEN
	piece='TourN1'
ENDIF

END SUBROUTINE pionnoir



SUBROUTINE pionblanc(piece)
IMPLICIT NONE

CHARACTER (LEN=18), INTENT(OUT) :: piece
INTEGER :: a

CALL execute_command_line('clear')
PRINT*,'Choisissez une pièce à échanger avec le pion :'
PRINT*,'Dame = 1		Fou = 2		Cavalier = 3		Tour = 4'
PRINT*,''
READ(*,*)a

IF (a == 1) THEN
	piece = 'DameB'
ELSEIF (a==2) THEN
	piece = 'FouB1'
ELSEIF ( a==3 ) THEN
	piece = 'CavalierB1'
ELSEIF ( a == 4) THEN
	piece='TourB1'
ENDIF


END SUBROUTINE pionblanc


FUNCTION aleatoire(itest)
IMPLICIT NONE

INTEGER, INTENT(IN) :: itest
REAL :: aleatoire


aleatoire = rand(itest)
	

END FUNCTION aleatoire



