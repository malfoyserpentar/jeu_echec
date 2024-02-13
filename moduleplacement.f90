Module placement
!Variable!

IMPLICIT NONE

CONTAINS

SUBROUTINE place (col,sortie)

IMPLICIT NONE

Character (LEN=1), INTENT(IN) :: col
Integer :: sortie

	SELECT CASE (col)
		CASE ('A')
			sortie = 1
		CASE ('a')
			sortie = 1	
	
		CASE ('B')
			sortie = 2
		CASE ('b')
			sortie = 2
		
		CASE ('C')
			sortie = 3
		CASE ('c')
			sortie = 3
	
		CASE ('D')
			sortie = 4		
		CASE ('d')
			sortie = 4

		CASE ('E')
			sortie = 5
		CASE ('e')
			sortie = 5

		CASE ('F')
			sortie = 6
		CASE ('f')
			sortie = 6

		CASE ('G')
			sortie = 7
		CASE ('g')
			sortie = 7

		CASE ('H')
			sortie = 8
		CASE ('h')
			sortie = 8

		CASE DEFAULT 
			print*, 'ERREUR, veuillez entrer une lettre entre A et H'
	END SELECT

ENDSUBROUTINE place

END MODULE placement
