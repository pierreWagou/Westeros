; --- Implémentation des bases de données ---
(setq *BR* '( ; base de règles
  (R1 ((valeur pouvoir) (armee dragon)) (maison targaryen))
  (R2 ((valeur honneur) (religion ancien)) (maison stark))
  (R3 ((valeur fierte) (religion sel)) (maison greyjoy))
  (R4 ((religion rouge) (cheveu brun)) (maison baratheon))
  (R5 ((fortune riche) (cheveu blond)) (maison lannister))
  (R6 ((valeur pouvoir) (strategie manipulation)) (maison tyrel))
  (R7 ((objectif vengeance) (boussole sud)) (maison martel))
  (R8 ((religion nouveau) (boussole ouest)) (maison tully))
  (R9 ((valeur famille) (boussole est)) (maison aryn))

  (R10 ((objectif regne) (noblesse haute)) (valeur pouvoir))
  (R11 ((arme epee) (strategie guerre)) (valeur fierte))
  (R12 ((objectif vengeance)) (valeur fierte))
  (R13 ((armee banneret) (ordre chevalier)) (valeur honneur))
  (R14 ((caractere brave) (objectif protection)) (valeur honneur))
  (R15 ((objectif protection)) (valeur famille))
  (R16 ((religion nouveau)) (valeur famille))

  (R17 ((armee bateau) (objectif conquete)) (religion sel))
  (R18 ((strategie guerre) (paysage mer)) (religion sel))
  (R19 ((boussole nord) (objectif protection)) (religion ancien))
  (R20 ((boussole nord) (objectif protection)) (religion ancien))
  (R21 ((hobby forniquer) (arme magie)) (religion rouge))
  (R22 ((ordre pretre) (cheveux roux)) (religion rouge))
  (R23 ((arme laser)) (religion nouveau))

  (R24 ((hobby boire) (boisson biere)) (boussole nord))
  (R25 ((caractere humble) (fortune pauvre)) (boussole nord))
  (R26 ((hobby forniquer)) (boussole sud))
  (R27 ((hobby boire) (boisson vin)) (boussole sud))
  (R28 ((paysage montagne)) (boussole est))
  (R29 ((paysage campagne)) (boussole ouest))

  (R30 ((boisson pampryl) (paysage ville)) (fortune riche))
  (R31 ((boisson vin) (hobby forniquer)) (fortune riche))
  (R32 ((caractere humble) (noblesse basse)) (fortune pauvre))

  (R33 ((boisson eau) (hobby sport)) (armee bateau))
  (R34 ((paysage mer)) (armee bateau))
  (R35 ((strategie conquete) (noblesse haute)) (armee dragon))
  (R36 ((strategie sympathie)) (armee banneret))
  (R37 ((fortune riche) (strategie guerre)) (armee mercenaire))
  (R38 ((fortune riche) (noblesse basse)) (armee mercenaire))

  (R39 ((caractere ambitieux) (ordre conseil)) (strategie manipulation))
  (R40 ((arme arc) (noblesse basse)) (strategie manipulation))
  (R41 ((ordre pretre)) (strategie neutre))
  (R42 ((ordre mestre)) (strategie neutre))
  (R43 ((ordre chevalier)) (strategie guerre))
  (R44 ((armee mercenaire) (noblesse basse)) (strategie guerre))
  (R45 ((armee banneret) (noblesse haute)) (strategie geurre))
  (R46 ((boisson biere)) (strategie sympathie))

  (R47 ((caractere ambitieux) (arme epee)) (objectif conquete))
  (R48 ((strategie guerre)) (objectif conquete))
  (R49 ((paysage montagne) (strategie neutre)) (objectif protection))
  (R50 ((arme arc) (paysage ville)) (objectif protection))
  (R51 ((strategie manipulation) (ordre conseil)) (objectif regne))
  (R52 ((noblesse haute) (hobby reve)) (objectif regne))
  (R53 ((hobby boire) (arme epee)) (objectif vengeance))
  (R54 ((strategie manipulation) (arme arc)) (objectif vengeance))

  (R55 ((boussole sud)) (cheveu brun))
  (R56 ((noblesse haute)) (cheveu blond))
  (R57 ((arme magie)) (cheveu brun))

  (R58 ((caractere brave) (arme epee)) (ordre chevalier))
  (R59 ((hobby sport)) (ordre chevalier))
  (R60 ((caractere sage) (hobby reve)) (ordre mestre))
  (R61 ((caractere humble) (fortune basse)) (ordre pretre))
  (R62 ((caractere sage) (paysage ville)) (ordre conseil))

  (R63 ((paysage ville)) (noblesse haute))
  (R64 ((fortune riche)) (noblesse haute))
  (R65 ((paysage campagne)) (noblesse basse))
  (R66 ((fortune pauvre)) (noblesse basse))
))

(setq *BF* nil) ; base de faits

(setq *BB* nil) ; base de buts

(setq *EF* '( ; etats finaux
  (maison targaryen)
  (maison stark)
  (maison lannister)
  (maison greyjoy)
  (maison baratheon)
  (maison tyrel)
  (maison martel)
  (maison tully)
  (maison aryn)
))

; --- Fonction de services ---
(defun spot (item liste) ; recherche item (attribut valeur) dans liste
  (let ((result nil))
    (dolist (x liste result)
      (if (and (equal (car x) (car item)) (equal (cadr x) (cadr item)))
        (setq result x)
      )
    )
  )
)

(defun listeRegles () ; construction liste des clés des règles
  (mapcar #'car *BR*)
)

(defun premisse (regle) ; récupération des premisses d'une règle
  (cadr (assoc regle *BR*))
)

(defun but (regle) ; récupération du but d'une règle
  (caddr (assoc regle *BR*))
)

; --- Moteurs d'inférence
(defun verifPremissesAvant (premisses)
  (let ((result t))
    (dolist (x premisses result) ; parcours des prémisses de la règle
      (if (not (spot x *BF*)) ; vérification de leur présence dans la base de faits
        (setq result nil)
      )
    )
  )
)

(defun reglesCandidatesAvant (regles)
  (let ((result nil))
    (dolist (x regles result) ; parcours des règles
      (if (and (verifPremissesAvant (premisse x)) (not (spot (but x) *BF*))) ; vérifcation de la présence de tous les prémisses et du but de la règle dans la base de règle
        (push x result) ; règle retenue
      )
    )
  )
)

(defun verifPremissesArriere (premisses)
  (let ((result nil))
    (dolist (x premisses result) ; ; parcours des prémisses de la règles
      (if (not (spot x *BB*)) ; vérification de leur présence dans la base de buts
        (setq result t)
      )
    )
  )
)

(defun reglesCandidatesArriere (regles)
  (let ((result nil))
    (dolist (x regles result) ; parcours des règles
      (if (and (verifPremissesArriere (premisse x)) (spot (but x) *BB*)) ; vérification de la présence de tous les prémisses et du but de la règle dans la base de buts
        (push x result)
      )
    )
  )
)

(defun majBF (regles)
  (let ((candidats (reglesCandidatesAvant regles)))
    (dolist (x candidats) ; parcours des règles candidates
      (push (but x) *BF*) ; mise à jour de la base de faits
    )
  )
)

(defun majBB (regles)
  (let ((candidats (reglesCandidatesArriere regles)))
    (dolist (x candidats) ; parcours des règles candidates
      (dolist (y (premisse x)) ; parcours des prémisses d'une règle candidate
        (if (not (spot y *BB*))
          (push y *BB*) ; mise à jour de la base de buts
        )
      )
    )
  )
)

(defun allegeanceAvant ()
  (let ((result nil))
    (dolist (x *BF* result) ; parcours des faits
      (if (spot x *EF*) ; vérification de présence d'une maison dans les faits
        (push x result) ; maissons assignables
      )
    )
  )
)

(defun allegeanceArriere ()
  (let ((result t))
    (dolist (x *BF* result) ; parcours des faits
      (if (not (spot x *BB*)) ; vérification de présence d'un fait initial dans la base des buts
        (setq result nil)
      )
    )
  )
)

(defun moteurAvant ()
  (let ((maison (cadr (car (allegeanceAvant)))))
    (if (not (null maison)) ; vérification de l'appartenance à une maison
     (format t "Tu appartiens a la maison ~A" maison)
     (if (null (reglesCandidatesAvant (listeRegles))) ; cas de la garde de la nuit
        (format t "Tu appartiens a la garde de la nuit.")
        (let ((regles (listeRegles)))
          (majBF regles) ; mises à jour de la base de faits
          (moteurAvant)
        )
      )
    )
  )
)

(defun moteurArriere ()
  (let ((result nil))
    (dolist (x *EF*) ; parcours des états finaux
      (setq *BB* nil) ; initialisation de la base de buts
      (push x *BB*)
      (sousMoteur)
      (if (not (null (allegeanceArriere))) ; vérification de l'appartenance à une maison
        (setq result t)
      )
    )
    (if (null result) ; cas de la garde de la nuit
      (format t "Tu appartiens a la garde de la nuit.")
    )
  )
)

(defun sousMoteur ()
  (if (not (null (allegeanceArriere))) ; vérification de l'appartenance à une maison
    (format t "Tu appartiens a la maison ~A" (cadr (car(last *BB*))))
    (if (null (reglesCandidatesArriere (listeRegles))) ; raisonnement dans une impasse
      (setq *BB* nil)
      (let ((regles (listeRegles)))
        (majBB regles) ; mises à jour de la base de buts
        (sousMoteur)
      )
    )
  )
)

; --- Scenarios ---
(defun scenario1 ()
  (setq *BF* '(
    (boisson biere)
    (arme epee)
    (caractere brave)
    (paysage mer)
    (hobby forniquer)
  ))
  (moteurAvant)
 )

(defun scenario2 ()
  (setq *BF* '(
    (paysage ville)
    (caractere ambitieux)
    (hobby reve)
    (arme arc)
    (boisson vin)
  ))
  (moteurArriere)
 )

(defun scenario3 ()
  (setq *BF* '(
    (boisson vin)
    (arme magie)
    (caractere sage)
    (paysage ville)
    (hobby boire)
  ))
  (moteurAvant)
 )

(defun scenario4 ()
  (setq *BF* '(
    (boisson vin)
    (arme arc)
    (caractere ambitieux)
    (paysage ville)
    (hobby forniquer)
  ))
  (moteurArriere)
 )

(defun scenario5 ()
  (setq *BF* '(
    (boisson vin)
    (arme laser)
    (caractere sage)
    (paysage ville)
    (hobby forniquer)
  ))
  (moteurAvant)
 )

; --- Execution ---
(scenario1)
(scenario2)
(scenario3)
(scenario4)
(scenario5)
