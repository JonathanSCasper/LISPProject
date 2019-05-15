;;***********************************************************;;
;* Program: Unify                                            *;
;* Author: Jackson Altman                                    *;
;* Date: November 7, 2018                                    *;
;;***********************************************************;;

;;***********************************************************;;
;* Function: unify                                           *;
;* Param1: term1 - first term being unified                  *;
;* Param2: term2 - second term being unified                 *;
;* Return: returns the unifying subsitution                  *;
;* of the two given terms.                                   *;
;;***********************************************************;;
(defun unify (term1 term2) 
	(unify1 term1 term2 '(nil))
)

;;***********************************************************;;
;* Function: unify1 - adds a sublst which keeps track of all *;
;* subsitutions being made.                                  *;
;* Param1: term1 - first term being unified                  *;
;* Param2: term2 - second term being unified                 *;
;* Param2: term2 - second term being unified                 *;
;* Return: returns the unifying subsitution                  *;
;* of the two given terms.                                   *;
;;***********************************************************;;
(defun unify1 (term1 term2 sublst) 
	(cond   ((and (null term1) (null term2)) sublst)
			((or (null term1) (null term2)) nil)
			((equal term1 term2) sublst)
			((isvar term1 varlist) 	(list (cons term1 term2))) 
			((isvar term2 varlist) 	(list (cons term2 term1)))
			((or (atom term2) (atom term1)) nil)
			(t (let ((sigmacar (unify (car term1) (car term2))))
				(if (null sigmacar) nil 					 
					(unify1 (subsitute (cdr term1) (condappend sublst sigmacar)) (subsitute (cdr term2) (condappend sublst sigmacar)) (backsub sublst sigmacar)) 
				);Closing of if statement
			);Closing the let with sigmacar
			);Closing of catchall cond last cond object. 
	)
)

;;***********************************************************;;
;* Function: backsub                                         *;
;* Param1: sublst - list of subsitutions already made to     *;
;* the terms.                                                *;
;* Param2: newsub - list of the new subsitution being made   *;
;* to the terms.                                             *;
;* Return: returns a list of all the subsitutions            *;
;* including the new subsitution with backsubsitution        *;
;* applied.                                                  *;
;;***********************************************************;;
(defun backsub (sublst newsub)
	(cond
		((null sublst) newsub)
		((null (car sublst)) newsub)
		((null (car newsub)) sublst)
		((equal (caar newsub) (cdr (car sublst))) (append (list (cons (caar sublst) (cdr (car newsub)))) (backsub (cdr sublst) newsub)))
		(t (append (list (car sublst)) (backsub (cdr sublst) newsub)))
	)
)

;;***********************************************************;;
;* Setq varlist defines a list of variables for the program. *;
;* Function: isvar                                           *;
;* Param1: v the variable in question                        *;
;* Param2: vars a list containing all potential variables.   *;
;* Return: returns bool t/nil if v is contained in the       *;
;* list of predefined variables.                             *;
;;***********************************************************;;
(setq varlist '(u v w x y z))
(defun isvar (v vars)
		(cond
			((null vars) nil)
			((listp v) nil)
			((eq v (car vars)) t)
			(t (isvar v (cdr vars)))
		)
)

;;***********************************************************;;
;* Function: condappend - special append function used to    *;
;* avoid extra nil results at certain points in the result.  *;
;* Param1: sublst - the current list of subsitutions         *;
;* Param2: sigmacar - the subsitution being appended         *;
;* Return: the list containing sublst and sigmacar           *;
;* after being formatted properly.                           *;
;;***********************************************************;;
(defun condappend (sublst sigmacar)
		(cond
			((null (car sigmacar)) sublst)
			((and (null (car sublst)) (car sigmacar)) sigmacar)
			(t (append sublst sigmacar))
		)
)

;;***********************************************************;;
;* Function: subsitute                                       *;
;* Param1: lst - list sublst will be applied to.             *;
;* Param2: sublst - list of the subsitutions to apply to     *;
;* the lst.                                                  *;
;* Return: returns the list after all subsitutions have      *;
;* been applied to every level of the lst.                   *;
;;***********************************************************;;
(defun subsitute (lst sublst)
	(cond
			((null sublst) lst)
			(t (subsitute (myreplace (car (car sublst)) (cdr (car sublst)) lst) (cdr sublst)))
	)
)

;;***********************************************************;;
;* Function: myreplace                                       *;
;* Param1: srch - term being searched for in lst             *;
;* Param2: rplc - term to put in the place of srch           *;
;* Param3: lst - list being searched through                 *;
;* Return: list after all replacements are made.             *;
;;***********************************************************;;
(defun myreplace (srch rplc lst) 
	(cond    
		((null lst) lst)
		((atom (car lst)) (if (equal (car lst) srch)
									(cons rplc (myreplace srch rplc (cdr lst)))
									(cons (car lst) (myreplace srch rplc (cdr lst)))
								))
		(t (cons (myreplace srch rplc (car lst)) (myreplace srch rplc (cdr lst))))
	)
)