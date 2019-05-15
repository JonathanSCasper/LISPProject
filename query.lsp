;Author: Jackson Altman
;Project: Deductive Database Program
;Date: 12/1/2018

;;*************************************************************;;
;* Function ?: Uses backwards chaining to answer db questions. *:
;* ? will also determine if query is a ?var or ?const query    *;
;* Param1 - query: statement that is being evaluated           *;
;* Return - all values that fit the variable or a yes/no       *;
;;*************************************************************;;
(defun ? (query) 
	(if (isVarQuery query)
		(?var query db '(nil))
		(?const query db nil)
	)
)

;;*************************************************************;;
;* Function ?var handles any queries containing variables      *:
;* Param1 query - query passed from ? function                 *:
;* Param2 tempdb - variable db that is cycled through, always  *:
;* initialized to db in query.                                 *:
;* Param3 answer - an accumulator to gather the answer         *:
;;*************************************************************;;
(defun ?var (query tempdb answer)	
	(cond 
		((null tempdb) answer)
		(t (let ((sublst (unify  (cadar tempdb) query)))
				(cond 	
					((null sublst) (?var query (cdr tempdb) answer))
					((atom (caar tempdb)) (?var query (cdr tempdb) (condappend answer (list (cdar sublst)))))
					(t (let ((branch (? (applysub sublst (caar tempdb)))))
							(cond 
								((equal branch 'YES) (?var query (cdr tempdb) (condappend answer (list (getappliedsub sublst query)))))
								((equal branch 'NO) (?var query (cdr tempdb) answer))
								(t (?var query (cdr tempdb) (condappend answer branch)))
							);end if2						
						);end let2
					);end catchall2
				);end if
			);end let
		);end catchall
	)
)

;;*************************************************************;;
;* Function ?const handles any queries without variables       *:
;* Param1 query - query passed from ? function                 *:
;* Param2 tempdb - variable db that is cycled through, always  *:
;* initialized to db in query.                                 *:
;* Param3 answer - an accumulator to gather the answer         *:
;;*************************************************************;;
(defun ?const (query tempdb answer)	
	(cond 
		((null tempdb) 'NO)
		(answer 'YES)
		(t (let ((sublst (unify  (cadar tempdb) query)))
				(cond 	
					((null sublst) (?const query (cdr tempdb) nil))
					((atom (caar tempdb)) 'YES)
					(t (let ((branch (?const (applysub sublst (caar tempdb)) db nil)))
							(if (equal branch 'YES)
								branch
								(?const query (cdr tempdb) nil)
							);end if2						
						);end let2
					);end catchall2
				);end if
			);end let
		);end catchall
	)
)

;;*************************************************************;;
;* Function: getappliedsub - find the variable that was a part *:
;* of the query, then moves onto finding its corresponding     *;
;* value.                                                      *;
;* Param subst - list of all subsituions that were made        *:
;* Param query - the original query containing a variable      *:
;* Returns - value of the replacement value in the original    *:
;* query.                                                      *;
;;*************************************************************;;
(defun getappliedsub (subst query)
	(cond
		((null query) 'error)
		((isvar (car query) varlist) (findreplacement subst (car query))) 
		(t (getappliedsub (cdr query) subst))
	)
)

;;*************************************************************;;
;* Function: findreplacement - finds corresponding subst value *:
;* Param subst - list of all subsituions that were made        *:
;* Param var - variable to find the subst for                  *:
;* Returns - corresponding subst value                         *:
;;*************************************************************;;
(defun findreplacement (subst var)
	(cond
		((null subst) 'YES)
		((equal (caar subst) var) (cdar subst))
		(t (getappliedsub query (cdr subst)))
	)
)




;;*************************************************************;;
;* Function : condappend- appends the correct value depending  *:
;* on which list contains nil to avoid extra nils in  result   *;
;* Params two lists to be appended without any nils             :
;;*************************************************************;;
(defun condappend (lst1 lst2)
		(cond
			((null (car lst1)) lst2)
			((and (null lst1) (null lst2)) nil)
			((and lst1 (null lst2)) lst1)
			((and (null lst1) lst2) lst2)
			(t (append lst1 lst2))
		)
)

;;*************************************************************;;
;* Function isVarQuery - determines if query contains a variable
;* Param: query - the query in question*:
;* Return: true if contains a variable, false otherwise*:
;;*************************************************************;;
(defun isVarQuery (query) 
	(cond 
		((null query) nil)
		((member (car query) varlist) t)
		(t (isVarQuery (cdr query)))
	)
)

;==============================================================================
; function applysub
;
;   This function will accept a substitution list and a term, then apply all
;   the substituions to that term, returning the term.
;==============================================================================
(defun applysub (subst term)
  (if (equal subst '(nil)) term
      (do* ((subs subst (cdr subs)) (sub (car subs) (car subs)))
           ((null subs) term)
        (setq term (applyone sub term))
      )
  )
)

;==============================================================================
; function applyone
;
;   This function will accept a substitution and a term and apply that
;   substitution to the term, returning the term.
;==============================================================================
(defun applyone (sub term)
  (cond ((null term) nil)
        ((atom term) (if (eq term (car sub)) (cdr sub) term))
        (t (cons (applyone sub (car term)) (applyone sub (cdr term))))
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
(setq varlist '(u v w x y z x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20))
(defun isvar (v vars)
		(cond
			((null vars) nil)
			((listp v) nil)
			((eq v (car vars)) t)
			(t (isvar v (cdr vars)))
		)
)
