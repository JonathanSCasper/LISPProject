;;****************************************************************************;;
;* Author: Jackson Altman                                                     *;
;* Project Name: Simplify Function                                            *;
;* Date: 10/21/2018                                                           *;
;;****************************************************************************;;


;;****************************************************************************;;
;* Function: simplify - serves as the start and restart point for             *;
;* simp                                                                       *;
;* if result of simp = term that is passed, simplification is done return s   *;
;* if result of simp != term that was passed, call simplify again             *;
;******************************************************************************;
;* Param1: term - will be passed to simp as acc and the term                  *;
;* Param2: r - a list of rules in the form (((rulex) replacementx))           *;
;******************************************************************************;
;* Return: s - is the result of simp                                          *;
;;****************************************************************************;;
(defun simplify (term r) 
	(let ((s (simp term r '(nil) term)))
		(cond 	((equal s term) s)
				(t (simplify s r))
		)
	)
)

;;****************************************************************************;;
;* Function: simp - manages a bfs(breadth first search) that matches rules    *;
;* to a pattern, then switches out the pattern with a value specified in the  *;
;* rule.                                                                      *;
;******************************************************************************;
;* Param1: term - term will be checked to find any match with the rules       *;
;* Param2: r - a list of rules in the form (((rulex) replacementx))           *;
;* Param3: branchlst - a queue that keeps track of the next branch to be      *;
;* traversed.                                                                 *;
;* Param4: acc(accumulator) -  used for subsitution when a rule is found      *;
;******************************************************************************;
;* Return: acc - returns acc either after a subsitution was made, or with     *;
;* the same value as it started with.                                         *;
;;****************************************************************************;;
(defun simp (term r branchlst acc) 
	(cond 
		((and (null term) (null branchlst)) acc)
		(t (let ((ar (checkrules term r)))
				(if (null ar) 
					(simp (car branchlst) r (append (cdr branchlst) (if (atom term) nil term)) acc)
					(rdc term ar acc)														 
				)
			)
		)
	)
)

;;****************************************************************************;;
;* Function: checkrules - check for any match rules can be applied to a term  *;
;******************************************************************************;
;* Param1: term - term will be checked to find any match with the rules       *;
;* Param2: r - a list of rules in the form (((rule1) replacement1))           *;
;******************************************************************************;
;* Return1: nil - when none of the rules match                                *;
;* Return2: ((rulex) replacementx) - when rulex matches the term              *;
;;****************************************************************************;;
(defun checkrules (term r)	
						(cond 	((null r) nil)
								((match term (caar r)) (if (atom term) (car (resub (cdar r) (match term (caar r)))) (resub (cdar r) (match term (caar r)))))
								(t (if (atom term) nil (checkrules term (cdr r))))
						)
)

;;****************************************************************************;;
;* Function: getbranches                                                      *;
;******************************************************************************;
;* Term: branch - will be defined as a sublist of term in this context        *;
;******************************************************************************;
;* Param1: term - term will be checked to find any match with the rules       *;
;******************************************************************************;
;* Return: nil - if the term has no branches                                  *;
;* Return: list of sublists - the list of all branches in the term               *;
;;****************************************************************************;;
;(defun getbranches (term)	
;						(cond ((or (atom term) (null term)) nil)
;							(t 
;								(if (atom (car term)) (getbranches (cdr term))				 
;									(cons (car term) (getbranches (cdr term))) ;else
;								)
;							)
;						)
;)

;;****************************************************************************;;
;* Function: rdc(reduce)                                                      *;
;******************************************************************************;
;* Param1: srch - term you will be reducing                                   *;
;* Param2: rplc - what you will be replacing that term with                   *;
;* Param3: lst - list you will search for term to reduce                      *;
;******************************************************************************;
;* Return: lst - returns the lst with the reduction completed                 *;
;;****************************************************************************;;
(defun rdc (srch rplc lst) 
	(cond    
		((null lst) lst)
		((equal srch lst) (car rplc))
		((atom (car lst)) (if (equal (car lst) srch) (cons rplc (rdc srch rplc (cdr lst)))
									                 (cons (car lst) (rdc srch rplc (cdr lst)))
								))
		(t (cons (rdc srch rplc (car lst)) (rdc srch rplc (cdr lst))))
	)
)


;;****************************************************************************;;
;* Function: resub(resubsitute)                                               *;
;******************************************************************************;
;* Param1: lst - list that will have values of sublst subsituted into it      *;
;* Param2: sublst - list of pairs that in the form: (rplcx can be a list)     *;
;*                                ((srch1 . rplc1) (srch2 . rplc2))           *;
;******************************************************************************;
;* Return: lst - returns the lst after all subsitutions are done              *;
;;****************************************************************************;;
(defun resub (lst sublst)
	(cond
			((null sublst) lst)
			(t (resub (rep (caar sublst) (cdr (car sublst)) lst) (cdr sublst)))
	)
)

;;****************************************************************************;;
;* Function: rep(replace) -search for srch and replaces it with rplc in       *;
;* the lst, nested instances of srch will also be found and replaced.         *;
;******************************************************************************;
;* Param1: srch - term to be replaced                                         *;
;* Param2: rplc - what you will be replacing srch with                        *;
;* Param3: lst - list you will search for term to replace                     *;
;******************************************************************************;
;* Return: lst - returns the lst with the replacement done                    *;
;;****************************************************************************;;
(defun rep (srch rplc lst) 
	(cond    
		((null lst) lst)
		((atom (car lst)) (if (equal (car lst) srch)
									(cons rplc (rep srch rplc (cdr lst)))
									(cons (car lst) (rep srch rplc (cdr lst)))
								))
		(t (cons (rep srch rplc (car lst)) (rep srch rplc (cdr lst))))
	)
)
















