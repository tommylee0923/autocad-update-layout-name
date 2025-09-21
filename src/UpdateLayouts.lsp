;----------------------------------------------------------------------
; UpdateLayouts.lsp
; Author:     Tommy Lee
; Date:       2025-09-20
; Version:    0.3.0
; Purpose:    Renames layouts to match sheet number + drawing title
; Notes:      - Assumes one title block per layout
;             - Requires values in the attribute tags configured below
;             - Requires the following configuration variables at the top of this file:
;			          - *TB_BLOCK_NAME* → your title block name
;			          - *SHEET_TAG* → attribute tag for sheet number
;			          - *TITLE_TAGS* → list of attribute tags for drawing title lines
; Change Log:
;   v0.3.0    - Add prompt to rename Current layout or All layouts (new command flow).
;   v0.2.0    - Add command to rename all layouts at once; add helpers that accept layout objects.
;   v0.1.0    - Initial public release with config-driven tags.
;----------------------------------------------------------------------

(vl-load-com)

;----------------------------------------------------------------------
; Configuration (EDIT THESE TO MATCH YOUR TITLE BLOCK)
;----------------------------------------------------------------------
(setq *TB_BLOCK_NAME* "TITLE_BLOCK_NAME") 
(setq *SHEET_TAG*     "SHEET_NUMBER")     
(setq *TITLE_TAGS*    '("TITLE_LINE_1"    
                        "TITLE_LINE_2"    
                        "TITLE_LINE_3"    
                        "TITLE_LINE_4"))


;----------------------------------------------------------------------
; Helpers
;----------------------------------------------------------------------
(defun _trim (s) (vl-string-trim "\t\r\n" (if s s "")))
(defun _compare (a b) (= (strcase a) (strcase b)))

(defun _getSheetNumberOnLayout (blkName attTag / currLayout sset idx entity obj val att)
  (setq currLayout (getvar "CTAB"))
  (if
    (setq sset (ssget "X"
                      (list '(0 . "INSERT")
                            (cons 410 currLayout))))
    (progn
      (setq idx 0)
      (while (and (< idx (sslength sset)) (not val))
        (setq entity (ssname sset idx)
              obj    (vlax-ename->vla-object entity))
        (if (and (eq (vla-get-ObjectName obj) "AcDbBlockReference")
                 (equal
                   (strcase (if (vlax-property-available-p obj 'EffectiveName)
                              (vla-get-EffectiveName obj)
                              (vla-get-Name obj)))
                   (strcase blkName)))
          (if (vlax-method-applicable-p obj 'GetAttributes)
            (foreach att (vlax-invoke obj 'GetAttributes)
              (if (equal (strcase (vla-get-TagString att)) (strcase attTag))
                (setq val (vla-get-TextString att))))))
        (setq idx (1+ idx)))
      val)
    ;; else
    nil))

(defun _getTitleFromTagList (tagList blkName / acad doc lay layBlk br att table txt acc)
  (setq acad   (vlax-get-acad-object)
        doc    (vla-get-ActiveDocument acad)
        lay    (vla-Item (vla-get-Layouts doc) (getvar 'CTAB))
        layBlk (vla-get-Block lay)
        acc    '())
  (vlax-for br layBlk
    (if (and (eq (vla-get-ObjectName br) "AcDbBlockReference")
             (equal
               (strcase (if (vlax-property-available-p br 'EffectiveName)
                          (vla-get-EffectiveName br)
                          (vla-get-Name br)))
               (strcase blkName))
             (vla-get-HasAttributes br)
             (null acc))
      (progn
        (setq table (mapcar
                      '(lambda (a)
                         (cons (strcase (vla-get-TagString a))
                               (_trim (vla-get-TextString a))))
                      (vlax-safearray->list
                        (vlax-variant-value (vla-GetAttributes br)))))
        (foreach tag tagList
          (setq txt (cdr (assoc (strcase tag) table)))
          (if (and txt (/= txt "")) (setq acc (append acc (list txt))))))))
  (if acc
    (vl-string-trim " " (apply 'strcat (mapcar '(lambda (s) (strcat s " ")) acc)))
    nil))

(defun _getTitleString (/ title)
  (setq title (_getTitleFromTagList *TITLE_TAGS* *TB_BLOCK_NAME*))
  title)

(defun _getSheetNumberOnLayoutObj (lay blkName attTag / layBlk val br att bName aTag)
  (setq layBlk (vla-get-Block lay)
        val    nil
        blkName (strcase blkName)
        attTag  (strcase attTag)
  )
  (vlax-for br layBlk
    (if (and (eq (vla-get-ObjectName br) "AcDbBlockReference")
             (null val)) ;; stop at first match
      (progn
        (setq bName (if (vlax-property-available-p br 'EffectiveName)
                      (vla-get-EffectiveName br)
                      (vla-get-Name br)))
        (if (equal (strcase bName) blkName)
          (if (vla-get-HasAttributes br)
            (foreach att (vlax-safearray->list (vlax-variant-value (vla-GetAttributes br)))
              (setq aTag (strcase (vla-get-TagString att)))
              (if (equal aTag attTag)
                (progn (setq val (_trim (vla-get-TextString att)))
                       (if (= val "") (setq val nil)))
              )
            )
          )
        )
      )
    )
  )
  val
)

(defun _getTitleFromTagListFromLayObj (lay tagList blkName / acad doc lay layBlk br att table txt acc) 
  (setq layBlk (vla-get-Block lay)
        acc    '()
  )
	
  (vlax-for br layBlk 
    (if 
      (and (eq (vla-get-ObjectName br) "AcDbBlockReference") 
               (equal 
                 (strcase 
                   (if (vlax-property-available-p br 'EffectiveName) 
                     (vla-get-EffectiveName br)
                     (vla-get-Name br)
                   )
                 )
                 (strcase blkName)
				) 
           (vla-get-HasAttributes br)
           (null acc)
      ) 
      (progn 
       
        (setq table (mapcar 
                      '(lambda (a) 
                         (cons (strcase (vla-get-TagString a)) 
                               (_trim (vla-get-TextString a))
                         )
                       )
                      (vlax-safearray->list (vlax-variant-value (vla-GetAttributes br)))
                    )
        )

       
        (foreach tag tagList 
          (setq txt (cdr (assoc (strcase tag) table)))
          (if (and txt (/= txt "")) (setq acc (append acc (list txt))))
        )
      )
    )
	
  (if acc
    (vl-string-trim " " (apply 'strcat (mapcar '(lambda (s) (strcat s " ")) acc)))
    nil
  )
)
)
;----------------------------------------------------------------------
; Main Commands
;----------------------------------------------------------------------
(defun _updateCurrentLayoutName (/ shtnum title newName oldName acadApp doc layouts layObj)
  (setq shtnum  (_getSheetNumberOnLayout *TB_BLOCK_NAME* *SHEET_TAG*)
        title   (_getTitleString)
        newName (strcat (if shtnum shtnum "") " " (if title title "")))
  (if (and newName (/= (setq newName (vl-string-trim " \t\r\n" newName)) ""))
    (progn
      (setq oldName (getvar "CTAB"))
      (if (equal (strcase oldName) "MODEL")
        (prompt "\nError: You cannot rename the Model tab.")
        (progn
          (setq acadApp (vlax-get-acad-object)
                doc     (vla-get-ActiveDocument acadApp)
                layouts (vla-get-Layouts doc))
          (if (not (equal (strcase oldName) (strcase newName)))
            (progn
              ;; check if target name already exists
              (if (not (vl-catch-all-error-p
                         (vl-catch-all-apply 'vla-Item (list layouts newName))))
                (prompt (strcat "\nError: A layout named \"" newName "\" already exists."))
                (progn
                  ;; rename the current layout object
                  (setq layObj (vla-Item layouts oldName))
                  (if (vl-catch-all-error-p
                        (vl-catch-all-apply 'vla-put-Name (list layObj newName)))
                    (prompt "\nError: Failed to rename layout.")
                    (prompt (strcat "\nLayout renamed from \"" oldName "\" to \"" newName "\".")))))))
            (prompt "\nThe layout name is already correct; no renaming needed."))))
    (prompt "\nError: Could not find a valid sheet number in the title block."))
  (princ))

(defun _updateAllLayoutName (/ shtnum title newName oldName acad 
                                               doc layouts L
                                              ) 
  (setq
		acad (vlax-get-acad-object)
		doc (vla-get-ActiveDocument acad)
		layouts (vla-get-Layouts doc)
	)
  (vlax-for L layouts
	
	  (setq shtnum  (_getSheetNumberOnLayoutObj L *TB_BLOCK_NAME* *SHEET_TAG*)
			title   (_getTitleFromTagListFromLayObj L *TITLE_TAGS* *TB_BLOCK_NAME*)
			newName (strcat (if shtnum shtnum "") " " (if title title ""))
			oldName (vla-get-Name L)
	  )
	  (if (and newName (/= (setq newName (_trim newName)) "")) 
		(progn 
		
		  (if (= :vlax-false (vla-get-ModelType L))
			(progn 
			  (if (not (equal (strcase oldName) (strcase newName))) 
				(progn 
				  ;; check if target name already exists
				  (if 
					(not 
					  (vl-catch-all-error-p 
						(vl-catch-all-apply 'vla-Item (list layouts newName))
					  )
					)
					(prompt 
					  (strcat "\nError: A layout named \"" 
							  newName
							  "\" already exists."
					  )
					)
					(progn 
					  ;; rename the current layout object
					  (if 
						(vl-catch-all-error-p 
						  (vl-catch-all-apply 'vla-put-Name (list L newName))
						)
						(prompt "\nError: Failed to rename layout.")
						(prompt 
						  (strcat "\nLayout renamed from \"" oldName "\" to \"" newName 
								  "\"."
						  )
						)
					  )
					)
				  )
				)
				(prompt "\nThe layout name is already correct; no renaming needed.")
			  )
			)
			;(prompt "\nYou cannot rename the Model tab.")
		  )
		)
		(prompt "\nError: Could not find a valid sheet number or title in the title block.")
	  )
  )
  (princ)
)

(defun c:UpdateLayouts ( / option)
  (initget "All Current")
  (setq option (getkword "\nWhich layout(s) do you want to rename? [All/Current] <All>: "))
  
  (if (null option) (setq option "All"))
  
  (cond
    ((= option "All")
      (princ "\nRenaming all layouts...")
      (_updateAllLayoutName)     
    )
    ((= option "Current")
      (princ "\nRenaming current layout...")
      (_updateCurrentLayoutName)     
    )
  )
  (princ)
)