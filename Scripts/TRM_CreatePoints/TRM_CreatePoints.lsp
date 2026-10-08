(defun TRM:GetLwPolylineVertices (entity / DXFData elevation)
  (setq DXFData  (entget entity))
  (setq elevation (cond ((cdr (assoc 38 DXFData))) (0.0)))
  (mapcar
    '(lambda (pair)
       (list (cadr pair) (caddr pair) elevation)
     )
    (vl-remove-if-not '(lambda (pair) (= (car pair) 10)) DXFData)
  )
)

(defun TRM:GetPolylineVertices (entity / currentVertex data vertices)
	(setq currentVertex (entnext entity))
	(while (and currentVertex
			  (/= (cdr (assoc 0 (setq data (entget currentVertex)))) "SEQEND")
           )
		(setq vertices (cons (cdr (assoc 10 data)) vertices))
		(setq currentVertex (entnext currentVertex))
     )
	(reverse vertices)
)

(defun TRM:GetDXFData (entity / DXFData)
  	(setq entity (nth 0 selection))
	(setq DXFData (entget entity))
	DXFData
)

(defun TRM:GetVertices ( entity / DXFData DXFSubclass)
	; entity DXF data is a list of group codes and values for the selected entity.
    (setq DXFData (entget entity))
    ; ------------------------------------------------------
    ; Basic entity information
    ; ------------------------------------------------------
    (setq DXFSubclass (TRM:GetDXFSubclass DXFData))
	
	(cond
		((= DXFSubclass "AcDbPolyline")
			(TRM:GetLwPolylineVertices entity)
	  	)	
	  	((member DXFSubclass '("AcDb2dPolyline" "AcDb3dPolyline"))
			(TRM:GetPolylineVertices entity)	
	  	)
		(T
			(princ "\nGetVertices error: Selected object is not currently supported.")
			nil
		)
    )
)

(defun TRM:CreateAutoCADPoints (vertices / )
	(mapcar
		'(lambda (vertex)
		   (entmake
				(list
    			'(0 . "POINT")
    			(cons 10 vertex)
  				)
		   )
		)
		vertices
	)
)


(defun c:TRM_CreatePoints (
                           / ss selection entity DXFData DXFSubclass creationOption vertices
                           )
	(princ "\nTRM_CreatePoints V.0.1.0")
	(prompt "\nSelect objects to create points from its vertices.")
	(setq ss (ssget '((0 . "LWPOLYLINE,POLYLINE,CIRCLE,LINE,ARC"))))
    ;| (setq DXFData (TRM:GetDXFData entity))
    (setq DXFSubclass (TRM:GetDXFSubclass DXFData)) |;
  (if ss
	(progn 
		(initget "AUTOCADPOINTS BLOCK CIVIL")
        (setq creationOption (getkword "\nCreate points as [AUTOCADPOINTS/BLOCK/CIVIL] <AUTOCADPOINTS>: "))
		(if (null creationOption)
			(setq creationOption "AUTOCADPOINTS")
		)
		(cond
			((= creationOption "AUTOCADPOINTS")
             (setq i 0 total 0 skipped 0)
             (repeat (sslength ss)
				(setq entity (ssname ss i)
                      vertices (TRM:GetVertices entity)
                )
               	(if vertices
					
              		(setq total (+ total (length (TRM:CreateAutoCADPoints vertices))))
              		(setq skipped (1+ skipped))
            	)
            	(setq i (1+ i))
             )
             (princ (strcat "\nTotal points created: " (itoa total)))
             (princ (strcat "\nTotal objects skipped: " (itoa skipped)))
             (princ "\nPoints created as AutoCAD points.")
            )
			(T
            (princ "\nSelected option is not currently supported.")
            )
        )
    )
  )
  (princ)
)