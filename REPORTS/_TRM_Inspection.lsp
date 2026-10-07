;; ============================================================
;; TRM_Inspect
;; ------------------------------------------------------------
;; Description:
;;     Inspects AutoCAD/Civil 3D polyline entities and reports
;;     their type, vertex count and vertex coordinates.
;;
;; Commands:
;;     TRM_INSPECT
;;     TRM_ISPEZIONE
;;
;; Version:
;;     0.1.0
;;
;; ============================================================


;; ------------------------------------------------------------
;; Helper function: Get 3D Polyline vertices
;; ------------------------------------------------------------

(defun TRM:Get3DPolylineVertices (entity / vertices vertexData vertex)
  
  (setq vertices '())

  ;; Start with the first vertex
  (setq vertexData (entnext entity))

  ;; Traverse all sub-entities
  (while vertexData

    (if (= "VERTEX" (cdr (assoc 0 (entget vertexData))))
      (progn
        (setq vertex (cdr (assoc 10 (entget vertexData))))
        (setq vertices (append vertices (list vertex)))
      )
    )

    ;; Stop at SEQEND
    (if (= "SEQEND" (cdr (assoc 0 (entget vertexData))))
      (setq vertexData nil)
      (setq vertexData (entnext vertexData))
    )
  )

  vertices
)


;; ------------------------------------------------------------
;; Main command
;; ------------------------------------------------------------

(defun c:TRM_INSPECT
  (
    /
    selection
    entity
    entityData
    entityType
    elevation
    vertices
    vertexCount
    index
    vertex
  )

  (princ "\n")
  (princ "==============================================")
  (princ "\nTRM Inspect v0.1.0")
  (princ "\n==============================================")

  ;; Select entity
  (setq selection (entsel "\nSelect an entity to inspect: "))

  ;; Check selection
  (if selection

    (progn

      ;; Get entity name
      (setq entity (car selection))

      ;; Get DXF data
      (setq entityData (entget entity))

      ;; Get entity type
      (setq entityType (cdr (assoc 0 entityData)))

      (princ (strcat "\n\nEntity type: " entityType))


      ;; ------------------------------------------------------
      ;; LWPOLYLINE
      ;; ------------------------------------------------------

      (cond

        ;; LWPOLYLINE
        (
          (= entityType "LWPOLYLINE")

          (setq elevation (cdr (assoc 38 entityData)))

          (princ "\nPolyline type: LWPOLYLINE")

          (if elevation
            (princ
              (strcat
                "\nElevation: "
                (rtos elevation 2 3)
              )
            )
            (princ "\nElevation: 0.000")
          )

          ;; Get vertices
          (setq vertices '())

          (foreach data entityData

            (if (= (car data) 10)
              (setq vertices
                (append
                  vertices
                  (list
                    (list
                      (cadr data)
                      (caddr data)
                      (if elevation elevation 0.0)
                    )
                  )
                )
              )
            )
          )

          ;; Number of vertices
          (setq vertexCount (length vertices))

          (princ
            (strcat
              "\nVertex count: "
              (itoa vertexCount)
            )
          )

          ;; Print vertices
          (setq index 1)

          (foreach vertex vertices

            (princ
              (strcat
                "\n\nVertex "
                (itoa index)
                ":"
                "\n  X = "
                (rtos (car vertex) 2 3)
                "\n  Y = "
                (rtos (cadr vertex) 2 3)
                "\n  Z = "
                (rtos (caddr vertex) 2 3)
              )
            )

            (setq index (1+ index))
          )
        )


        ;; ----------------------------------------------------
        ;; POLYLINE
        ;; ----------------------------------------------------

        (
          (= entityType "POLYLINE")

          ;; Get flags from group code 70
          (setq polylineFlags (cdr (assoc 70 entityData)))

          ;; Check if it is a 3D Polyline
          (if
            (= 8 (logand 8 polylineFlags))

            (progn

              (princ "\nPolyline type: 3D POLYLINE")

              ;; Get vertices
              (setq vertices
                (TRM:Get3DPolylineVertices entity)
              )

              (setq vertexCount (length vertices))

              (princ
                (strcat
                  "\nVertex count: "
                  (itoa vertexCount)
                )
              )

              ;; Print vertices
              (setq index 1)

              (foreach vertex vertices

                (princ
                  (strcat
                    "\n\nVertex "
                    (itoa index)
                    ":"
                    "\n  X = "
                    (rtos (car vertex) 2 3)
                    "\n  Y = "
                    (rtos (cadr vertex) 2 3)
                    "\n  Z = "
                    (rtos (caddr vertex) 2 3)
                  )
                )

                (setq index (1+ index))
              )
            )

            ;; Otherwise it is a regular POLYLINE
            (progn

              (princ "\nPolyline type: 2D POLYLINE")

              (princ
                "\nNote: 2D POLYLINE vertex extraction will be"
              )
              (princ
                "\nimplemented in a later version."
              )
            )
          )
        )


        ;; ----------------------------------------------------
        ;; Other entity
        ;; ----------------------------------------------------

        (
          T

          (princ
            "\nThis entity is not a supported polyline type."
          )
        )
      )
    )

    ;; No selection
    (princ "\nNothing selected.")
  )

  (princ "\n")
  (princ "==============================================")
  (princ "\nInspection finished.")
  (princ "\n==============================================")

  (princ)
)


;; ------------------------------------------------------------
;; Italian alias
;; ------------------------------------------------------------

(defun c:TRM_ISPEZIONE ()
  (c:TRM_INSPECT)
)


(princ "\nTRM_Inspect v0.1.0 loaded.")
(princ)
