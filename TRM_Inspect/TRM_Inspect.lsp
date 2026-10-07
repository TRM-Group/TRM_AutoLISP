; ============================================================
; TRM_Inspect.lsp
; Version: 0.1.0
;
; Purpose:
;   Inspect a selected object and report its relevant
;   structural information.
;
; Supported objects:
;   - LWPOLYLINE
;   - 2D POLYLINE
;   - 3D POLYLINE
;
; Responsibility:
;   Identify and describe the selected object.
;
; Does NOT:
;   - extract vertices
;   - create COGO points
;   - modify the drawing
; ============================================================


; ------------------------------------------------------------
; TRM:GetSubclass
;
; Searches the DXF data for the Civil/AutoCAD subclass that
; identifies the actual object.
; ------------------------------------------------------------
(defun TRM:GetSubclass (entityData / pair)
  (setq pair (assoc 100 (reverse entityData)))
  (if pair (cdr pair))
)

; ------------------------------------------------------------
; TRM:GetPolylineType
;
; Converts the technical DXF classification into a human-
; readable polyline type.
; ------------------------------------------------------------




(setq TRM:*PolylineTypes*
  '(("AcDbPolyline"   . "LWPOLYLINE")
    ("AcDb2dPolyline" . "2D POLYLINE")
    ("AcDb3dPolyline" . "3D POLYLINE")
    ("AeccDbAlignment" . "Alignment")    
   )
)

(defun TRM:GetPolylineType (subclass / found)
  (setq found (assoc subclass TRM:*PolylineTypes*))
  (if found (cdr found) "Unknown / Unsupported")
)


; ------------------------------------------------------------
; TRM:CountPolylineVertices
;
; Counts VERTEX child entities of a legacy POLYLINE.
;
; This function only counts them. It does not extract or
; transform their coordinates.
; ------------------------------------------------------------

(defun TRM:CountPolylineVertices (entity / current count)

  (setq current (entnext entity))
  (setq count 0)

  (while current

    (cond

      ; Vertex found
      (
        (= (cdr (assoc 0 (entget current))) "VERTEX")

        (setq count (1+ count))
      )

      ; End of polyline
      (
        (= (cdr (assoc 0 (entget current))) "SEQEND")

        (setq current nil)
      )
    )

    (if current
      (setq current (entnext current))
    )
  )

  count
)


; ------------------------------------------------------------
; TRM_INSPECT
;
; Main command.
; ------------------------------------------------------------

(defun c:TRM_INSPECT (/ selection entity entityData
                         entityType subclass polylineType
                         elevation vertexCount)

  (prompt "\nTRM_INSPECT V.0.1.0 - Select an entity...")

  ; ----------------------------------------------------------
  ; Select entity
  ; ----------------------------------------------------------
  
  (setq selection (entsel "\nSelect Entity: "))

  (if selection
    (progn

      (setq entity     (nth 0 selection))
      (setq entityData (entget entity))
      ; ------------------------------------------------------
      ; Basic entity information
      ; ------------------------------------------------------

      (setq entityType
            (cdr (assoc 0 entityData)))

      (setq subclass
            (TRM:GetSubclass entityData))

      (setq polylineType
            (TRM:GetPolylineType subclass))


      ; ------------------------------------------------------
      ; Header
      ; ------------------------------------------------------

      (prompt "\n")
      (prompt "\n----------------------------------------")
      (prompt "\nTRM_INSPECT")
      (prompt "\n----------------------------------------")


      ; ------------------------------------------------------
      ; Entity information
      ; ------------------------------------------------------

      (prompt
        (strcat
          "\nEntity type: "
          entityType
        )
      )

      (if subclass
        (prompt
          (strcat
            "\nSubclass: "
            subclass
          )
        )
      )

      (prompt
        (strcat
          "\nPolyline type: "
          polylineType
        )
      )


      ; ------------------------------------------------------
      ; LWPOLYLINE
      ; ------------------------------------------------------

      (if (= subclass "AcDbPolyline")
        (progn

          ; Global elevation
          (setq elevation
                (cdr (assoc 38 entityData)))

          ; Vertex count
          (setq vertexCount
                (cdr (assoc 90 entityData)))

          (if elevation
            (prompt
              (strcat
                "\nElevation: "
                (rtos elevation 2 3)
              )
            )
          )

          (if vertexCount
            (prompt
              (strcat
                "\nVertex count: "
                (itoa vertexCount)
              )
            )
          )
        )
      )


      ; ------------------------------------------------------
      ; Legacy 2D POLYLINE
      ; ------------------------------------------------------

      (if (= subclass "AcDb2dPolyline")
        (progn

          ; Parent POLYLINE stores the elevation in the Z
          ; component of group code 10.
          (setq elevation
                (caddr (assoc 10 entityData)))

          ; Count child VERTEX entities.
          (setq vertexCount
                (TRM:CountPolylineVertices entity))

          (prompt
            (strcat
              "\nElevation: "
              (rtos elevation 2 3)
            )
          )

          (prompt
            (strcat
              "\nVertex count: "
              (itoa vertexCount)
            )
          )
        )
      )


      ; ------------------------------------------------------
      ; 3D POLYLINE
      ; ------------------------------------------------------

      (if (= subclass "AcDb3dPolyline")
        (progn

          ; 3D POLYLINE does not use a single global elevation.
          ; Each VERTEX contains its own XYZ coordinates.

          (setq vertexCount
                (TRM:CountPolylineVertices entity))

          (prompt
            "\nElevation: per vertex"
          )

          (prompt
            (strcat
              "\nVertex count: "
              (itoa vertexCount)
            )
          )
        )
      )


      ; ------------------------------------------------------
      ; Footer
      ; ------------------------------------------------------

      (prompt "\n----------------------------------------")

    )

    ; --------------------------------------------------------
    ; Nothing selected
    ; --------------------------------------------------------

    (prompt "\nNo entity selected.")
  )

  (princ)
)


; ------------------------------------------------------------
; Italian alias
; ------------------------------------------------------------

(defun c:TRM_ISPEZIONE ()
  (c:TRM_INSPECT)
)


; ------------------------------------------------------------
; End of file
; ------------------------------------------------------------

(princ "\nTRM_Inspect v0.1.0 loaded.")
(princ)