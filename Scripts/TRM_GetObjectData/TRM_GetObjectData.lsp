; ============================================================
; TRM_GetObjectData.lsp
; Version: 0.1.0
; Purpose:
;   Get data from a selected object and report its relevant
;   structural information.
; Supported objects:
;   - LWPOLYLINE
;   - 2D POLYLINE (legacy)
;   - 3D POLYLINE
; Responsibility:
;   Identify and describe the selected object.
; ============================================================
; ------------------------------------------------------------
; TRM:GetDXFSubclass
;
; Searches the DXF data for the Civil/AutoCAD DXFSubclass that
; identifies the actual object.
; ------------------------------------------------------------
(defun TRM:GetDXFSubclass (DXFData)
  (cdr (assoc 100 (reverse DXFData)))
)
; ------------------------------------------------------------
; TRM:GetObjectType
;
; Converts the technical DXF classification into a human-
; readable object type.
; ------------------------------------------------------------
(setq TRM:*ObjectTypesDict*
  '(("AcDbPolyline"   . "LWPOLYLINE")
    ("AcDb2dPolyline" . "2D POLYLINE")
    ("AcDb3dPolyline" . "3D POLYLINE")
    ("AeccDbAlignment" . "Alignment")    
   )
)
(defun TRM:GetObjectType (DXFSubclass / found)
  (setq found (assoc DXFSubclass TRM:*ObjectTypesDict*))
  (if found (cdr found) "Unknown / Unsupported")
)
; ------------------------------------------------------------
; TRM:CountObjectVertices
;
; At the moment, this function counts VERTEX child entities of a legacy POLYLINE.
;
; This function only counts them. It does not extract or
; transform their coordinates.
; ------------------------------------------------------------
(defun TRM:CountObjectVertices (ObjectList / len)
  (setq len (length ObjectList))
  len
)

; ------------------------------------------------------------
; TRM_GetObjectData
;
; Main command.
; ------------------------------------------------------------
(defun c:TRM_GetObjectData (/ selection entity entityDXFData
                         entityDXFType DXFSubclass entityType)
  (prompt "\nTRM_GetObjectData V.0.1.0")
  ; ----------------------------------------------------------
  ; Select object also known as entity in AutoCAD.
  ; ----------------------------------------------------------
  (setq selection (entsel "\nSelect Object: "))
  (if selection
    (progn

      (setq entity     (nth 0 selection))
      (setq entityDXFData (entget entity))
      
      ; ------------------------------------------------------
      ; Basic entity information
      ; ------------------------------------------------------
      (setq entityDXFType
            (cdr (assoc 0 entityDXFData)))
      (setq DXFSubclass
            (TRM:GetDXFSubclass entityDXFData))
      (setq entityType
            (TRM:GetObjectType DXFSubclass))

      ; ------------------------------------------------------
      ; Header
      ; ------------------------------------------------------

      (prompt "\n")
      (prompt "\n=========================================")
      (prompt "\nEntity Information")
      (prompt "\n=========================================")

      ; ------------------------------------------------------
      ; Entity information
      ; ------------------------------------------------------
      (prompt
        (strcat
          "\nDXF Entity type: "
          entityDXFType
        )
      )
      (if DXFSubclass
        (prompt
          (strcat
            "\nDXF Subclass: "
            DXFSubclass
          )
        )
      )
      (prompt
        (strcat
          "\nEntity type: "
          entityType
        )
      )


      ; ------------------------------------------------------
      ; LWPOLYLINE
      ; ------------------------------------------------------

      (if (= DXFSubclass "AcDbPolyline")
        (progn

          ; Global elevation
          (setq elevation
                (cdr (assoc 38 entityDXFData)))

          ; Vertex count
          (setq vertexCount
                (cdr (assoc 90 entityDXFData)))

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

      (if (= DXFSubclass "AcDb2dPolyline")
        (progn
          ; Parent POLYLINE stores the elevation in the Z
          ; component of group code 10 at position 3.
          (setq elevation
                (nth 3 (assoc 10 entityDXFData)))
          (prompt
            (strcat
              "\nGlobal Elevation: "
              (rtos elevation 2 3)
            )
          )
          ; Count child VERTEX entities.
          (setq vertexCount
                (TRM:CountObjectVertices (TRM:GetPolylineVertices entity)))
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

      (if (= DXFSubclass "AcDb3dPolyline")
        (progn
          ; 3D POLYLINE does not use a single global elevation.
          ; Each VERTEX contains its own XYZ coordinates.
          (setq vertexCount
                (TRM:CountObjectVertices (TRM:GetPolylineVertices entity)))

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
  (c:TRM_GetObjectData)
)

; ------------------------------------------------------------
; End of file
; ------------------------------------------------------------

(princ "\nTRM_GetObjectData v0.1.0 loaded.")
(princ)