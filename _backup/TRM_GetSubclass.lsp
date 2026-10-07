(defun TRM:GetSubclass (entityData / pair subclass)

  (foreach pair entityData
    (if (= (car pair) 100)
      (setq subclass (cdr pair))
    )
  )

  subclass
)


(defun TRM:GetSubclass (entityData)
  (cdr (last (vl-remove-if-not '(lambda (p) (= (car p) 100)) entityData)))
)


(defun TRM:GetPolylineType (subclass)
  (cond
    ; Lightweight Polyline
    (
      (and (= subclass "AcDbPolyline"))
      "LWPOLYLINE"
    )
    ; Legacy 2D Polyline
    (
      (and (= subclass "AcDb2dPolyline"))
      "2D POLYLINE"
    )
    ; 3D Polyline
    (
      (and (= subclass "AcDb3dPolyline"))
      "3D POLYLINE"
    )
    ; Unknown / unsupported
    (T "Unknown / Unsupported")
  )
)