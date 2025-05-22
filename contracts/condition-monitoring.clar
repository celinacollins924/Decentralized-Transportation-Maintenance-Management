;; Condition Monitoring Contract
;; Tracks physical state of transportation infrastructure

(define-data-var last-report-id uint u0)

(define-map condition-reports
  { report-id: uint }
  {
    asset-id: uint,
    condition-score: uint,  ;; 1-10 scale (10 being excellent)
    report-date: uint,
    inspector: principal,
    notes: (string-ascii 500)
  }
)

(define-map asset-latest-report
  { asset-id: uint }
  { report-id: uint }
)

(define-read-only (get-report (report-id uint))
  (map-get? condition-reports { report-id: report-id })
)

(define-read-only (get-asset-latest-report (asset-id uint))
  (match (map-get? asset-latest-report { asset-id: asset-id })
    latest-id (get-report (get report-id latest-id))
    none
  )
)

(define-read-only (get-last-report-id)
  (var-get last-report-id)
)

(define-public (submit-condition-report
                (asset-id uint)
                (condition-score uint)
                (notes (string-ascii 500)))
  (let ((new-report-id (+ (var-get last-report-id) u1)))
    ;; Validate condition score is between 1 and 10
    (asserts! (and (>= condition-score u1) (<= condition-score u10)) (err u400))

    (var-set last-report-id new-report-id)
    (map-set condition-reports
      { report-id: new-report-id }
      {
        asset-id: asset-id,
        condition-score: condition-score,
        report-date: block-height,
        inspector: tx-sender,
        notes: notes
      }
    )

    ;; Update the latest report reference for this asset
    (map-set asset-latest-report
      { asset-id: asset-id }
      { report-id: new-report-id }
    )

    (ok new-report-id)
  )
)

;; Function to check if an asset needs maintenance based on condition score
(define-read-only (needs-maintenance (asset-id uint))
  (match (get-asset-latest-report asset-id)
    report (< (get condition-score report) u6)  ;; Score below 6 needs maintenance
    false
  )
)
