;; Maintenance Scheduling Contract
;; Manages repair planning for transportation infrastructure

(define-data-var last-schedule-id uint u0)

(define-map maintenance-schedules
  { schedule-id: uint }
  {
    asset-id: uint,
    planned-date: uint,
    estimated-duration: uint,  ;; in days
    priority: uint,  ;; 1=Low, 2=Medium, 3=High, 4=Critical
    status: uint,    ;; 1=Scheduled, 2=In Progress, 3=Completed, 4=Cancelled
    created-by: principal
  }
)

(define-map asset-schedules
  { asset-id: uint }
  { schedule-ids: (list 20 uint) }
)

(define-read-only (get-schedule (schedule-id uint))
  (map-get? maintenance-schedules { schedule-id: schedule-id })
)

(define-read-only (get-asset-schedules (asset-id uint))
  (default-to { schedule-ids: (list) }
    (map-get? asset-schedules { asset-id: asset-id }))
)

(define-read-only (get-last-schedule-id)
  (var-get last-schedule-id)
)

(define-public (schedule-maintenance
                (asset-id uint)
                (planned-date uint)
                (estimated-duration uint)
                (priority uint))
  (let ((new-schedule-id (+ (var-get last-schedule-id) u1))
        (asset-schedule-list (get-asset-schedules asset-id)))

    ;; Validate priority is between 1 and 4
    (asserts! (and (>= priority u1) (<= priority u4)) (err u400))

    ;; Create new maintenance schedule
    (var-set last-schedule-id new-schedule-id)
    (map-set maintenance-schedules
      { schedule-id: new-schedule-id }
      {
        asset-id: asset-id,
        planned-date: planned-date,
        estimated-duration: estimated-duration,
        priority: priority,
        status: u1,  ;; Scheduled by default
        created-by: tx-sender
      }
    )

    ;; Add to asset's schedule list
    (map-set asset-schedules
      { asset-id: asset-id }
      { schedule-ids: (unwrap-panic (as-max-len?
                                      (append (get schedule-ids asset-schedule-list) new-schedule-id)
                                      u20)) }
    )

    (ok new-schedule-id)
  )
)

(define-public (update-schedule-status (schedule-id uint) (new-status uint))
  (let ((schedule (map-get? maintenance-schedules { schedule-id: schedule-id })))
    (asserts! (is-some schedule) (err u404)) ;; Schedule not found
    (asserts! (and (>= new-status u1) (<= new-status u4)) (err u400)) ;; Invalid status

    (map-set maintenance-schedules
      { schedule-id: schedule-id }
      (merge (unwrap-panic schedule) { status: new-status })
    )
    (ok true)
  )
)

;; Function to get all upcoming maintenance for an asset
(define-read-only (get-upcoming-maintenance (asset-id uint))
  (filter is-upcoming (get schedule-ids (get-asset-schedules asset-id)))
)

;; Helper function to check if maintenance is upcoming
(define-private (is-upcoming (schedule-id uint))
  (match (get-schedule schedule-id)
    schedule (and (is-eq (get status schedule) u1)  ;; Status is Scheduled
                 (> (get planned-date schedule) block-height))
    false
  )
)
