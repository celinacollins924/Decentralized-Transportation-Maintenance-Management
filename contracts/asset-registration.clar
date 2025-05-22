;; Asset Registration Contract
;; Records transportation infrastructure assets

(define-data-var last-asset-id uint u0)

;; Asset types: 1=Road, 2=Bridge, 3=Tunnel, 4=Railway, 5=Airport
(define-map assets
  { asset-id: uint }
  {
    asset-type: uint,
    name: (string-ascii 100),
    location: (string-ascii 100),
    installation-date: uint,
    owner: principal,
    status: uint  ;; 1=Active, 2=Under Maintenance, 3=Decommissioned
  }
)

(define-read-only (get-asset (asset-id uint))
  (map-get? assets { asset-id: asset-id })
)

(define-read-only (get-last-asset-id)
  (var-get last-asset-id)
)

(define-public (register-asset
                (asset-type uint)
                (name (string-ascii 100))
                (location (string-ascii 100))
                (installation-date uint))
  (let ((new-asset-id (+ (var-get last-asset-id) u1)))
    (var-set last-asset-id new-asset-id)
    (map-set assets
      { asset-id: new-asset-id }
      {
        asset-type: asset-type,
        name: name,
        location: location,
        installation-date: installation-date,
        owner: tx-sender,
        status: u1  ;; Active by default
      }
    )
    (ok new-asset-id)
  )
)

(define-public (update-asset-status (asset-id uint) (new-status uint))
  (let ((asset (map-get? assets { asset-id: asset-id })))
    (asserts! (is-some asset) (err u404)) ;; Asset not found
    (asserts! (is-eq tx-sender (get owner (unwrap-panic asset))) (err u403)) ;; Not authorized
    (asserts! (and (>= new-status u1) (<= new-status u3)) (err u400)) ;; Invalid status

    (map-set assets
      { asset-id: asset-id }
      (merge (unwrap-panic asset) { status: new-status })
    )
    (ok true)
  )
)

(define-public (transfer-ownership (asset-id uint) (new-owner principal))
  (let ((asset (map-get? assets { asset-id: asset-id })))
    (asserts! (is-some asset) (err u404)) ;; Asset not found
    (asserts! (is-eq tx-sender (get owner (unwrap-panic asset))) (err u403)) ;; Not authorized

    (map-set assets
      { asset-id: asset-id }
      (merge (unwrap-panic asset) { owner: new-owner })
    )
    (ok true)
  )
)
