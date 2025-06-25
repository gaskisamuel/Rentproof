(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_NOT_FOUND (err u101))
(define-constant ERR_ALREADY_EXISTS (err u102))
(define-constant ERR_INVALID_RATING (err u103))
(define-constant ERR_INVALID_DURATION (err u104))
(define-constant ERR_LEASE_ACTIVE (err u105))

(define-non-fungible-token rentproof-nft uint)

(define-data-var next-nft-id uint u1)
(define-data-var contract-uri (string-ascii 256) "https://rentproof.io/metadata/")

(define-map user-profiles principal {
    total-leases: uint,
    completed-leases: uint,
    average-rating: uint,
    reputation-score: uint,
    is-verified: bool
})

(define-map lease-records uint {
    landlord: principal,
    tenant: principal,
    property-address: (string-ascii 256),
    lease-start: uint,
    lease-end: uint,
    monthly-rent: uint,
    security-deposit: uint,
    is-active: bool,
    completion-status: (string-ascii 50)
})

(define-map lease-ratings uint {
    landlord-rating: uint,
    tenant-rating: uint,
    landlord-review: (string-ascii 500),
    tenant-review: (string-ascii 500),
    rated-by-landlord: bool,
    rated-by-tenant: bool
})

(define-map user-lease-history principal (list 50 uint))

(define-public (create-lease-record 
    (tenant principal)
    (property-address (string-ascii 256))
    (lease-duration uint)
    (monthly-rent uint)
    (security-deposit uint))
    (let ((lease-id (var-get next-nft-id)))
        (asserts! (> lease-duration u0) ERR_INVALID_DURATION)
        (asserts! (> monthly-rent u0) ERR_INVALID_DURATION)
        
        (try! (nft-mint? rentproof-nft lease-id tx-sender))
        
        (map-set lease-records lease-id {
            landlord: tx-sender,
            tenant: tenant,
            property-address: property-address,
            lease-start: stacks-block-height,
            lease-end: (+ stacks-block-height lease-duration),
            monthly-rent: monthly-rent,
            security-deposit: security-deposit,
            is-active: true,
            completion-status: "active"
        })
        
        (update-user-profile tx-sender)
        (update-user-profile tenant)
        (add-lease-to-history tx-sender lease-id)
        (add-lease-to-history tenant lease-id)
        
        (var-set next-nft-id (+ lease-id u1))
        (ok lease-id)))

(define-public (complete-lease (lease-id uint) (completion-status (string-ascii 50)))
    (let ((lease-data (unwrap! (map-get? lease-records lease-id) ERR_NOT_FOUND)))
        (asserts! (or (is-eq tx-sender (get landlord lease-data))
                     (is-eq tx-sender (get tenant lease-data))) ERR_UNAUTHORIZED)
        (asserts! (get is-active lease-data) ERR_NOT_FOUND)
        
        (map-set lease-records lease-id (merge lease-data {
            is-active: false,
            completion-status: completion-status
        }))
        
        (update-completed-leases (get landlord lease-data))
        (update-completed-leases (get tenant lease-data))
        (ok true)))

(define-public (rate-lease 
    (lease-id uint)
    (rating uint)
    (review (string-ascii 500)))
    (let ((lease-data (unwrap! (map-get? lease-records lease-id) ERR_NOT_FOUND))
          (existing-rating (default-to {
              landlord-rating: u0,
              tenant-rating: u0,
              landlord-review: "",
              tenant-review: "",
              rated-by-landlord: false,
              rated-by-tenant: false
          } (map-get? lease-ratings lease-id))))
        
        (asserts! (and (>= rating u1) (<= rating u5)) ERR_INVALID_RATING)
        (asserts! (or (is-eq tx-sender (get landlord lease-data))
                     (is-eq tx-sender (get tenant lease-data))) ERR_UNAUTHORIZED)
        (asserts! (not (get is-active lease-data)) ERR_LEASE_ACTIVE)
        
        (if (is-eq tx-sender (get landlord lease-data))
            (begin
                (asserts! (not (get rated-by-landlord existing-rating)) ERR_ALREADY_EXISTS)
                (map-set lease-ratings lease-id (merge existing-rating {
                    tenant-rating: rating,
                    tenant-review: review,
                    rated-by-landlord: true
                }))
                (update-user-rating (get tenant lease-data) rating))
            (begin
                (asserts! (not (get rated-by-tenant existing-rating)) ERR_ALREADY_EXISTS)
                (map-set lease-ratings lease-id (merge existing-rating {
                    landlord-rating: rating,
                    landlord-review: review,
                    rated-by-tenant: true
                }))
                (update-user-rating (get landlord lease-data) rating)))
        (ok true)))

(define-public (verify-user (user principal))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (let ((profile (get-user-profile user)))
            (map-set user-profiles user (merge profile { is-verified: true }))
            (ok true))))

(define-public (set-contract-uri (new-uri (string-ascii 256)))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (var-set contract-uri new-uri)
        (ok true)))

(define-read-only (get-lease-record (lease-id uint))
    (map-get? lease-records lease-id))

(define-read-only (get-lease-rating (lease-id uint))
    (map-get? lease-ratings lease-id))

(define-read-only (get-user-profile (user principal))
    (default-to {
        total-leases: u0,
        completed-leases: u0,
        average-rating: u0,
        reputation-score: u0,
        is-verified: false
    } (map-get? user-profiles user)))

(define-read-only (get-user-lease-history (user principal))
    (default-to (list) (map-get? user-lease-history user)))

(define-read-only (get-reputation-score (user principal))
    (let ((profile (get-user-profile user)))
        (+ (* (get average-rating profile) u20)
           (* (get completed-leases profile) u5)
           (if (get is-verified profile) u50 u0))))

(define-read-only (get-contract-uri)
    (var-get contract-uri))

(define-read-only (get-token-uri (token-id uint))
    (ok (some (concat (var-get contract-uri) (uint-to-ascii token-id)))))

(define-read-only (get-owner (token-id uint))
    (ok (nft-get-owner? rentproof-nft token-id)))

(define-read-only (get-last-token-id)
    (ok (- (var-get next-nft-id) u1)))

(define-private (update-user-profile (user principal))
    (let ((current-profile (get-user-profile user)))
        (map-set user-profiles user (merge current-profile {
            total-leases: (+ (get total-leases current-profile) u1)
        }))))

(define-private (update-completed-leases (user principal))
    (let ((current-profile (get-user-profile user)))
        (map-set user-profiles user (merge current-profile {
            completed-leases: (+ (get completed-leases current-profile) u1)
        }))))

(define-private (update-user-rating (user principal) (new-rating uint))
    (let ((current-profile (get-user-profile user))
          (total-completed (get completed-leases current-profile))
          (current-avg (get average-rating current-profile)))
        (if (is-eq total-completed u0)
            (map-set user-profiles user (merge current-profile {
                average-rating: new-rating
            }))
            (let ((new-average (/ (+ (* current-avg total-completed) new-rating) 
                                 (+ total-completed u1))))
                (map-set user-profiles user (merge current-profile {
                    average-rating: new-average
                }))))))

(define-private (add-lease-to-history (user principal) (lease-id uint))
    (let ((current-history (get-user-lease-history user)))
        (map-set user-lease-history user 
            (unwrap-panic (as-max-len? (append current-history lease-id) u50)))))

(define-private (uint-to-ascii (value uint))
    (if (<= value u9)
        (unwrap-panic (element-at "0123456789" value))
        (get r (fold int-to-ascii-fold 
                    (list u1 u1 u1 u1 u1 u1 u1 u1 u1 u1)
                    { v: value, r: "" }))))

(define-private (int-to-ascii-fold (i uint) (data { v: uint, r: (string-ascii 10) }))
    (if (> (get v data) u0)
        {
            v: (/ (get v data) u10),
            r: (unwrap-panic (as-max-len? 
                (concat (unwrap-panic (element-at "0123456789" (mod (get v data) u10))) 
                        (get r data)) u10))
        }
        data))