(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_NOT_FOUND (err u101))
(define-constant ERR_ALREADY_EXISTS (err u102))
(define-constant ERR_INVALID_RATING (err u103))
(define-constant ERR_INVALID_DURATION (err u104))
(define-constant ERR_LEASE_ACTIVE (err u105))
(define-constant ERR_DISPUTE_NOT_FOUND (err u106))
(define-constant ERR_DISPUTE_CLOSED (err u107))
(define-constant ERR_ALREADY_VOTED (err u108))
(define-constant ERR_INSUFFICIENT_STAKE (err u109))
(define-constant ERR_DISPUTE_STILL_ACTIVE (err u110))

(define-non-fungible-token rentproof-nft uint)

(define-data-var next-nft-id uint u1)
(define-data-var contract-uri (string-ascii 256) "https://rentproof.io/metadata/")
(define-data-var next-dispute-id uint u1)
(define-data-var dispute-resolution-stake uint u1000)

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

(define-map dispute-records uint {
    dispute-type: (string-ascii 50),
    complainant: principal,
    respondent: principal,
    lease-id: uint,
    description: (string-ascii 1000),
    evidence-hash: (string-ascii 64),
    status: (string-ascii 20),
    created-at: uint,
    resolution-deadline: uint,
    resolved-at: (optional uint),
    resolution: (string-ascii 500),
    complainant-satisfied: (optional bool),
    respondent-satisfied: (optional bool)
})

(define-map dispute-votes uint {
    total-votes: uint,
    favor-complainant: uint,
    favor-respondent: uint,
    neutral: uint
})

(define-map user-dispute-votes { user: principal, dispute-id: uint } {
    vote: (string-ascii 20),
    stake-amount: uint,
    voted-at: uint
})

(define-map user-dispute-history principal (list 20 uint))

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

(define-public (create-dispute 
    (dispute-type (string-ascii 50))
    (respondent principal)
    (lease-id uint)
    (description (string-ascii 1000))
    (evidence-hash (string-ascii 64)))
    (let ((dispute-id (var-get next-dispute-id))
          (lease-data (unwrap! (map-get? lease-records lease-id) ERR_NOT_FOUND))
          (resolution-blocks u1440))
        
        (asserts! (or (is-eq tx-sender (get landlord lease-data))
                     (is-eq tx-sender (get tenant lease-data))) ERR_UNAUTHORIZED)
        (asserts! (or (is-eq respondent (get landlord lease-data))
                     (is-eq respondent (get tenant lease-data))) ERR_UNAUTHORIZED)
        (asserts! (not (is-eq tx-sender respondent)) ERR_UNAUTHORIZED)
        
        (map-set dispute-records dispute-id {
            dispute-type: dispute-type,
            complainant: tx-sender,
            respondent: respondent,
            lease-id: lease-id,
            description: description,
            evidence-hash: evidence-hash,
            status: "open",
            created-at: stacks-block-height,
            resolution-deadline: (+ stacks-block-height resolution-blocks),
            resolved-at: none,
            resolution: "",
            complainant-satisfied: none,
            respondent-satisfied: none
        })
        
        (map-set dispute-votes dispute-id {
            total-votes: u0,
            favor-complainant: u0,
            favor-respondent: u0,
            neutral: u0
        })
        
        (add-dispute-to-history tx-sender dispute-id)
        (add-dispute-to-history respondent dispute-id)
        
        (var-set next-dispute-id (+ dispute-id u1))
        (ok dispute-id)))

(define-public (vote-on-dispute 
    (dispute-id uint)
    (vote (string-ascii 20))
    (stake-amount uint))
    (let ((dispute-data (unwrap! (map-get? dispute-records dispute-id) ERR_DISPUTE_NOT_FOUND))
          (current-votes (unwrap! (map-get? dispute-votes dispute-id) ERR_DISPUTE_NOT_FOUND))
          (vote-key { user: tx-sender, dispute-id: dispute-id })
          (min-stake (var-get dispute-resolution-stake)))
        
        (asserts! (is-eq (get status dispute-data) "open") ERR_DISPUTE_CLOSED)
        (asserts! (< stacks-block-height (get resolution-deadline dispute-data)) ERR_DISPUTE_CLOSED)
        (asserts! (>= stake-amount min-stake) ERR_INSUFFICIENT_STAKE)
        (asserts! (is-none (map-get? user-dispute-votes vote-key)) ERR_ALREADY_VOTED)
        (asserts! (not (is-eq tx-sender (get complainant dispute-data))) ERR_UNAUTHORIZED)
        (asserts! (not (is-eq tx-sender (get respondent dispute-data))) ERR_UNAUTHORIZED)
        (asserts! (or (is-eq vote "favor-complainant") 
                     (is-eq vote "favor-respondent") 
                     (is-eq vote "neutral")) ERR_INVALID_RATING)
        
        (map-set user-dispute-votes vote-key {
            vote: vote,
            stake-amount: stake-amount,
            voted-at: stacks-block-height
        })
        
        (let ((new-total (+ (get total-votes current-votes) u1)))
            (if (is-eq vote "favor-complainant")
                (map-set dispute-votes dispute-id (merge current-votes {
                    total-votes: new-total,
                    favor-complainant: (+ (get favor-complainant current-votes) u1)
                }))
                (if (is-eq vote "favor-respondent")
                    (map-set dispute-votes dispute-id (merge current-votes {
                        total-votes: new-total,
                        favor-respondent: (+ (get favor-respondent current-votes) u1)
                    }))
                    (map-set dispute-votes dispute-id (merge current-votes {
                        total-votes: new-total,
                        neutral: (+ (get neutral current-votes) u1)
                    })))))
        (ok true)))

(define-public (resolve-dispute 
    (dispute-id uint)
    (resolution (string-ascii 500)))
    (let ((dispute-data (unwrap! (map-get? dispute-records dispute-id) ERR_DISPUTE_NOT_FOUND))
          (vote-data (unwrap! (map-get? dispute-votes dispute-id) ERR_DISPUTE_NOT_FOUND)))
        
        (asserts! (is-eq (get status dispute-data) "open") ERR_DISPUTE_CLOSED)
        (asserts! (>= stacks-block-height (get resolution-deadline dispute-data)) ERR_DISPUTE_STILL_ACTIVE)
        (asserts! (or (is-eq tx-sender CONTRACT_OWNER)
                     (is-eq tx-sender (get complainant dispute-data))
                     (is-eq tx-sender (get respondent dispute-data))) ERR_UNAUTHORIZED)
        
        (let ((winning-decision (determine-winning-vote vote-data)))
            (map-set dispute-records dispute-id (merge dispute-data {
                status: "resolved",
                resolved-at: (some stacks-block-height),
                resolution: resolution
            })))
        (ok true)))

(define-public (mark-dispute-satisfaction 
    (dispute-id uint)
    (is-satisfied bool))
    (let ((dispute-data (unwrap! (map-get? dispute-records dispute-id) ERR_DISPUTE_NOT_FOUND)))
        
        (asserts! (is-eq (get status dispute-data) "resolved") ERR_DISPUTE_NOT_FOUND)
        (asserts! (or (is-eq tx-sender (get complainant dispute-data))
                     (is-eq tx-sender (get respondent dispute-data))) ERR_UNAUTHORIZED)
        
        (if (is-eq tx-sender (get complainant dispute-data))
            (map-set dispute-records dispute-id (merge dispute-data {
                complainant-satisfied: (some is-satisfied)
            }))
            (map-set dispute-records dispute-id (merge dispute-data {
                respondent-satisfied: (some is-satisfied)
            })))
        (ok true)))

(define-public (set-dispute-stake (new-stake uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (var-set dispute-resolution-stake new-stake)
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

(define-read-only (get-dispute-record (dispute-id uint))
    (map-get? dispute-records dispute-id))

(define-read-only (get-dispute-votes (dispute-id uint))
    (map-get? dispute-votes dispute-id))

(define-read-only (get-user-dispute-vote (user principal) (dispute-id uint))
    (map-get? user-dispute-votes { user: user, dispute-id: dispute-id }))

(define-read-only (get-user-dispute-history (user principal))
    (default-to (list) (map-get? user-dispute-history user)))

(define-read-only (get-dispute-stake-requirement)
    (var-get dispute-resolution-stake))

(define-read-only (get-next-dispute-id)
    (var-get next-dispute-id))

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

(define-private (add-dispute-to-history (user principal) (dispute-id uint))
    (let ((current-history (get-user-dispute-history user)))
        (map-set user-dispute-history user 
            (unwrap-panic (as-max-len? (append current-history dispute-id) u20)))))

(define-private (determine-winning-vote (vote-data { total-votes: uint, favor-complainant: uint, favor-respondent: uint, neutral: uint }))
    (let ((complainant-votes (get favor-complainant vote-data))
          (respondent-votes (get favor-respondent vote-data))
          (neutral-votes (get neutral vote-data)))
        (if (> complainant-votes respondent-votes)
            (if (> complainant-votes neutral-votes)
                "favor-complainant"
                "neutral")
            (if (> respondent-votes neutral-votes)
                "favor-respondent"
                "neutral"))))

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