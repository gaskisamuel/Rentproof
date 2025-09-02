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
(define-constant ERR_ESCROW_NOT_FOUND (err u111))
(define-constant ERR_ESCROW_ALREADY_EXISTS (err u112))
(define-constant ERR_INSUFFICIENT_FUNDS (err u113))
(define-constant ERR_ESCROW_NOT_ACTIVE (err u114))
(define-constant ERR_CLAIM_PERIOD_EXPIRED (err u115))
(define-constant ERR_ALREADY_CLAIMED (err u116))
(define-constant ERR_INVALID_CLAIM_AMOUNT (err u117))

(define-non-fungible-token rentproof-nft uint)

(define-data-var next-nft-id uint u1)
(define-data-var contract-uri (string-ascii 256) "https://rentproof.io/metadata/")
(define-data-var next-dispute-id uint u1)
(define-data-var dispute-resolution-stake uint u1000)
(define-data-var next-escrow-id uint u1)

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

(define-map escrow-records uint {
    lease-id: uint,
    landlord: principal,
    tenant: principal,
    deposit-amount: uint,
    status: (string-ascii 20),
    created-at: uint,
    claim-deadline: uint,
    total-claims: uint,
    released-amount: uint,
    auto-release-date: uint
})

(define-map damage-claims uint {
    escrow-id: uint,
    claimant: principal,
    claim-type: (string-ascii 50),
    description: (string-ascii 500),
    evidence-hash: (string-ascii 64),
    requested-amount: uint,
    status: (string-ascii 20),
    created-at: uint,
    approved-amount: uint,
    processed-at: (optional uint)
})

(define-map escrow-balances uint uint)

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

(define-public (create-escrow 
    (lease-id uint)
    (deposit-amount uint))
    (let ((escrow-id (var-get next-escrow-id))
          (lease-data (unwrap! (map-get? lease-records lease-id) ERR_NOT_FOUND))
          (hold-period u2016))
        
        (asserts! (> deposit-amount u0) ERR_INVALID_CLAIM_AMOUNT)
        (asserts! (is-eq tx-sender (get tenant lease-data)) ERR_UNAUTHORIZED)
        (asserts! (is-none (get-escrow-by-lease lease-id)) ERR_ESCROW_ALREADY_EXISTS)
        (asserts! (>= (stx-get-balance tx-sender) deposit-amount) ERR_INSUFFICIENT_FUNDS)
        
        (try! (stx-transfer? deposit-amount tx-sender (as-contract tx-sender)))
        
        (map-set escrow-records escrow-id {
            lease-id: lease-id,
            landlord: (get landlord lease-data),
            tenant: (get tenant lease-data),
            deposit-amount: deposit-amount,
            status: "active",
            created-at: stacks-block-height,
            claim-deadline: (+ (get lease-end lease-data) u720),
            total-claims: u0,
            released-amount: u0,
            auto-release-date: (+ (get lease-end lease-data) hold-period)
        })
        
        (map-set escrow-balances escrow-id deposit-amount)
        (var-set next-escrow-id (+ escrow-id u1))
        (ok escrow-id)))

(define-public (submit-damage-claim 
    (escrow-id uint)
    (claim-type (string-ascii 50))
    (description (string-ascii 500))
    (evidence-hash (string-ascii 64))
    (requested-amount uint))
    (let ((escrow-data (unwrap! (map-get? escrow-records escrow-id) ERR_ESCROW_NOT_FOUND))
          (available-balance (unwrap! (map-get? escrow-balances escrow-id) ERR_ESCROW_NOT_FOUND)))
        
        (asserts! (is-eq (get status escrow-data) "active") ERR_ESCROW_NOT_ACTIVE)
        (asserts! (is-eq tx-sender (get landlord escrow-data)) ERR_UNAUTHORIZED)
        (asserts! (< stacks-block-height (get claim-deadline escrow-data)) ERR_CLAIM_PERIOD_EXPIRED)
        (asserts! (and (> requested-amount u0) (<= requested-amount available-balance)) ERR_INVALID_CLAIM_AMOUNT)
        
        (let ((claim-id (+ escrow-id u10000)))
            (map-set damage-claims claim-id {
                escrow-id: escrow-id,
                claimant: tx-sender,
                claim-type: claim-type,
                description: description,
                evidence-hash: evidence-hash,
                requested-amount: requested-amount,
                status: "pending",
                created-at: stacks-block-height,
                approved-amount: u0,
                processed-at: none
            })
            
            (map-set escrow-records escrow-id (merge escrow-data {
                total-claims: (+ (get total-claims escrow-data) u1)
            }))
            (ok claim-id))))

(define-public (approve-damage-claim 
    (claim-id uint)
    (approved-amount uint))
    (let ((claim-data (unwrap! (map-get? damage-claims claim-id) ERR_NOT_FOUND))
          (escrow-data (unwrap! (map-get? escrow-records (get escrow-id claim-data)) ERR_ESCROW_NOT_FOUND))
          (available-balance (unwrap! (map-get? escrow-balances (get escrow-id claim-data)) ERR_ESCROW_NOT_FOUND)))
        
        (asserts! (is-eq tx-sender (get tenant escrow-data)) ERR_UNAUTHORIZED)
        (asserts! (is-eq (get status claim-data) "pending") ERR_ALREADY_CLAIMED)
        (asserts! (<= approved-amount (get requested-amount claim-data)) ERR_INVALID_CLAIM_AMOUNT)
        (asserts! (<= approved-amount available-balance) ERR_INSUFFICIENT_FUNDS)
        
        (if (> approved-amount u0)
            (begin
                (try! (as-contract (stx-transfer? approved-amount tx-sender (get claimant claim-data))))
                (map-set escrow-balances (get escrow-id claim-data) (- available-balance approved-amount))
                (map-set escrow-records (get escrow-id claim-data) (merge escrow-data {
                    released-amount: (+ (get released-amount escrow-data) approved-amount)
                })))
            true)
        
        (map-set damage-claims claim-id (merge claim-data {
            status: "approved",
            approved-amount: approved-amount,
            processed-at: (some stacks-block-height)
        }))
        (ok true)))

(define-public (reject-damage-claim (claim-id uint))
    (let ((claim-data (unwrap! (map-get? damage-claims claim-id) ERR_NOT_FOUND))
          (escrow-data (unwrap! (map-get? escrow-records (get escrow-id claim-data)) ERR_ESCROW_NOT_FOUND)))
        
        (asserts! (is-eq tx-sender (get tenant escrow-data)) ERR_UNAUTHORIZED)
        (asserts! (is-eq (get status claim-data) "pending") ERR_ALREADY_CLAIMED)
        
        (map-set damage-claims claim-id (merge claim-data {
            status: "rejected",
            processed-at: (some stacks-block-height)
        }))
        (ok true)))

(define-public (release-remaining-deposit (escrow-id uint))
    (let ((escrow-data (unwrap! (map-get? escrow-records escrow-id) ERR_ESCROW_NOT_FOUND))
          (remaining-balance (unwrap! (map-get? escrow-balances escrow-id) ERR_ESCROW_NOT_FOUND)))
        
        (asserts! (is-eq (get status escrow-data) "active") ERR_ESCROW_NOT_ACTIVE)
        (asserts! (or (is-eq tx-sender (get tenant escrow-data))
                     (>= stacks-block-height (get auto-release-date escrow-data))) ERR_UNAUTHORIZED)
        (asserts! (>= stacks-block-height (get claim-deadline escrow-data)) ERR_CLAIM_PERIOD_EXPIRED)
        
        (if (> remaining-balance u0)
            (begin
                (try! (as-contract (stx-transfer? remaining-balance tx-sender (get tenant escrow-data))))
                (map-set escrow-balances escrow-id u0))
            true)
        
        (map-set escrow-records escrow-id (merge escrow-data {
            status: "completed",
            released-amount: (get deposit-amount escrow-data)
        }))
        (ok true)))

(define-public (emergency-release-escrow (escrow-id uint))
    (let ((escrow-data (unwrap! (map-get? escrow-records escrow-id) ERR_ESCROW_NOT_FOUND))
          (remaining-balance (unwrap! (map-get? escrow-balances escrow-id) ERR_ESCROW_NOT_FOUND)))
        
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (asserts! (is-eq (get status escrow-data) "active") ERR_ESCROW_NOT_ACTIVE)
        
        (if (> remaining-balance u0)
            (try! (as-contract (stx-transfer? remaining-balance tx-sender (get tenant escrow-data))))
            true)
        
        (map-set escrow-balances escrow-id u0)
        (map-set escrow-records escrow-id (merge escrow-data {
            status: "emergency-released",
            released-amount: (get deposit-amount escrow-data)
        }))
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

(define-read-only (get-escrow-record (escrow-id uint))
    (map-get? escrow-records escrow-id))

(define-read-only (get-escrow-balance (escrow-id uint))
    (map-get? escrow-balances escrow-id))

(define-read-only (get-damage-claim (claim-id uint))
    (map-get? damage-claims claim-id))

(define-read-only (get-escrow-by-lease (lease-id uint))
    (get found (fold check-escrow-lease (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10) 
                     { lease-id: lease-id, found: none, current-id: u1 })))

(define-read-only (get-next-escrow-id)
    (var-get next-escrow-id))

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

(define-private (check-escrow-lease (id uint) (acc { lease-id: uint, found: (optional uint), current-id: uint }))
    (if (is-some (get found acc))
        acc
        (let ((escrow-data (map-get? escrow-records (get current-id acc))))
            (if (and (is-some escrow-data) 
                     (is-eq (get lease-id (unwrap-panic escrow-data)) (get lease-id acc)))
                { lease-id: (get lease-id acc), found: (some (get current-id acc)), current-id: (+ (get current-id acc) u1) }
                { lease-id: (get lease-id acc), found: none, current-id: (+ (get current-id acc) u1) }))))








