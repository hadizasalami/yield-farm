;; Yield Farm - Regional Stablecoin Protocol
;; Implements tri-asset algorithmic stablecoin with geographic value anchoring

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-insufficient-balance (err u101))
(define-constant err-invalid-region (err u102))
(define-constant err-insufficient-collateral (err u103))
(define-constant err-invalid-amount (err u104))
(define-constant err-not-authorized (err u105))
(define-constant err-oracle-not-found (err u106))

;; Regional identifiers
(define-constant REGION-US u1)
(define-constant REGION-EU u2)
(define-constant REGION-AS u3)

;; Collateralization ratio (150% = 150)
(define-constant min-collateral-ratio u150)

;; Data Variables
(define-data-var protocol-active bool true)
(define-data-var total-value-locked uint u0)
(define-data-var stability-pool-balance uint u0)

;; Data Maps

;; Regional stablecoin balances (MESA-US, MESA-EU, MESA-AS)
(define-map regional-balances
    { user: principal, region: uint }
    { balance: uint }
)

;; Collateral deposits by user
(define-map collateral-deposits
    { user: principal }
    { amount: uint, region: uint }
)

;; Regional price oracles (price in micro-units)
(define-map regional-oracles
    { region: uint }
    { price: uint, last-update: uint }
)

;; Yield vault balances by region
(define-map regional-vaults
    { region: uint }
    { balance: uint, total-yield: uint }
)

;; Stability mining rewards
(define-map stability-rewards
    { user: principal }
    { accumulated: uint, last-claim: uint }
)

;; User positions for overcollateralization tracking
(define-map user-positions
    { user: principal, region: uint }
    { collateral: uint, debt: uint }
)

;; Authorization map for oracle operators
(define-map oracle-operators
    { operator: principal }
    { authorized: bool }
)

;; Read-only functions

(define-read-only (get-balance (user principal) (region uint))
    (default-to 
        { balance: u0 }
        (map-get? regional-balances { user: user, region: region })
    )
)

(define-read-only (get-collateral (user principal))
    (default-to 
        { amount: u0, region: u0 }
        (map-get? collateral-deposits { user: user })
    )
)

(define-read-only (get-regional-price (region uint))
    (default-to 
        { price: u1000000, last-update: u0 }
        (map-get? regional-oracles { region: region })
    )
)

(define-read-only (get-vault-balance (region uint))
    (default-to 
        { balance: u0, total-yield: u0 }
        (map-get? regional-vaults { region: region })
    )
)

(define-read-only (get-user-position (user principal) (region uint))
    (default-to 
        { collateral: u0, debt: u0 }
        (map-get? user-positions { user: user, region: region })
    )
)

(define-read-only (calculate-collateral-ratio (user principal) (region uint))
    (let (
        (position (get-user-position user region))
        (collateral-val (get collateral position))
        (debt-val (get debt position))
    )
        (if (is-eq debt-val u0)
            u0
            (/ (* collateral-val u100) debt-val)
        )
    )
)

(define-read-only (get-protocol-status)
    {
        active: (var-get protocol-active),
        tvl: (var-get total-value-locked),
        stability-pool: (var-get stability-pool-balance)
    }
)

(define-read-only (is-oracle-operator (operator principal))
    (default-to 
        false
        (get authorized (map-get? oracle-operators { operator: operator }))
    )
)

;; Public functions

;; Deposit collateral
(define-public (deposit-collateral (amount uint) (region uint))
    (begin
        (asserts! (var-get protocol-active) err-not-authorized)
        (asserts! (> amount u0) err-invalid-amount)
        (asserts! (or (is-eq region REGION-US) 
                     (or (is-eq region REGION-EU) 
                         (is-eq region REGION-AS))) err-invalid-region)
        
        (let (
            (current-deposit (get-collateral tx-sender))
            (new-amount (+ (get amount current-deposit) amount))
        )
            (map-set collateral-deposits
                { user: tx-sender }
                { amount: new-amount, region: region }
            )
            (var-set total-value-locked (+ (var-get total-value-locked) amount))
            (ok true)
        )
    )
)

;; Mint regional stablecoin
(define-public (mint-stablecoin (amount uint) (region uint))
    (begin
        (asserts! (var-get protocol-active) err-not-authorized)
        (asserts! (> amount u0) err-invalid-amount)
        (asserts! (or (is-eq region REGION-US) 
                     (or (is-eq region REGION-EU) 
                         (is-eq region REGION-AS))) err-invalid-region)
        
        (let (
            (user-collateral (get-collateral tx-sender))
            (current-balance (get-balance tx-sender region))
            (position (get-user-position tx-sender region))
            (new-debt (+ (get debt position) amount))
            (collateral-amount (get amount user-collateral))
            (new-ratio (/ (* collateral-amount u100) new-debt))
        )
            (asserts! (>= new-ratio min-collateral-ratio) err-insufficient-collateral)
            
            (map-set regional-balances
                { user: tx-sender, region: region }
                { balance: (+ (get balance current-balance) amount) }
            )
            
            (map-set user-positions
                { user: tx-sender, region: region }
                { collateral: (get collateral position), debt: new-debt }
            )
            
            (ok true)
        )
    )
)

;; Burn stablecoin to reduce debt
(define-public (burn-stablecoin (amount uint) (region uint))
    (begin
        (asserts! (var-get protocol-active) err-not-authorized)
        (asserts! (> amount u0) err-invalid-amount)
        
        (let (
            (current-balance (get-balance tx-sender region))
            (position (get-user-position tx-sender region))
            (current-debt (get debt position))
        )
            (asserts! (>= (get balance current-balance) amount) err-insufficient-balance)
            (asserts! (>= current-debt amount) err-invalid-amount)
            
            (map-set regional-balances
                { user: tx-sender, region: region }
                { balance: (- (get balance current-balance) amount) }
            )
            
            (map-set user-positions
                { user: tx-sender, region: region }
                { collateral: (get collateral position), debt: (- current-debt amount) }
            )
            
            (ok true)
        )
    )
)

;; Withdraw collateral
(define-public (withdraw-collateral (amount uint))
    (begin
        (asserts! (var-get protocol-active) err-not-authorized)
        (asserts! (> amount u0) err-invalid-amount)
        
        (let (
            (user-collateral (get-collateral tx-sender))
            (current-amount (get amount user-collateral))
            (region (get region user-collateral))
            (position (get-user-position tx-sender region))
            (debt (get debt position))
            (remaining-collateral (- current-amount amount))
            (new-ratio (if (is-eq debt u0) 
                          u999999 
                          (/ (* remaining-collateral u100) debt)))
        )
            (asserts! (>= current-amount amount) err-insufficient-collateral)
            (asserts! (or (is-eq debt u0) (>= new-ratio min-collateral-ratio)) err-insufficient-collateral)
            
            (map-set collateral-deposits
                { user: tx-sender }
                { amount: remaining-collateral, region: region }
            )
            
            (var-set total-value-locked (- (var-get total-value-locked) amount))
            (ok true)
        )
    )
)

;; Deposit to regional yield vault
(define-public (deposit-to-vault (amount uint) (region uint))
    (begin
        (asserts! (var-get protocol-active) err-not-authorized)
        (asserts! (> amount u0) err-invalid-amount)
        
        (let (
            (current-balance (get-balance tx-sender region))
            (vault (get-vault-balance region))
        )
            (asserts! (>= (get balance current-balance) amount) err-insufficient-balance)
            
            (map-set regional-balances
                { user: tx-sender, region: region }
                { balance: (- (get balance current-balance) amount) }
            )
            
            (map-set regional-vaults
                { region: region }
                { balance: (+ (get balance vault) amount), 
                  total-yield: (get total-yield vault) }
            )
            
            (ok true)
        )
    )
)

;; Claim stability mining rewards
(define-public (claim-stability-rewards)
    (begin
        (asserts! (var-get protocol-active) err-not-authorized)
        
        (let (
            (rewards (default-to 
                { accumulated: u0, last-claim: u0 }
                (map-get? stability-rewards { user: tx-sender })
            ))
            (reward-amount (get accumulated rewards))
        )
            (asserts! (> reward-amount u0) err-invalid-amount)
            
            (map-set stability-rewards
                { user: tx-sender }
                { accumulated: u0, last-claim: block-height }
            )
            
            (ok reward-amount)
        )
    )
)

;; Admin functions

;; Update regional price oracle
(define-public (update-regional-oracle (region uint) (price uint))
    (begin
        (asserts! (is-oracle-operator tx-sender) err-not-authorized)
        (asserts! (or (is-eq region REGION-US) 
                     (or (is-eq region REGION-EU) 
                         (is-eq region REGION-AS))) err-invalid-region)
        (asserts! (> price u0) err-invalid-amount)
        
        (map-set regional-oracles
            { region: region }
            { price: price, last-update: block-height }
        )
        (ok true)
    )
)

;; Add oracle operator
(define-public (add-oracle-operator (operator principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        
        (map-set oracle-operators
            { operator: operator }
            { authorized: true }
        )
        (ok true)
    )
)

;; Remove oracle operator
(define-public (remove-oracle-operator (operator principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        
        (map-set oracle-operators
            { operator: operator }
            { authorized: false }
        )
        (ok true)
    )
)

;; Toggle protocol status
(define-public (toggle-protocol)
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (var-set protocol-active (not (var-get protocol-active)))
        (ok (var-get protocol-active))
    )
)

;; Distribute stability rewards
(define-public (distribute-rewards (user principal) (amount uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (> amount u0) err-invalid-amount)
        
        (let (
            (current-rewards (default-to 
                { accumulated: u0, last-claim: u0 }
                (map-get? stability-rewards { user: user })
            ))
        )
            (map-set stability-rewards
                { user: user }
                { accumulated: (+ (get accumulated current-rewards) amount), 
                  last-claim: (get last-claim current-rewards) }
            )
            (ok true)
        )
    )
)

;; Initialize regional vaults
(define-public (initialize-vaults)
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        
        (map-set regional-vaults { region: REGION-US } { balance: u0, total-yield: u0 })
        (map-set regional-vaults { region: REGION-EU } { balance: u0, total-yield: u0 })
        (map-set regional-vaults { region: REGION-AS } { balance: u0, total-yield: u0 })
        
        (map-set regional-oracles { region: REGION-US } { price: u1000000, last-update: block-height })
        (map-set regional-oracles { region: REGION-EU } { price: u1000000, last-update: block-height })
        (map-set regional-oracles { region: REGION-AS } { price: u1000000, last-update: block-height })
        
        (ok true)
    )
)
