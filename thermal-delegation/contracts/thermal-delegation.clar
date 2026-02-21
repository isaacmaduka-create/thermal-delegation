;; Thermal Delegation DAO

;; ===========================
;; Constants
;; ===========================

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-PROPOSAL-NOT-FOUND (err u101))
(define-constant ERR-ALREADY-VOTED (err u102))
(define-constant ERR-PROPOSAL-CLOSED (err u103))
(define-constant ERR-COOLDOWN-ACTIVE (err u104))
(define-constant ERR-INVALID-AMOUNT (err u105))
(define-constant ERR-PROPOSAL-ACTIVE (err u106))
(define-constant ERR-INVALID-DELEGATE (err u107))

;; Thermal constants (scaled by u1000 for fixed-point arithmetic)
(define-constant DECAY-RATE u50)              ;; 5% decay per epoch (50/1000)
(define-constant BOOST-PARTICIPATION u200)    ;; +20% boost for voting (200/1000)
(define-constant BOOST-PROPOSAL u300)         ;; +30% boost for submitting proposal (300/1000)
(define-constant CONDUCTIVITY-RATE u800)      ;; 80% of heat transferred via delegation (800/1000)
(define-constant COOLDOWN-BLOCKS u144)        ;; ~24 hours in blocks
(define-constant MIN-HEAT u100)               ;; Minimum heat to participate
(define-constant BASE-HEAT u1000)             ;; Starting heat for new users
(define-constant PROPOSAL-DURATION u1008)     ;; ~7 days in blocks
(define-constant QUORUM-THRESHOLD u3000)      ;; 30% of network heat required (3000/1000)

;; ===========================
;; Data Maps & Variables
;; ===========================

;; Network Heat - global governance activity tracker
(define-data-var network-heat uint u0)
(define-data-var total-participants uint u0)
(define-data-var proposal-nonce uint u0)

;; Individual Heat - per-user thermal state
(define-map user-heat
  { user: principal }
  {
    heat: uint,              ;; Current individual heat score
    last-decay-block: uint,  ;; Block height of last decay application
    last-stake-block: uint,  ;; Block height of last stake increase (for cooldown)
    stake: uint,             ;; STX staked by user
    delegate: (optional principal) ;; Current delegate
  }
)

;; Delegation conductivity chains
(define-map delegation-received
  { delegate: principal }
  { total-conducted-heat: uint, delegator-count: uint }
)

;; Proposals with Proposal Heat
(define-map proposals
  { proposal-id: uint }
  {
    proposer: principal,
    title: (string-ascii 80),
    description: (string-ascii 500),
    proposal-heat: uint,       ;; Heat score of proposal (drives priority)
    votes-for: uint,
    votes-against: uint,
    start-block: uint,
    end-block: uint,
    executed: bool,
    passed: bool
  }
)

;; Vote records to prevent double voting
(define-map vote-records
  { proposal-id: uint, voter: principal }
  { voted: bool, vote-heat: uint, in-favor: bool }
)

;; ===========================
;; Private Helpers
;; ===========================

;; Apply thermal decay to a heat value based on elapsed blocks
(define-private (apply-decay (heat uint) (blocks-elapsed uint))
  (let (
    ;; Each epoch (144 blocks) reduces heat by DECAY-RATE/1000
    (epochs (/ blocks-elapsed u144))
    (decay-factor (- u1000 (* epochs DECAY-RATE)))
    (effective-factor (if (> decay-factor u0) decay-factor u0))
  )
    (/ (* heat effective-factor) u1000)
  )
)

;; Get current heat for a user, accounting for decay
(define-private (get-effective-heat (user principal))
  (match (map-get? user-heat { user: user })
    state
      (let (
        (blocks-elapsed (- block-height (get last-decay-block state)))
        (decayed (apply-decay (get heat state) blocks-elapsed))
      )
        decayed
      )
    u0
  )
)

;; Get total voting power (own heat + conducted delegation heat)
(define-private (get-voting-power (user principal))
  (let (
    (own-heat (get-effective-heat user))
    (delegated-heat
      (match (map-get? delegation-received { delegate: user })
        d (/ (* (get total-conducted-heat d) CONDUCTIVITY-RATE) u1000)
        u0
      )
    )
  )
    (+ own-heat delegated-heat)
  )
)

;; Update network heat
(define-private (update-network-heat (delta int))
  (let (
    (current (var-get network-heat))
    (updated
      (if (> delta 0)
        (+ current (to-uint delta))
        (if (> current (to-uint (* -1 delta)))
          (- current (to-uint (* -1 delta)))
          u0
        )
      )
    )
  )
    (var-set network-heat updated)
  )
)

;; Ensure user record exists, initializing with BASE-HEAT if new
(define-private (ensure-user-initialized (user principal))
  (if (is-none (map-get? user-heat { user: user }))
    (begin
      (map-set user-heat { user: user }
        {
          heat: BASE-HEAT,
          last-decay-block: block-height,
          last-stake-block: u0,
          stake: u0,
          delegate: none
        }
      )
      (var-set total-participants (+ (var-get total-participants) u1))
      (update-network-heat 1000)
      true
    )
    false
  )
)

;; ===========================
;; Public Functions
;; ===========================

;; Stake STX to increase individual heat (subject to cooldown/thermal resistance)
(define-public (stake-heat (amount uint))
  (begin
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (ensure-user-initialized tx-sender)
    (let (
      (state (unwrap-panic (map-get? user-heat { user: tx-sender })))
      (last-stake (get last-stake-block state))
    )
      ;; Thermal resistance: cooldown required between large stake increases
      (asserts!
        (or (is-eq last-stake u0)
            (>= (- block-height last-stake) COOLDOWN-BLOCKS))
        ERR-COOLDOWN-ACTIVE
      )
      (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
      (let (
        (current-heat (get-effective-heat tx-sender))
        ;; Heat gain from staking scales with square root approximation (simplified)
        (heat-gain (/ (* amount u100) u1000000))
        (new-heat (+ current-heat (max heat-gain u10)))
      )
        (map-set user-heat { user: tx-sender }
          (merge state
            {
              heat: new-heat,
              stake: (+ (get stake state) amount),
              last-decay-block: block-height,
              last-stake-block: block-height
            }
          )
        )
        (update-network-heat (to-int heat-gain))
        (ok new-heat)
      )
    )
  )
)

;; Unstake STX (reduces individual heat proportionally)
(define-public (unstake-heat (amount uint))
  (let (
    (state (unwrap! (map-get? user-heat { user: tx-sender }) ERR-NOT-AUTHORIZED))
  )
    (asserts! (>= (get stake state) amount) ERR-INVALID-AMOUNT)
    (let (
      (current-heat (get-effective-heat tx-sender))
      (stake-ratio (/ (* amount u1000) (max (get stake state) u1)))
      (heat-lost (/ (* current-heat stake-ratio) u1000))
      (new-heat (if (> current-heat heat-lost) (- current-heat heat-lost) u0))
    )
      (try! (as-contract (stx-transfer? amount tx-sender tx-sender)))
      (map-set user-heat { user: tx-sender }
        (merge state
          {
            heat: new-heat,
            stake: (- (get stake state) amount),
            last-decay-block: block-height
          }
        )
      )
      (update-network-heat (* -1 (to-int heat-lost)))
      (ok new-heat)
    )
  )
)

;; Delegate thermal energy to another principal (conductivity chain)
(define-public (delegate-heat (delegate principal))
  (begin
    (asserts! (not (is-eq delegate tx-sender)) ERR-INVALID-DELEGATE)
    (ensure-user-initialized tx-sender)
    (let (
      (state (unwrap-panic (map-get? user-heat { user: tx-sender })))
      (my-heat (get-effective-heat tx-sender))
    )
      ;; Remove heat from previous delegate if any
      (match (get delegate state)
        prev-delegate
          (match (map-get? delegation-received { delegate: prev-delegate })
            prev-d
              (map-set delegation-received { delegate: prev-delegate }
                {
                  total-conducted-heat:
                    (if (> (get total-conducted-heat prev-d) my-heat)
                      (- (get total-conducted-heat prev-d) my-heat)
                      u0
                    ),
                  delegator-count:
                    (if (> (get delegator-count prev-d) u0)
                      (- (get delegator-count prev-d) u1)
                      u0
                    )
                }
              )
            true
          )
        true
      )
      ;; Add heat to new delegate
      (match (map-get? delegation-received { delegate: delegate })
        existing
          (map-set delegation-received { delegate: delegate }
            {
              total-conducted-heat: (+ (get total-conducted-heat existing) my-heat),
              delegator-count: (+ (get delegator-count existing) u1)
            }
          )
        (map-set delegation-received { delegate: delegate }
          { total-conducted-heat: my-heat, delegator-count: u1 }
        )
      )
      ;; Update user state with new delegate
      (map-set user-heat { user: tx-sender }
        (merge state { delegate: (some delegate) })
      )
      (ok true)
    )
  )
)

;; Remove delegation
(define-public (undelegate-heat)
  (let (
    (state (unwrap! (map-get? user-heat { user: tx-sender }) ERR-NOT-AUTHORIZED))
    (my-heat (get-effective-heat tx-sender))
  )
    (match (get delegate state)
      prev-delegate
        (match (map-get? delegation-received { delegate: prev-delegate })
          prev-d
            (map-set delegation-received { delegate: prev-delegate }
              {
                total-conducted-heat:
                  (if (> (get total-conducted-heat prev-d) my-heat)
                    (- (get total-conducted-heat prev-d) my-heat)
                    u0
                  ),
                delegator-count:
                  (if (> (get delegator-count prev-d) u0)
                    (- (get delegator-count prev-d) u1)
                    u0
                  )
              }
            )
          true
        )
      true
    )
    (map-set user-heat { user: tx-sender }
      (merge state { delegate: none })
    )
    (ok true)
  )
)

;; Submit a governance proposal (thermal boost for proposer)
(define-public (submit-proposal (title (string-ascii 80)) (description (string-ascii 500)))
  (begin
    (ensure-user-initialized tx-sender)
    (let (
      (current-heat (get-effective-heat tx-sender))
      (state (unwrap-panic (map-get? user-heat { user: tx-sender })))
    )
      (asserts! (>= current-heat MIN-HEAT) ERR-NOT-AUTHORIZED)
      (let (
        (proposal-id (+ (var-get proposal-nonce) u1))
        ;; Proposal heat seeded from proposer's individual heat
        (initial-proposal-heat current-heat)
        ;; Boost proposer's individual heat
        (boost-amount (/ (* current-heat BOOST-PROPOSAL) u1000))
        (new-heat (+ current-heat boost-amount))
      )
        (var-set proposal-nonce proposal-id)
        (map-set proposals { proposal-id: proposal-id }
          {
            proposer: tx-sender,
            title: title,
            description: description,
            proposal-heat: initial-proposal-heat,
            votes-for: u0,
            votes-against: u0,
            start-block: block-height,
            end-block: (+ block-height PROPOSAL-DURATION),
            executed: false,
            passed: false
          }
        )
        ;; Apply thermal boost to proposer
        (map-set user-heat { user: tx-sender }
          (merge state { heat: new-heat, last-decay-block: block-height })
        )
        (update-network-heat (to-int boost-amount))
        (ok proposal-id)
      )
    )
  )
)

;; Vote on a proposal (thermal boost for voter, increases proposal heat)
(define-public (vote (proposal-id uint) (in-favor bool))
  (begin
    (ensure-user-initialized tx-sender)
    (let (
      (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR-PROPOSAL-NOT-FOUND))
      (vote-key { proposal-id: proposal-id, voter: tx-sender })
    )
      (asserts! (is-none (map-get? vote-records vote-key)) ERR-ALREADY-VOTED)
      (asserts! (<= block-height (get end-block proposal)) ERR-PROPOSAL-CLOSED)
      (let (
        (voting-power (get-voting-power tx-sender))
        (state (unwrap-panic (map-get? user-heat { user: tx-sender })))
        (current-heat (get-effective-heat tx-sender))
        ;; Thermal boost for participating
        (boost-amount (/ (* current-heat BOOST-PARTICIPATION) u1000))
        (new-heat (+ current-heat boost-amount))
        ;; Update proposal heat based on voter engagement
        (new-proposal-heat (+ (get proposal-heat proposal) (/ voting-power u10)))
      )
        ;; Record vote
        (map-set vote-records vote-key
          { voted: true, vote-heat: voting-power, in-favor: in-favor }
        )
        ;; Update vote tallies on proposal
        (map-set proposals { proposal-id: proposal-id }
          (merge proposal
            {
              votes-for: (if in-favor (+ (get votes-for proposal) voting-power) (get votes-for proposal)),
              votes-against: (if in-favor (get votes-against proposal) (+ (get votes-against proposal) voting-power)),
              proposal-heat: new-proposal-heat
            }
          )
        )
        ;; Apply thermal boost to voter's individual heat
        (map-set user-heat { user: tx-sender }
          (merge state { heat: new-heat, last-decay-block: block-height })
        )
        (update-network-heat (to-int boost-amount))
        (ok voting-power)
      )
    )
  )
)

;; Execute a proposal after voting period ends
(define-public (execute-proposal (proposal-id uint))
  (let (
    (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR-PROPOSAL-NOT-FOUND))
    (net-heat (var-get network-heat))
  )
    (asserts! (> block-height (get end-block proposal)) ERR-PROPOSAL-ACTIVE)
    (asserts! (not (get executed proposal)) ERR-PROPOSAL-CLOSED)
    ;; Quorum check: total votes must represent minimum portion of network heat
    (let (
      (total-votes (+ (get votes-for proposal) (get votes-against proposal)))
      (quorum-required (/ (* net-heat QUORUM-THRESHOLD) u1000))
      (passed (and
        (>= total-votes (max quorum-required u1))
        (> (get votes-for proposal) (get votes-against proposal))
      ))
    )
      (map-set proposals { proposal-id: proposal-id }
        (merge proposal { executed: true, passed: passed })
      )
      (ok passed)
    )
  )
)

;; Manually apply decay to refresh stored heat value
(define-public (refresh-heat)
  (let (
    (state (unwrap! (map-get? user-heat { user: tx-sender }) ERR-NOT-AUTHORIZED))
    (decayed-heat (get-effective-heat tx-sender))
  )
    (map-set user-heat { user: tx-sender }
      (merge state { heat: decayed-heat, last-decay-block: block-height })
    )
    (ok decayed-heat)
  )
)

;; ===========================
;; Read-Only Functions
;; ===========================

;; Get individual heat for any user (with live decay applied)
(define-read-only (get-individual-heat (user principal))
  (ok (get-effective-heat user))
)

;; Get voting power (own + conducted delegation)
(define-read-only (get-user-voting-power (user principal))
  (ok (get-voting-power user))
)

;; Get full user thermal state
(define-read-only (get-user-state (user principal))
  (ok (map-get? user-heat { user: user }))
)

;; Get delegation info received by a delegate
(define-read-only (get-delegation-info (delegate principal))
  (ok (map-get? delegation-received { delegate: delegate }))
)

;; Get proposal details including current Proposal Heat
(define-read-only (get-proposal (proposal-id uint))
  (ok (map-get? proposals { proposal-id: proposal-id }))
)

;; Get vote record for a user on a proposal
(define-read-only (get-vote-record (proposal-id uint) (voter principal))
  (ok (map-get? vote-records { proposal-id: proposal-id, voter: voter }))
)

;; Get Network Heat (global governance activity metric)
(define-read-only (get-network-heat)
  (ok (var-get network-heat))
)

;; Get governance summary stats
(define-read-only (get-governance-stats)
  (ok {
    network-heat: (var-get network-heat),
    total-participants: (var-get total-participants),
    total-proposals: (var-get proposal-nonce)
  })
)

;; Check if a user is in cooldown for staking
(define-read-only (is-in-cooldown (user principal))
  (match (map-get? user-heat { user: user })
    state
      (ok (< (- block-height (get last-stake-block state)) COOLDOWN-BLOCKS))
    (ok false)
  )
)

;; ===========================
;; Utility
;; ===========================

(define-private (max (a uint) (b uint))
  (if (>= a b) a b)
)
