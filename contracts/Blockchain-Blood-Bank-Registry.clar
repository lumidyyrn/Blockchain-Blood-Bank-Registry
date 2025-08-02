(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_DONOR_NOT_FOUND (err u101))
(define-constant ERR_RECIPIENT_NOT_FOUND (err u102))
(define-constant ERR_DONATION_NOT_FOUND (err u103))
(define-constant ERR_INVALID_BLOOD_TYPE (err u104))
(define-constant ERR_INSUFFICIENT_QUANTITY (err u105))
(define-constant ERR_ALREADY_USED (err u106))
(define-constant ERR_EXPIRED (err u107))
(define-constant ERR_BADGE_ALREADY_CLAIMED (err u108))
(define-constant ERR_INSUFFICIENT_POINTS (err u109))

(define-constant ERR_REQUEST_NOT_FOUND (err u110))
(define-constant ERR_REQUEST_EXPIRED (err u111))
(define-constant ERR_REQUEST_FULFILLED (err u112))

(define-data-var emergency-request-counter uint u0)

(define-data-var donation-counter uint u0)
(define-data-var transfusion-counter uint u0)

(define-map donors
  { donor-id: principal }
  {
    name: (string-ascii 50),
    blood-type: (string-ascii 3),
    age: uint,
    last-donation: uint,
    total-donations: uint,
    verified: bool
  }
)

(define-map recipients
  { recipient-id: principal }
  {
    name: (string-ascii 50),
    blood-type: (string-ascii 3),
    age: uint,
    medical-condition: (string-ascii 100),
    verified: bool
  }
)

(define-map donations
  { donation-id: uint }
  {
    donor: principal,
    blood-type: (string-ascii 3),
    quantity: uint,
    donation-date: uint,
    expiry-date: uint,
    location: (string-ascii 50),
    status: (string-ascii 20),
    verified: bool
  }
)

(define-map transfusions
  { transfusion-id: uint }
  {
    donation-id: uint,
    recipient: principal,
    quantity-used: uint,
    transfusion-date: uint,
    hospital: (string-ascii 50),
    verified: bool
  }
)

(define-map blood-inventory
  { blood-type: (string-ascii 3) }
  { available-quantity: uint }
)

(define-read-only (get-donor (donor-id principal))
  (map-get? donors { donor-id: donor-id })
)

(define-read-only (get-recipient (recipient-id principal))
  (map-get? recipients { recipient-id: recipient-id })
)

(define-read-only (get-donation (donation-id uint))
  (map-get? donations { donation-id: donation-id })
)

(define-read-only (get-transfusion (transfusion-id uint))
  (map-get? transfusions { transfusion-id: transfusion-id })
)

(define-read-only (get-blood-inventory (blood-type (string-ascii 3)))
  (default-to { available-quantity: u0 } (map-get? blood-inventory { blood-type: blood-type }))
)

(define-read-only (get-donation-counter)
  (var-get donation-counter)
)

(define-read-only (get-transfusion-counter)
  (var-get transfusion-counter)
)

(define-public (register-donor (name (string-ascii 50)) (blood-type (string-ascii 3)) (age uint))
  (begin
    (asserts! (is-valid-blood-type blood-type) ERR_INVALID_BLOOD_TYPE)
    (ok (map-set donors
      { donor-id: tx-sender }
      {
        name: name,
        blood-type: blood-type,
        age: age,
        last-donation: u0,
        total-donations: u0,
        verified: false
      }
    ))
  )
)

(define-public (register-recipient (name (string-ascii 50)) (blood-type (string-ascii 3)) (age uint) (medical-condition (string-ascii 100)))
  (begin
    (asserts! (is-valid-blood-type blood-type) ERR_INVALID_BLOOD_TYPE)
    (ok (map-set recipients
      { recipient-id: tx-sender }
      {
        name: name,
        blood-type: blood-type,
        age: age,
        medical-condition: medical-condition,
        verified: false
      }
    ))
  )
)

(define-public (record-donation (blood-type (string-ascii 3)) (quantity uint) (location (string-ascii 50)))
  (let
    (
      (donation-id (+ (var-get donation-counter) u1))
      (current-block stacks-block-height)
      (expiry-block (+ current-block u4320))
      (donor-data (unwrap! (get-donor tx-sender) ERR_DONOR_NOT_FOUND))
    )
    (asserts! (is-valid-blood-type blood-type) ERR_INVALID_BLOOD_TYPE)
    (asserts! (is-eq (get blood-type donor-data) blood-type) ERR_INVALID_BLOOD_TYPE)
    (var-set donation-counter donation-id)
    (map-set donations
      { donation-id: donation-id }
      {
        donor: tx-sender,
        blood-type: blood-type,
        quantity: quantity,
        donation-date: current-block,
        expiry-date: expiry-block,
        location: location,
        status: "available",
        verified: false
      }
    )
    (map-set donors
      { donor-id: tx-sender }
      (merge donor-data {
        last-donation: current-block,
        total-donations: (+ (get total-donations donor-data) u1)
      })
    )
    (update-inventory blood-type quantity true)
    (ok donation-id)
  )
)

(define-public (record-transfusion (donation-id uint) (recipient principal) (quantity-used uint) (hospital (string-ascii 50)))
  (let
    (
      (transfusion-id (+ (var-get transfusion-counter) u1))
      (donation-data (unwrap! (get-donation donation-id) ERR_DONATION_NOT_FOUND))
      (recipient-data (unwrap! (get-recipient recipient) ERR_RECIPIENT_NOT_FOUND))
    )
    (asserts! (is-eq (get status donation-data) "available") ERR_ALREADY_USED)
    (asserts! (>= (get quantity donation-data) quantity-used) ERR_INSUFFICIENT_QUANTITY)
    (asserts! (> (get expiry-date donation-data) stacks-block-height) ERR_EXPIRED)
    (asserts! (is-compatible-blood-type (get blood-type donation-data) (get blood-type recipient-data)) ERR_INVALID_BLOOD_TYPE)
    (var-set transfusion-counter transfusion-id)
    (map-set transfusions
      { transfusion-id: transfusion-id }
      {
        donation-id: donation-id,
        recipient: recipient,
        quantity-used: quantity-used,
        transfusion-date: stacks-block-height,
        hospital: hospital,
        verified: false
      }
    )
    (map-set donations
      { donation-id: donation-id }
      (merge donation-data { status: "used" })
    )
    (update-inventory (get blood-type donation-data) quantity-used false)
    (ok transfusion-id)
  )
)

(define-public (verify-donor (donor-id principal))
  (let
    (
      (donor-data (unwrap! (get-donor donor-id) ERR_DONOR_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (ok (map-set donors
      { donor-id: donor-id }
      (merge donor-data { verified: true })
    ))
  )
)

(define-public (verify-recipient (recipient-id principal))
  (let
    (
      (recipient-data (unwrap! (get-recipient recipient-id) ERR_RECIPIENT_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (ok (map-set recipients
      { recipient-id: recipient-id }
      (merge recipient-data { verified: true })
    ))
  )
)

(define-public (verify-donation (donation-id uint))
  (let
    (
      (donation-data (unwrap! (get-donation donation-id) ERR_DONATION_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (ok (map-set donations
      { donation-id: donation-id }
      (merge donation-data { verified: true })
    ))
  )
)

(define-public (verify-transfusion (transfusion-id uint))
  (let
    (
      (transfusion-data (unwrap! (get-transfusion transfusion-id) (err u404)))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (ok (map-set transfusions
      { transfusion-id: transfusion-id }
      (merge transfusion-data { verified: true })
    ))
  )
)

(define-private (is-valid-blood-type (blood-type (string-ascii 3)))
  (or
    (is-eq blood-type "A+")
    (is-eq blood-type "A-")
    (is-eq blood-type "B+")
    (is-eq blood-type "B-")
    (is-eq blood-type "AB+")
    (is-eq blood-type "AB-")
    (is-eq blood-type "O+")
    (is-eq blood-type "O-")
  )
)

(define-private (is-compatible-blood-type (donor-type (string-ascii 3)) (recipient-type (string-ascii 3)))
  (or
    (is-eq donor-type "O-")
    (is-eq donor-type recipient-type)
    (and (is-eq donor-type "O+") (or (is-eq recipient-type "A+") (is-eq recipient-type "B+") (is-eq recipient-type "AB+") (is-eq recipient-type "O+")))
    (and (is-eq donor-type "A-") (or (is-eq recipient-type "A+") (is-eq recipient-type "A-") (is-eq recipient-type "AB+") (is-eq recipient-type "AB-")))
    (and (is-eq donor-type "B-") (or (is-eq recipient-type "B+") (is-eq recipient-type "B-") (is-eq recipient-type "AB+") (is-eq recipient-type "AB-")))
    (and (is-eq donor-type "A+") (or (is-eq recipient-type "A+") (is-eq recipient-type "AB+")))
    (and (is-eq donor-type "B+") (or (is-eq recipient-type "B+") (is-eq recipient-type "AB+")))
    (and (is-eq donor-type "AB-") (or (is-eq recipient-type "AB+") (is-eq recipient-type "AB-")))
  )
)

(define-private (update-inventory (blood-type (string-ascii 3)) (quantity uint) (is-addition bool))
  (let
    (
      (current-inventory (get-blood-inventory blood-type))
      (current-quantity (get available-quantity current-inventory))
    )
    (map-set blood-inventory
      { blood-type: blood-type }
      {
        available-quantity: (if is-addition
          (+ current-quantity quantity)
          (if (>= current-quantity quantity)
            (- current-quantity quantity)
            u0
          )
        )
      }
    )
  )
)


(define-map donor-rewards
  { donor-id: principal }
  {
    total-points: uint,
    lifesaver-badge: bool,
    hero-badge: bool,
    legend-badge: bool,
    last-streak: uint,
    current-streak: uint
  }
)

(define-map badge-requirements
  { badge-name: (string-ascii 20) }
  { points-required: uint }
)

(define-private (initialize-badges)
  (begin
    (map-set badge-requirements { badge-name: "lifesaver" } { points-required: u100 })
    (map-set badge-requirements { badge-name: "hero" } { points-required: u500 })
    (map-set badge-requirements { badge-name: "legend" } { points-required: u1000 })
  )
)

(define-private (get-blood-type-points (blood-type (string-ascii 3)))
  (if (is-eq blood-type "O-") u50
    (if (or (is-eq blood-type "AB-") (is-eq blood-type "A-") (is-eq blood-type "B-")) u30
      u20
    )
  )
)

(define-public (award-donation-points (donor principal) (blood-type (string-ascii 3)))
  (let
    (
      (current-rewards (default-to { total-points: u0, lifesaver-badge: false, hero-badge: false, legend-badge: false, last-streak: u0, current-streak: u0 } 
                       (map-get? donor-rewards { donor-id: donor })))
      (points-to-add (get-blood-type-points blood-type))
      (new-total-points (+ (get total-points current-rewards) points-to-add))
      (new-streak (+ (get current-streak current-rewards) u1))
    )
    (map-set donor-rewards
      { donor-id: donor }
      (merge current-rewards {
        total-points: new-total-points,
        current-streak: new-streak
      })
    )
    (ok new-total-points)
  )
)

(define-public (claim-badge (badge-name (string-ascii 20)))
  (let
    (
      (donor-data (default-to { total-points: u0, lifesaver-badge: false, hero-badge: false, legend-badge: false, last-streak: u0, current-streak: u0 } 
                  (map-get? donor-rewards { donor-id: tx-sender })))
      (badge-req (unwrap! (map-get? badge-requirements { badge-name: badge-name }) (err u404)))
      (points-required (get points-required badge-req))
    )
    (asserts! (>= (get total-points donor-data) points-required) ERR_INSUFFICIENT_POINTS)
    (if (is-eq badge-name "lifesaver")
      (begin
        (asserts! (not (get lifesaver-badge donor-data)) ERR_BADGE_ALREADY_CLAIMED)
        (map-set donor-rewards { donor-id: tx-sender } (merge donor-data { lifesaver-badge: true }))
        (ok true)
      )
      (if (is-eq badge-name "hero")
        (begin
          (asserts! (not (get hero-badge donor-data)) ERR_BADGE_ALREADY_CLAIMED)
          (map-set donor-rewards { donor-id: tx-sender } (merge donor-data { hero-badge: true }))
          (ok true)
        )
        (begin
          (asserts! (not (get legend-badge donor-data)) ERR_BADGE_ALREADY_CLAIMED)
          (map-set donor-rewards { donor-id: tx-sender } (merge donor-data { legend-badge: true }))
          (ok true)
        )
      )
    )
  )
)

(define-read-only (get-donor-rewards (donor-id principal))
  (map-get? donor-rewards { donor-id: donor-id })
)

(define-map emergency-requests
  { request-id: uint }
  {
    hospital: principal,
    blood-type: (string-ascii 3),
    quantity-needed: uint,
    urgency-level: uint,
    request-time: uint,
    expiry-time: uint,
    quantity-fulfilled: uint,
    status: (string-ascii 20),
    location: (string-ascii 50)
  }
)

(define-map emergency-responses
  { request-id: uint, responder: principal }
  {
    donation-id: uint,
    response-time: uint,
    quantity-contributed: uint
  }
)

(define-public (create-emergency-request 
    (blood-type (string-ascii 3)) 
    (quantity-needed uint) 
    (urgency-level uint) 
    (duration-blocks uint)
    (location (string-ascii 50)))
  (let
    (
      (request-id (+ (var-get emergency-request-counter) u1))
      (current-time stacks-block-height)
      (expiry-time (+ current-time duration-blocks))
    )
    (asserts! (is-valid-blood-type blood-type) ERR_INVALID_BLOOD_TYPE)
    (asserts! (<= urgency-level u5) (err u400))
    (var-set emergency-request-counter request-id)
    (map-set emergency-requests
      { request-id: request-id }
      {
        hospital: tx-sender,
        blood-type: blood-type,
        quantity-needed: quantity-needed,
        urgency-level: urgency-level,
        request-time: current-time,
        expiry-time: expiry-time,
        quantity-fulfilled: u0,
        status: "active",
        location: location
      }
    )
    (ok request-id)
  )
)

(define-public (respond-to-emergency (request-id uint) (donation-id uint))
  (let
    (
      (request-data (unwrap! (map-get? emergency-requests { request-id: request-id }) ERR_REQUEST_NOT_FOUND))
      (donation-data (unwrap! (get-donation donation-id) ERR_DONATION_NOT_FOUND))
      (quantity-available (get quantity donation-data))
      (quantity-needed (- (get quantity-needed request-data) (get quantity-fulfilled request-data)))
      (quantity-to-fulfill (if (<= quantity-needed quantity-available) quantity-needed quantity-available))
    )
    (asserts! (> (get expiry-time request-data) stacks-block-height) ERR_REQUEST_EXPIRED)
    (asserts! (is-eq (get status request-data) "active") ERR_REQUEST_FULFILLED)
    (asserts! (is-eq (get blood-type request-data) (get blood-type donation-data)) ERR_INVALID_BLOOD_TYPE)
    (asserts! (is-eq (get status donation-data) "available") ERR_ALREADY_USED)
    (map-set emergency-responses
      { request-id: request-id, responder: tx-sender }
      {
        donation-id: donation-id,
        response-time: stacks-block-height,
        quantity-contributed: quantity-to-fulfill
      }
    )
    (map-set emergency-requests
      { request-id: request-id }
      (merge request-data {
        quantity-fulfilled: (+ (get quantity-fulfilled request-data) quantity-to-fulfill),
        status: (if (>= (+ (get quantity-fulfilled request-data) quantity-to-fulfill) (get quantity-needed request-data)) "fulfilled" "active")
      })
    )
    (ok quantity-to-fulfill)
  )
)

(define-read-only (get-emergency-request (request-id uint))
  (map-get? emergency-requests { request-id: request-id })
)

(define-read-only (get-emergency-response (request-id uint) (responder principal))
  (map-get? emergency-responses { request-id: request-id, responder: responder })
)

(define-read-only (get-emergency-request-counter)
  (var-get emergency-request-counter)
)