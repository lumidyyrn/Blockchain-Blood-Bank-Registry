# 🩸 Blockchain Blood Bank Registry

A decentralized blood donation and transfusion tracking system built on the Stacks blockchain using Clarity smart contracts.

## 🎯 Overview

The Blood Bank Registry enables transparent and immutable tracking of blood donations from donors to recipients, ensuring traceability, compatibility verification, and inventory management in the blood supply chain.

## ✨ Features

- 👥 **Donor Registration**: Register blood donors with personal details and blood type
- 🏥 **Recipient Registration**: Register blood recipients with medical information
- 💉 **Donation Tracking**: Record and track blood donations with expiry dates
- 🔄 **Transfusion Records**: Log blood transfusions with compatibility checks
- 📊 **Inventory Management**: Real-time blood inventory tracking by type
- ✅ **Verification System**: Admin verification for donors, recipients, donations, and transfusions
- 🧬 **Blood Compatibility**: Automatic blood type compatibility validation

## 🚀 Getting Started

### Prerequisites

- Clarinet CLI installed
- Stacks wallet for testing

### Installation

```bash
clarinet new blood-bank-registry
cd blood-bank-registry
```

Copy the contract code to `contracts/blood-bank-registry.clar`

### Testing

```bash
clarinet console
```

## 📖 Usage

### Register as a Donor

```clarity
(contract-call? .blood-bank-registry register-donor "John Doe" "O+" u25)
```

### Register as a Recipient

```clarity
(contract-call? .blood-bank-registry register-recipient "Jane Smith" "A+" u30 "Surgery patient")
```

### Record a Blood Donation

```clarity
(contract-call? .blood-bank-registry record-donation "O+" u450 "City Hospital")
```

### Record a Transfusion

```clarity
(contract-call? .blood-bank-registry record-transfusion u1 'ST1RECIPIENT u200 "General Hospital")
```

### Check Blood Inventory

```clarity
(contract-call? .blood-bank-registry get-blood-inventory "O+")
```

### Verify Records (Admin Only)

```clarity
(contract-call? .blood-bank-registry verify-donor 'ST1DONOR)
(contract-call? .blood-bank-registry verify-donation u1)
```

## 🔍 Read-Only Functions

- `get-donor`: Retrieve donor information
- `get-recipient`: Retrieve recipient information  
- `get-donation`: Get donation details
- `get-transfusion`: Get transfusion records
- `get-blood-inventory`: Check available blood by type
- `get-donation-counter`: Total donations recorded
- `get-transfusion-counter`: Total transfusions recorded

## 🩸 Supported Blood Types

- A+, A-, B+, B-, AB+, AB-, O+, O-

## 🛡️ Security Features

- Blood type compatibility validation
- Donation expiry checking (30 days default)
- Quantity verification for transfusions
- Admin-only verification system
- Immutable donation and transfusion records

## 🏗️ Contract Architecture

The contract uses several maps to store:
- **Donors**: Personal info and donation history
- **Recipients**: Medical info and blood requirements
- **Donations**: Blood unit details and status
- **Transfusions**: Usage records and hospital info
- **Blood Inventory**: Real-time availability by type

## 📝 Error Codes

- `u100`: Unauthorized access
- `u101`: Donor not found
- `u102`: Recipient not found
- `u103`: Donation not found
- `u104`: Invalid blood type
- `u105`: Insufficient quantity
- `u106`: Blood already used
- `u107`: Donation expired

