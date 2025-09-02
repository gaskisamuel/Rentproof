# 🏠 Rentproof - Landlord-Tenant Reputation NFT System

A blockchain-based reputation system that creates NFT records of rental lease agreements and tracks the rental history and ratings of both landlords and tenants on the Stacks blockchain.

## 🌟 Features

- **📋 Lease Record Creation**: Create immutable lease agreements as NFTs
- **⭐ Rating System**: Rate landlords and tenants after lease completion (1-5 stars)
- **🏆 Reputation Scoring**: Automatic calculation of reputation scores based on ratings and completed leases
- **✅ User Verification**: Contract owner can verify legitimate users
- **📊 Profile Management**: Track total leases, completed leases, and average ratings
- **📚 Lease History**: Maintain complete rental history for all users

## 🚀 Getting Started

### Prerequisites

- [Clarinet](https://github.com/hirosystems/clarinet) installed
- Stacks wallet for testing

### Installation

1. Clone this repository
2. Navigate to the project directory
3. Run Clarinet commands to test and deploy

```bash
clarinet check
```

```bash
clarinet test
```

```bash
clarinet deploy
```

## 📖 Usage

### Creating a Lease Record

```clarity
(contract-call? .rentproof create-lease-record 
    'ST1TENANT-ADDRESS
    "123 Main St, City, State"
    u365  ;; lease duration in blocks
    u1000 ;; monthly rent
    u2000) ;; security deposit
```

### Completing a Lease

```clarity
(contract-call? .rentproof complete-lease 
    u1 ;; lease-id
    "completed-successfully")
```

### Rating a Lease

```clarity
(contract-call? .rentproof rate-lease 
    u1 ;; lease-id
    u5 ;; rating (1-5)
    "Great tenant, paid on time!")
```

### Checking User Profile

```clarity
(contract-call? .rentproof get-user-profile 'ST1USER-ADDRESS)
```

## 🔧 Contract Functions

### Public Functions

- `create-lease-record` - Create a new lease agreement NFT
- `complete-lease` - Mark a lease as completed
- `rate-lease` - Rate the other party after lease completion
- `verify-user` - Verify a user (owner only)
- `set-contract-uri` - Update metadata URI (owner only)

### Read-Only Functions

- `get-lease-record` - Get lease details by ID
- `get-lease-rating` - Get ratings for a specific lease
- `get-user-profile` - Get user's rental profile
- `get-user-lease-history` - Get list of user's lease IDs
- `get-reputation-score` - Calculate user's reputation score
- `get-contract-uri` - Get metadata base URI

## 🏅 Reputation Scoring

The reputation score is calculated as:
- Average rating × 20 points
- Completed leases × 5 points each
- Verification bonus: +50 points

## 🛡️ Security Features

- Only lease participants can rate each other
- Ratings can only be submitted once per party per lease
- Only completed leases can be rated
- Contract owner controls user verification

## 📝 Error Codes

- `u100` - Unauthorized access
- `u101` - Record not found
- `u102` - Record already exists
- `u103` - Invalid rating (must be 1-5)
- `u104` - Invalid duration or amount
- `u105` - Lease still active

## 🤝 Contributing

Feel free to submit issues and enhancement requests!


