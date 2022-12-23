# Manifold.markets Haskell SDK

This SDK provides a Haskell interface to the Manifold.markets prediction market API.

## Installation

To install the SDK, run the following command:

```

```

## Usage

To use the SDK, import the `Manifold` module:

```haskell
import Manifold
```

Then, you can use the SDK to interact with the Manifold.markets API. For example, to get a market's details, you can use the `getMarket` function:

```haskell
let marketId = "12345"
let market = Manifold.getMarketById marketId
```

For more information on the Manifold.markets API, please refer to the [API documentation](https://docs.manifold.markets/api).