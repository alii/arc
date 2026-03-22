> [!NOTE]  
> Arc is an extremely new and __highly experimental__ research project Tread with caution!

# arc

(Highly experimental) JavaScript on the BEAM

![js](./.github/js.png)

## Development

```sh
gleam test  # Run the tests
```

### Running test262

```sh
# Run the full test262 execution suite
TEST262_EXEC=1 gleam test

# Also write results to a JSON file
TEST262_EXEC=1 RESULTS_FILE=results.json gleam test

# Run parser-only test262 (faster, parse conformance only)
TEST262=1 gleam test
```

![test262 conformance chart](.github/test262/conformance.png)

#### A note on unhandled promise rejections:

Arc implements `HostPromiseRejectionTracker` from the ES spec. Unhandled rejections are reported to stderr after each microtask flush. Currently they are logged as warnings but are not fatal (unlike Node.js v15+ / QuickJS which exit on the first one). This will almost certainly change in the future.
