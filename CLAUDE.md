# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project

Scala 3 infrastructure for controlling WiZ Connected smart lights over their local UDP JSON-RPC protocol. Built with
sbt, ZIO 2 (effects, layers, streams), circe (JSON), and Netty (UDP sockets).

## Commands

```bash
sbt compile                     # compile all modules
sbt test                        # run all tests (zio-test)
sbt backend/test                # test one module (apiJVM, apiJS, backend, server)
sbt "backend/testOnly *RequestSpec"                       # run a single spec
sbt "apiJVM/testOnly *BrightnessSpec -- -t \"from\""      # run tests whose name matches
sbt coverage test coverageReport                          # scoverage report
```

No formatter or linter is configured.

## Modules

- **api** (`net.wayfarerx.wizlights`): cross-built for JVM and Scala.js (`CrossType.Pure`), so it must stay
platform-neutral (no `java.net`, Netty, etc.). Contains:
  - `model`: domain values (`Light`, `MacAddress`, `Brightness`, `Color`, `Scene`, `Update`, …).
  - `service.Lighting`: the public service trait. It has many `update`/`updateAll` overloads, disambiguated with `@targetName`, and they all reduce to `updateAll(macs, Update)`.
- **backend** (`net.wayfarerx.wizlights.backend`): JVM implementation of `Lighting`. Depends on `apiJVM`.
- **server** (`net.wayfarerx.wizlights.server`): planned zio-http front end. It currently has no sources.

## Code conventions

- **Validated values** are `opaque type X <: Int = Int` (or similar). Each companion has `from(...): Either[String, X]` (pure), `make(...): IO[IllegalArgumentException, X]` (effectful), and circe `given Encoder`/`Decoder` instances, where the decoder calls `from`. See `model/Brightness.scala`.
- **Configuration** uses a `private` case-class constructor with `make(...): Task[Configuration]`, which validates fields by chaining `validate`.
- **Services** follow the ZIO pattern of a `trait` plus a `final class XLive private(...)`. The companion object provides `make(...): URIO[Scope, X]` / `RIO[Scope, X]` for construction with explicit dependencies, and `val layer: ZLayer[...]` reads them from the environment. Background work is attached with `.forkScoped`, so it stops when the layer's scope closes.
- Most public service methods return `UIO`. Errors are logged (`ZIO.logWarning`/`logInfoCause`) and swallowed instead of being surfaced.
- Scala 3 syntax throughout: braceless indentation, `then`/`do`, `for … yield` with `=` bindings. All sources start with the Apache 2.0 license header and use chained package clauses (`package net.wayfarerx.wizlights` / `package backend` / `package main`).
- Public and private members have Scaladoc. Overridden methods in implementations use a short `/* ... */` comment instead.
- Tests are `object XSpec extends ZIOSpecDefault` and use `suite`/`test`/`assertTrue`. Each model type and protocol message has a matching spec.

## Backend architecture

Layer graph: `Configuration` → `NetworkLive.layer` (needs `Configuration`) → `LightingLive.layer` (needs `Configuration & Clock & Network`) → `Lighting`.

### `protocol`
Typed WiZ JSON-RPC messages with circe codecs. `Request` subtypes (`GetPilotRequest`, `SetPilotRequest`, `GetSystemConfigRequest`, `RegistrationRequest`) carry an optional `id`, which is used for correlation (`withId`). `Response` subtypes hold `Either[error, payload]`. Method names and JSON keys are constants in `protocol/package.scala`. `SetPilotRequest.from(update)` maps a model `Update` to the wire format.

### `network`
- `Network` trait: `publish(Outgoing)`, `subscribe` (a scoped stream of `Incoming`), `events` (a `UStream[Event]` of topology changes), and `localhost(target)` (the MAC and IP of the local interface on the target's subnet, which is used for registration).
- `NetworkLive` owns a Netty event loop and two UDP `Socket`s: the command port (38899, which bulbs listen on) and the listener port (38900, which bulbs push `syncPilot`/`firstBeat` to after registration). Both feed one sliding `Hub[Incoming]`. `publish` resolves the message's destinations from the current topology and writes straight from the command socket (no outgoing queue). Write failures are logged per address.
- `Socket.make` binds inside `ZStream.asyncScoped` so the Netty `Handler` can `emit` received packets without `Unsafe`. The bound channel is handed back to `make` through a `Promise`, and the stream is run in a scoped fiber that publishes to the hub.
- `Topology` is a snapshot of local IPv4 `Interface`s (MAC, address, prefix, broadcast address), held in a `SubscriptionRef`. It is discovered once at startup and then every `refreshInterval`. `interfaceFor` picks the longest matching prefix. `Outgoing.Broadcast` resolves to the interfaces' broadcast addresses (or `255.255.255.255` if none are known), and `Outgoing.Send` targets explicit IPs.
- `events` diffs consecutive topologies into `Event.Deactivated`/`Event.Activated` (deactivations first). Each run of the stream subscribes independently, so one instance can be shared by any number of clients. Changes to broadcast addresses alone emit nothing.

### `main` (the `Lighting` implementation)
- `LightingLive` holds `Ref.Synchronized[Devices]` (immutable `byMac`/`byIp` index of per-device `State`), a `Hub` of `(Light, Option[Source])` events for `subscribe`, and `ConcurrentMap`s of in-flight `Invocation`s and last dispatch times.
- **Incoming flow**: network stream → `targetKey` (MAC from payload, else by invocation id, else by IP) → `groupByKey` so each device is processed sequentially → `onMessage` dispatches by response type → `Changes` (`Registered` / `Updates` / `Settings`) → `Devices.applyChanges` → publish to the hub if the `Light` changed.
- **Outgoing flow**: `invoke(targets, update)(request)` creates one `Invocation` per target. Each has a unique correlation id and a forked fiber running `dispatchLoop` (coalesces rapid updates per device within 50 ms) and then `retryLoop` (resends using `retryBackoffs` until a response with that id arrives or the backoffs run out). A new update cancels any in-flight invocations for the same device.
- **Optimistic updates**: when an update is sent, it is applied to `State` right away with an `authoritativeUntil` guard. Until the guard expires, conflicting state reported by the bulb is ignored, so the UI doesn't flicker back. The guard is cleared when the bulb acknowledges or the request times out. After a timeout, a `getPilot` re-read follows.
- **Background loops** (started in `LightingLive.make`): `discover` (broadcast `getPilot`, unicast probes to silent devices, `knownDevices` for other subnets, mark inactive devices disconnected), `register` (refresh the `registration` subscription before `registrationTtl` expires, or when the device or local IP changes), and `purge` (drop devices past `purgeTimeout`, expire stale invocations, refresh `getSystemConfig` hourly). Network `events`, debounced by 500 ms, force re-registration and discovery.
- Times are monotonic milliseconds taken from `Clock.nanoTime`, so tests can control them with `TestClock`.
