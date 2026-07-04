//// The Agent Record's [[CanBlock]] flag (ES2024 §9.7), as process-local
//// state (`arc_agent_ffi.erl`).
////
//// [[CanBlock]] is per-agent spec POLICY, not an embedder capability, so it
//// is deliberately NOT part of `host_hooks.HostHooks`: it lives in the
//// process dictionary of the agent's BEAM process, is read ONCE at realm boot
//// (`interpreter.new_state` seeds `State.can_block` from it) and never
//// changes for a running agent.
////
//// It defaults to True — arc's main agent and every spawned agent child can
//// block. The only writer is an embedder that must opt out BEFORE booting the
//// agent's realm: the test262 runner clears it in the test's worker process
//// for tests flagged `CanBlockIsFalse`.
////
//// AgentCanSuspend() (§25.4.3.14 DoWait step 10) is
//// `State.can_block && option.is_some(ctx.host_hooks.atomics)`: policy AND
//// capability.

/// Read the calling process's [[CanBlock]]. Unset (a fresh agent process) is
/// True.
@external(erlang, "arc_agent_ffi", "can_block")
pub fn can_block() -> Bool

/// Set the calling process's [[CanBlock]]. Call BEFORE booting the agent's
/// realm — `State.can_block` is a snapshot taken at `interpreter.new_state`.
@external(erlang, "arc_agent_ffi", "set_can_block")
pub fn set_can_block(can: Bool) -> Nil
