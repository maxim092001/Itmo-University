package raft.test

import raft.*

class MockStateMachine(private val actions: ActionSink) : StateMachine() {
    override fun apply(command: Command): CommandResult {
        actions += ProcessAction.ApplyCommand(command)
        return super.apply(command)
    }
}
