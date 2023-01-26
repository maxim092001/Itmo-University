abstract class AbstractFoo<Self : AbstractFoo<Self>> {
    abstract fun test(): Self
}

class FooImpl : AbstractFoo<FooImpl>() {
    override fun test(): FooImpl {
        return this
    }
}