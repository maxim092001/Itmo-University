import kotlin.reflect.KProperty

class RegularInt(var value: Int) {
    operator fun getValue(thisRef: Any?, prop: KProperty<*>): Int {
        return this.value
    }

    operator fun setValue(thisRef: Any?, prop: KProperty<*>, value: Int) {
        this.value = value
        updateStep()
    }
}
