package dijkstra.system.classloader

import java.io.File
import java.net.URLClassLoader

class AntiCheatClassLoader(parent: ClassLoader) : URLClassLoader(classpathUrls, parent) {
    companion object {
        private val classpathUrls = System
            .getProperty("java.class.path")
            .split(System.getProperty("path.separator"))
            .map { File(it).toURI().toURL() }
            .toTypedArray()
    }

    override fun loadClass(name: String?): Class<*> {
        return if (name?.startsWith("dijkstra.ProcessImpl") == true) {
            findClass(name)
        } else {
            super.loadClass(name)
        }
    }
}
