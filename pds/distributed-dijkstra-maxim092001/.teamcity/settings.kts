import jetbrains.buildServer.configs.kotlin.v2018_2.*
import jetbrains.buildServer.configs.kotlin.v2018_2.vcs.GitVcsRoot

version = "2019.1"

project {
    buildType {
        id("Build")
        name = "Build"
        vcs {
            root(DslContext.settingsRoot)
        }
    }
}