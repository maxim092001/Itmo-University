import subprocess
import glob

print("JUnit mode or 0/1 mode?: ")
mode = input()


def run_tests():
    if mode == "0/1":
        art = glob.glob("../../java-advanced-2021/artifacts/*.jar")
        lib_k = glob.glob("../../java-advanced-2021/lib/*.jar")
        lib_m = glob.glob("../lib/*.jar")
        java_args = [
            "-cp",
            ';'.join(art + lib_k + lib_m + ["out/"]),
            "../java-solutions/info/kgeorgiy/ja/grankin/rmi/BankTests.java"
        ]
    else:
        java_args = [
            "-jar",
            "../lib/junit-platform-console-standalone-1.8.0-M1.jar",
            "-cp",
            "out/",
            "--scan-class-path",
        ]
    subprocess.Popen("java " + " ".join(java_args), shell=True).wait()


run_tests()
