import subprocess
import glob

def compile_java():
    art = glob.glob("../../java-advanced-2021/artifacts/*.jar")
    lib_k = glob.glob("../../java-advanced-2021/lib/*.jar")
    lib_m = glob.glob("../lib/*.jar")
    javac_args = [
        "--module-path",
        ':'.join(art + lib_k + lib_m),
        "-cp",
        "../lib/*.jar",
        "../java-solutions/info/kgeorgiy/ja/grankin/rmi/*.java",
        "../java-solutions/info/kgeorgiy/ja/grankin/rmi/bank/*.java",
        "../java-solutions/info/kgeorgiy/ja/grankin/rmi/person/*.java",
        "../java-solutions/info/kgeorgiy/ja/grankin/rmi/account/*.java",
        "-d",
        "out"
    ]
    subprocess.Popen("javac " + " ".join(javac_args), shell=True).wait()

compile_java()