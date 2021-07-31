import subprocess
from pathlib import Path
import shutil

def compile_java():
    javac_args = [
        "--module-path",
        "../../java-advanced-2021/artifacts;../../java-advanced-2021/lib",
        "../java-solutions/module-info.java",
        "../java-solutions/info/kgeorgiy/ja/grankin/implementor/*.java",
        "-d",
        "out"
    ]
    subprocess.Popen("javac " + " ".join(javac_args), shell=True).wait()

compile_java()

def create_jar():
    jar_args = [
        "--module-path",
        "../../../java-advanced-2021/artifacts;../../java-advanced-2021/lib",
        "-c",
        "-v",
        "--file",
        "implementor.jar",
        "--manifest",
        "../java-solutions/info/kgeorgiy/ja/grankin/implementor/META-INF/MANIFEST.MF",
        "-C",
        "out",
        "."
    ]
    subprocess.Popen("jar " + " ".join(jar_args) , shell=True).wait()

create_jar()


def create_jar_dir():
    Path("jar-out").mkdir(parents=True, exist_ok=True)

def move_jar_to_dir():
    shutil.move("implementor.jar", "jar-out/implementor.jar")

create_jar_dir()
move_jar_to_dir()