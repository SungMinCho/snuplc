import subprocess
import os
import glob
import difflib

def main():

  for root, dirs, files in os.walk("./test"):
    for f in files:
      if f.endswith(".mod"):
        f = os.path.join(root, f)
        with open("ref", "w") as refout, open("you", "w") as youout:
          print("running", f)
          print("diff reference and yours")

          ref = subprocess.Popen(["./reference/test_parser", f], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
          refout.write(ref.stdout.read().decode())
          refout.write(ref.stderr.read().decode())

          yours = subprocess.Popen(["./snuplc/test_parser", f], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
          youout.write(yours.stdout.read().decode())
          youout.write(yours.stderr.read().decode())

        with open("ref", "r") as refin, open("you", "r") as youin:
          reflines = refin.readlines()
          youlines = youin.readlines()

          d = difflib.Differ()
          diff = d.compare(reflines, youlines)
          print("".join(diff))

        subprocess.Popen(["rm", "ref"]).communicate()
        subprocess.Popen(["rm", "you"]).communicate()


if __name__ == "__main__":
  main()
