import subprocess
import os
import glob
import difflib
import sys

def hilite(string, color):
  attr = []
  if color == 'green':
    attr.append('32')
  elif color == 'red':
    attr.append('31')
  return '\x1b[%sm%s\x1b[0m' % (';'.join(attr), string)

def main():

  total = 0
  success = 0

  for root, dirs, files in os.walk("./test"):
    for f in files:
      if f.endswith(".mod"):
        total += 1
        f = os.path.join(root, f)[2:]
        with open("result/you/" + f, "w") as youout:
          print("running", f)
          print("diff reference and yours")

          yours = subprocess.Popen(["./snuplc/test_parser", f], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
          youout.write(yours.stdout.read().decode())
          youout.write(yours.stderr.read().decode())

        with open("result/reference/" + f, "r") as refin, open("result/you/" + f, "r") as youin:
          reflines = refin.readlines()
          youlines = youin.readlines()

          d = difflib.Differ()
          diff = d.compare(reflines, youlines)
          cnt = 0
          for line in diff:
            if line[0] == '-':
              if sys.stdout.isatty():
                print(hilite(line, 'red'), end='')
              else:
                print(line, end='')
              cnt += 1
            elif line[0] == '+':
              if sys.stdout.isatty():
                print(hilite(line, 'green'), end='')
              else:
                print(line, end='')
              cnt += 1

        if cnt == 0:
          success += 1
          print("all same")

        subprocess.Popen(["rm", "ref"]).communicate()
        subprocess.Popen(["rm", "you"]).communicate()

  print()
  print("score", success, "/", total)


if __name__ == "__main__":
  main()
