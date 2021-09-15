#!/usr/bin/env python3

STD_WHITE = 0xB5CEC5

D = [
  (0xCC0000, "RED", "Red"),
  (0xC81200, "BLZ", "Blaze Orange"),
  (0xE02D00, "ORA", "Orange"),
  (0xEE6A00, "YEL", "Yellow"),
  (0x559900, "SNT", "Snot"),
  (0x00BB00, "GRN", "Green"),
  (0x00AC2C, "SGR", "Sea Green"),
  (0x008899, "CYN", "Cyan"),
  (0x003799, "SKY", "Sky Blue"),
  (0x0000BB, "BLU", "Blue"),
  (0x2A00BB, "PUR", "Purple"),
  (0x7000C4, "VIO", "Violet"),
  (0xAA0090, "MAG", "Magenta"),
  (0xD2002C, "CER", "Cerise"),
  (0xCC2235, "PNK", "Pink"),
]

basic_xforms = [
  (lambda x: x),
  (lambda x: (((56 * c + 50) // 100) for c in x)),
  (lambda x: (((8 * c + 50) // 100) for c in x)),
]
basic_xffads = [
  ("{}", "{}", "Main colours"),
  ("{}1", "Medium {}", "Medium intensity colours"),
  ("{}2", "Dark {}", "Low intensity colours"),
]

gs_names = [
  ("BLK", "Black"),
  ("WHT2", "Grey"),
  ("WHT1", "Silver"),
  ("WHT", "Standard white"),
  ("XEN", "Xenon flash"),
]

gs_xforms = [
  (lambda x: t3(0x000000)),
  (lambda x: tuple((((10 * c + 50) // 100) for c in x))),
  (lambda x: tuple((((35 * c + 50) // 100) for c in x))),
  (lambda x: x),
  (lambda x: t3(0xCADEE4)),
]

def t3(x):
  return (x >> 16), (x >> 8) & 255, x & 255

def s3(t3):
  return (t3[0] << 16) + (t3[1] << 8) + t3[2]

def pastelfn(t3):
  v = max(t3)
  result = tuple(
    (v - (6 * (v - c) + 4) // 8 for c in t3)
  )
  return result

def main():

  print("BAPM_Pal_Basic:")
  print("  resetenum")
  print("  bablock")
  print("  ; Greyscale")

  clix = 0

  fmt = "  enumdat {:<9}dbargb, 0x{:06X}  ; {:>2}{}"

  for gsns, xfn in zip(gs_names, gs_xforms):
    enumname, cmmt = gsns
    cmmtfield = ["",": "][cmmt != ""] + cmmt
    x = s3(xfn(t3(STD_WHITE)))
    print(fmt.format(enumname + ",", x, clix, cmmtfield))
    clix += 1

  for xfn, xffad in zip(basic_xforms, basic_xffads):
    efmt, cfmt, xfname = xffad
    if xfname:
      print("  ; {}".format(xfname))
    for colour, base_enum_name, desc in D:
      enumname = efmt.format(base_enum_name)
      x = s3(tuple(xfn(t3(colour))))
      cmmt = cfmt.format(desc)
      cmmtfield = ["",": "][cmmt != ""] + cmmt
      print(fmt.format(enumname + ",", x, clix, cmmtfield))
      clix += 1

  print("  ; Special colours, including dark ones "
        "which risk rounding to black")
  fmt = "  enumdat {:<9}dbargb, 0x{:06X}  ; {:>2}: {}"
  for enumname, x, desc in [
    ("FLA",   0xCC3F09, "Flame centre"),
    ("FLA1",  0x701602, "Flame periphery"),
    ("FLA2",  0x150300, "Flame tip"),
    ("FLA3",  0x050100, "Flame background"),
    ("BLP1",  0x0C006B, "Medium Blurple"),
    ("PSG",   0x2CCC48, "Pale Sea Green"),
    ("RED3",  0x040000, "Darkest Red"),
    ("ORA3",  0x040100, "Darkest Orange"),
    ("GRN3",  0x000400, "Darkest Green"),
    ("BLU3",  0x000004, "Darkest Blue"),
    ("PUR3",  0x010004, "Darkest Purple"),
    ("VIO3",  0x030005, "Darkest Violet"),
  ]:
    cmmtfield = ["",": "][desc != ""] + desc
    print(fmt.format(enumname + ",", x, clix, cmmtfield))
    clix += 1

  print("  endbab")
  print("BASIC_PALETTE_LENGTH equ ENUMIX")


if __name__ == '__main__':
  main()
