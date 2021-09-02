#!/usr/bin/python3


# (c) Copyright 2021, Daniel Neville

# This file is part of PrettyPoi47.
#
# PrettyPoi47 is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# PrettyPoi47 is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with PrettyPoi47. If not, see <https://www.gnu.org/licenses/>.


import numpy as np
import matplotlib.pyplot as plt
import matplotlib.ticker as ticker
#import random


def lcg_po2m(nbits, a, c, seed):
  # Linear Congruential Generator with a power-of-2 modulus
  #
  # Lower bits have short period. The lowest three bits are useless.
  # A full period LCM is be achieved when
  #   1. m and c are co-prime;
  #   2. a - 1 is divisible by all prime factors of m and
  #   3. a - 1 is divisible by 4 if m is divisible by 4.
  # (c = 1 is a common choice.)
  m = 1 << nbits
  mask = m - 1
  state = seed & mask
  while True:
    state = (a * state + c) & mask
    yield state


def urand(seed):
  # Generate random numbers with a uniform distribition in the range [0, 1).
  nbits = 128
  # Pick a nice multiplier from "Computationally easy, spectrally good
  # mutipliers for congruential pseudorandom number generators" by
  # Guy Steele and Sebastiano Vigna, January 22, 2021, arXiv:2001.05304v2.
  a = 0x8855c9aa096cdcc0eae76c902f3f2335  # For m = 2^128 and suitable c
  m = 1 << nbits
  rng = lcg_po2m(nbits, a, 1, seed & (m - 1))
  while True:
    x = next(rng)
    yield x / m


def lin_fn(x):
  # Linear reconstruction filter
  return (x + 1 if x < 0 else 1 - x) if -1 < x < 1 else 0


def lanczos(x, a):
  # Lanczos reconstruction filter with size parameter a, a positive integer
  #
  # a = 1 is useless. Larger values increasingly emulate the normalised sinc
  # function. (Lanczos is just the normalised sinc function windowed with the
  # horizontally scaled central lobe of another normalised sinc function.)
  return (np.sinc(x) * np.sinc(x / a)) if -a < x < a else 0


def lanczos_fn(a):
  # Return a 1-argt lanczos function tailored with a specific size parameter.
  return lambda x: lanczos(x, a)


def upsample(values, new_n, filter_fn, support, circular=False):
  result = []
  n = len(values)
  if circular:
    for j in range(new_n):
      x = j * n / new_n
      rng = range(
        int(np.floor(x)) + support[0] + 1,
        int(np.floor(x)) + support[1] + 1
      )
      result.append(sum(values[i % n] * filter_fn(x - i) for i in rng))
  else:
    a = max(0, -support[0])
    b = max(0, support[1])
    for j in range(new_n):
      x = j * (n - 1) / (new_n - 1)
      rng = range(
        int(np.floor(x)) + support[0] + 1,
        int(np.floor(x)) + support[1] + 1
      )
      s = sum(values[np.clip(i, 0, n - 1)] * filter_fn(x - i) for i in rng)
      result.append(s)
  return result


def rgb24int_to_rgbf(x):
  x = np.asarray(x)
  r = (x >> 16) & 255
  g = (x >> 8) & 255
  b = x & 255
  rgbf = np.vstack([r, g, b]).T / 255.0
  return rgbf


def rgbf_to_rgb24int(X):
  X = np.asarray(X)
  T = np.rint(X.clip(0, 1) * 255) * (1 << 16, 1 << 8, 1)
  c = T.sum(axis=1, dtype = np.uint32)
  return c


def graph_interpolation_test():

  D = [1, 3, 0, 1, 2, 3, 2, 2, 3, 1, 0]
  n = len(D)
  n1 = 10 * n

  L0 = upsample(D, n1, lin_fn, (-1, 1), circular=True)
  L2 = upsample(D, n1, lanczos_fn(2), (-2, 2), circular=True)
  L3 = upsample(D, n1, lanczos_fn(3), (-3, 3), circular=True)

  # Because the data is periodic, the plot needs an additional
  # entry (a copy of the first element).
  x = np.arange(n + 1)
  x1 = np.arange(n1 + 1) * n / n1

  fig = plt.figure()
  ax = plt.axes()
  ax.xaxis.set_major_locator(ticker.MultipleLocator(1))
  ax.yaxis.set_major_locator(ticker.MultipleLocator(1))
  ax.grid()

  plt.scatter(x, np.hstack([D, D[0]]), 20, 'black', marker='o', label="Input")
  plt.plot(x1, np.hstack([L0, L0[0]]), 'cyan', label="Linear")
  plt.plot(x1, np.hstack([L2, L2[0]]), 'blue', label="Lanzsos (a = 2)")
  plt.plot(x1, np.hstack([L3, L3[0]]), 'red', label="Lanzsos (a = 3)")
  ax.legend()
  plt.xlabel("Sample Index")
  plt.ylabel("Value, Sampled and Interpolated")
  plt.title("Linear and Lanczos Interpolation")
  plt.show(block=False)


def single_pulse_fn(x):
  d = 0.4
  x1 = 3.5 * x
  if x1 < 0:
    y = 0
  elif x1 < 1.0:
    y = 0.5 - 0.5 * np.cos(x1 * np.pi)
  elif x1 < 2.5:
    a = 0.5 * (1 + d)
    y = 1 - a + a * np.cos((x1 - 1.0) * np.pi * 2/3)
  elif x1 < 3.5:
    y = d * (-0.5 - 0.5 * np.cos((x1 - 2.5) * np.pi))
  else:
    y = 0
  return y


def pulse_train_fn(x):
  x1 = (x % 1) * 1.25
  return single_pulse_fn(x1)


def pulsejet_fn(progress, position):
  wpos = 2.0 * progress
  b = 0.28 + 0.59 * (1 - position ** 0.6)
  if 0 <= position <= 1:
    d = pulse_train_fn(0.5 * (wpos - position))
    w = 0.25 + 0.75 * (np.sin(np.pi * position ** 0.3)) + 0.02 * position
  else:
    d = 0
    w = 0
  y = b + 0.3 * w * d
  return y


def graph_single_pulse_fn():
  fig = plt.figure()
  ax = plt.axes()
  x = np.arange(0, 1, 1/64)
  y = np.array([single_pulse_fn(x_n) for x_n in x])
  ax.plot(x, y)
  ax.grid()
  plt.xlabel("Progress")
  plt.ylabel("Displacement")
  plt.title("Single Pulse Profile")
  plt.show(block=False)


def graph_pulsejet_fn():
  fig = plt.figure()
  ax = plt.axes()
  for i, t in enumerate(np.arange(0, 1, 1/32)):
    x = np.arange(0, 1, 1/128)
    y = np.array([pulsejet_fn(t, x_n) for x_n in x])
    ax.plot(x, y, alpha=[0.25, 1.0][i & 3 == 0])
  ax.grid()
  plt.xlabel("Position")
  plt.ylabel("Ramp Index")
  plt.title("Pulsejet Animation Frames")
  plt.show(block=False)


def graph_palette_interpolation(keypal_rgbf, ipal_rgbf, name, xkeypal=False):

  P0 = keypal_rgbf
  P1 = ipal_rgbf
  n0 = len(P0)
  n1 = len(P1)

  fig = plt.figure()
  ax = plt.axes()
  ax.xaxis.set_major_locator(ticker.MultipleLocator(1))
  #ax.yaxis.set_major_locator(ticker.MultipleLocator(0.25))
  ax.grid()
  n = len(P0)

  if xkeypal:
    # X coordinates given for key palette indices
    x = np.arange(n0)
    x1 = np.arange(n1) * (n0 - 1) / (n1 - 1)
  else:
    # X coordinates given for interpolated palette indices
    x = np.arange(n0) * (n1 - 1) / (n0 - 1)
    x1 = np.arange(n1)
  scat_props = {
    0: dict(color='red', label="$R_{in}$"),
    1: dict(color='green', label="$G_{in}$"),
    2: dict(color='blue', label="$B_{in}$"),
  }
  line_props = {
    0: dict(color='red', label="$R_{out}$"),
    1: dict(color='green', label="$G_{out}$"),
    2: dict(color='blue', label="$B_{out}$"),
  }
  for ch, kwargs in scat_props.items():
    ax.scatter(x, P0.T[ch], 20, marker='o', **scat_props[ch])
  for ch, kwargs in line_props.items():
    ax.plot(x1, P1.T[ch], **line_props[ch])
  ax.legend()
  title = "Palette Interpolation"
  if name:
    title = str(name) + " " + title
  plt.xlabel(("Input" if xkeypal else "Output") + " Index")
  plt.ylabel("Channel Level (1.0 = Full)")
  plt.title(title)

  plt.show(block=False)


def graph_pat3(keypat3, ipat3, name=None, xkeypal=False):

  P1 = np.array(ipat3)
  n1 = len(P1)
  if keypat3 is not None:
    P0 = np.array(keypat3)
    n0 = len(P0)
  else:
    P0 = None
    n0 = 1

  fig = plt.figure()
  ax = plt.axes()
  ax.xaxis.set_major_locator(ticker.MultipleLocator(5))
  ax.yaxis.set_major_locator(ticker.MultipleLocator(3))
  ax.grid()

  if xkeypal and P0 is not None:
    # X coordinates given for key palette indices
    x = np.arange(n0 + 1)
    x1 = np.arange(n1 + 1) * n0 / n1
  else:
    # X coordinates given for interpolated palette indices
    x = np.arange(n0 + 1) * n1 / n0
    x1 = np.arange(n1 + 1)

  scat_props = {
    0: dict(color='indigo', label="$Key_{prox,4c}$"),
    1: dict(color='darkorange', label="$Key_{med,4c}$"),
    2: dict(color='deepskyblue', label="$Key_{dist,4c}$"),
  }
  line_props = {
    0: dict(color='indigo', label="$Out_{prox,16c}$"),
    1: dict(color='darkorange', label="$Out_{med,16c}$"),
    2: dict(color='deepskyblue', label="$Out_{dist,16c}$"),
  }

  if P0 is not None:
    for ch in [0, 1, 2]:
      Y = P0.T[ch]
      Yc = np.hstack((Y, Y[0]))
      plt.scatter(x, Yc, 20, marker='o', **scat_props[ch])
  for ch in [0, 1, 2]:
    Y = P1.T[ch]
    Yc = np.hstack((Y, Y[0]))
    plt.plot(x1, Yc, alpha=0.6, **line_props[ch])

  xlstr = "Output"
  if P0 is not None and xkeypal:
    xlstr = "Input"

  nstr = (name + " ") if name else ""
  plt.xlabel(xlstr + " Frame Index")
  plt.ylabel("Ramp Index")
  plt.title(nstr + "3-LED Indexed Colour Animation")
  plt.legend(fancybox=True, framealpha=0.6)
  plt.show(block=False)


def gen_interp_palette(keypal_rgb24int, n, name=None, showgraph=False):

  P0 = rgb24int_to_rgbf(keypal_rgb24int)
  P1 = np.array(upsample(P0, n, lanczos_fn(2), (-2, 2)))
  B = rgbf_to_rgb24int(P1)

  nstr = (" " + name + " ") if name else " "
  symstr = ("BAPM_Pal_Ramp16_" + name) if name else "BAPM_Pal_Ramp16"
  print("; Interpolated{}Palette Colours:".format(nstr))
  print(symstr + ":")
  print("        bablock")
  for x in B:
    print("        dbvrgb  0x{:06X}".format(x))
  print("        endbab")
  print("")

  if showgraph:
    graph_palette_interpolation(P0, P1, name, xkeypal=False)


def add_reps_field(P):
  R = []
  Q = []
  ix = 0
  reps = 1
  while len(Q) > 0 or ix == 0:
    if ix < len(P):
      Q.append(np.array(P[ix]).tolist())
    if len(Q) >= 2 or ix >= len(P):
      do_pop = True
      if len(Q) >= 2:
        if (Q[0] == Q[1]):
          Q.pop(0)
          reps += 1
          do_pop = False
      if do_pop:
        row = Q.pop(0)
        R.append([reps] + row)
        reps = 1
    ix += 1
  return R


def print_pattern(L, name=None, use_local_frames_list=None):
  if use_local_frames_list is None:
    use_local_frames_list = True
  indstr = " " * 8
  patsymstr = "PM_Pattern"
  framessymstr = ""
  framesrefstr = "0"
  use_local_frames_list = True
  if name:
    patsymstr = ("PM_Pat_" + name)
    if not use_local_frames_list:
      framesrefstr = framessymstr = "BAPM_Frs_" + name
  print(patsymstr + ":")
  print("{}pattern PATDF_16C, 16, 1, PATSF_BAPM_PMF, {}"
        .format(indstr, len(L)))
  print("{}dw BAPM_Pal_Ramp16_Flame, 0, {}"
        .format(indstr, framesrefstr))
  if framessymstr:
    print(framessymstr + ":")
  print(indstr + "bablock")
  for row in L:
    print("{}pf16c   {:>3}, {:>3},{:>3},{:>3}".format(indstr, *row))
  print(indstr + "endbab")
  print("")


def gen_interp_pat(keypat_4c, n, seed=1, name=None, showgraph=False):

  rng = urand(seed)

  # 0, 3, 2, 1 -> 0, 1, 2, 3
  P0 = (4 - np.array(keypat_4c)) & 3
  # Scale for special fire palette.
  P0 = P0 * 15.0 / 3.0
  n0flat = len(P0) * 3
  R = np.array([-1 + 2 * next(rng) for i in range(n0flat)]).reshape(P0.shape)
  P0R = (P0 * 13.0 / 15.0 + 0.5) + R * 3.0

  P1 = np.array(upsample(P0R, n, lanczos_fn(2), (-2, 2),
                circular=True))
  P2 = np.array(np.rint(P1.clip(0.0, 15.0)), dtype=np.uint32)

  P3 = add_reps_field(P2)
  print_pattern(P3, name)

  if showgraph:
    graph_pat3(P0, P2, name, xkeypal=False)


def gen_pulsejet_pat(n, name=None, showgraph=False):
  P = []
  positions = [0.05, 0.40, 0.72]
  xscale = 0.95
  taps = [-0.07, 0, 0.07]
  weights = [0.25, 0.5, 0.25]
  for i in range(n):
    t = i / n
    xferfn = lambda y: 0.0 + 1.0 * (y ** 1.0)
    F = []
    for p in positions:
      y = 0
      for tp, tw in zip(taps, weights):
        y += tw * pulsejet_fn(t * xscale, np.clip(p + tp, 0, 1))
      rix = np.clip(int(round(15 * y)), 0, 15)
      F.append(rix)
    P.append(F)
  P1 = add_reps_field(P)
  print_pattern(P1, name)
  if showgraph:
    graph_pat3(None, P, name, xkeypal=False)


def main():

  pal_flame = [  # Realistic slightly pastel orange flame
    0,
    0x050000,
    0x170200,
    0x701602,
    0xCC3F09,
  ]

  pal_flurple = [  # Blurple flame
    0,
    0x000004,
    0x000016,
    0x0D006B,
    0x5500CC,
  ]

  pal_blaze = [  # Based on the standard Fire colour ramp
    0,
    0x040000,
    0x100000,
    0x780B00,
    0xE02D00,
  ]

  pal_redfire = [  # Based on the standard Fire colour ramp
    0,
    0x050000,
    0x1E0000,
    0xA00500,
    0xDA1A00,
  ]

  pal_strawberry = [  # Strawberry fire
    0,
    0x060000,
    0x1A0000,
    0x800615,
    0xCC3342,
  ]

  pal_bunsen = [
    0,
    0x010005,
    0x030017,
    0x000870,
    0x003A99,
  ]

  pal_acrid = [
    0,
    0x000400,
    0x071400,
    0x334C00,
    0x887700,
  ]

  # In order of brightness: 0, 3, 2, 1
  pat_hub_brazier = np.array([
    [1,  2,  3],
    [1,  1,  3],
    [1,  2,  2],
    [2,  3,  0],
    [1,  2,  3],
    [2,  2,  3],
    [2,  3,  3],
    [1,  3,  3],
    [1,  3,  0],
    [1,  2,  0],
    [2,  2,  3],
    [3,  2,  3],
    [2,  3,  0],
    [1,  3,  0],
    [2,  2,  3],
    [3,  3,  0],
    [2,  3,  0],
    [1,  3,  3],
    [1,  2,  3],
    [1,  1,  3],
    [1,  1,  2],
    [2,  2,  1],
    [1,  3,  2],
    [1,  2,  2],
    [1,  1,  2],
    [1,  2,  2],
    [1,  2,  3],
    [2,  2,  3],
    [2,  3,  3],
    [3,  3,  0],
    [1,  3,  0],
    [1,  2,  3],
    [1,  1,  3],
    [2,  3,  2],
    [3,  0,  2],
    [2,  0,  0],
    [1,  3,  0],
  ])

  # In order of brightness: 0, 3, 2, 1
  pat_rim_brazier = np.array([
    [3,  2,  1],
    [3,  1,  1],
    [2,  2,  1],
    [0,  3,  2],
    [3,  2,  1],
    [3,  2,  2],
    [3,  3,  2],
    [3,  3,  1],
    [0,  3,  1],
    [0,  2,  1],
    [3,  2,  2],
    [3,  2,  3],
    [0,  3,  2],
    [0,  3,  1],
    [3,  2,  2],
    [0,  3,  3],
    [0,  3,  2],
    [3,  3,  1],
    [3,  2,  1],
    [3,  1,  1],
    [2,  1,  1],
    [1,  2,  2],
    [2,  3,  1],
    [2,  2,  1],
    [2,  1,  1],
    [2,  2,  1],
    [3,  2,  1],
    [3,  2,  2],
    [3,  3,  2],
    [0,  3,  3],
    [0,  3,  1],
    [3,  2,  1],
    [3,  1,  1],
    [2,  3,  2],
    [2,  0,  3],
    [0,  0,  2],
    [0,  3,  1],
  ])

  # In order of brightness: 0, 3, 2, 1
  pat_mid_brazier = np.array([
    [2,  1,  2],
    [2,  1,  3],
    [1,  1,  0],
    [1,  2,  3],
    [2,  1,  3],
    [2,  1,  2],
    [3,  2,  1],
    [2,  1,  2],
    [3,  1,  2],
    [0,  2,  1],
    [2,  1,  0],
    [1,  1,  3],
    [1,  2,  1],
    [3,  1,  2],
    [2,  2,  1],
    [1,  3,  2],
    [3,  1,  0],
    [2,  1,  3],
    [0,  2,  2],
    [3,  2,  0],
    [2,  1,  3],
    [1,  1,  1],
    [3,  1,  2],
    [2,  1,  1],
    [1,  2,  3],
    [2,  3,  0],
    [0,  1,  3],
    [3,  2,  3],
    [2,  1,  2],
    [3,  1,  1],
    [3,  2,  3],
    [2,  1,  0],
    [1,  1,  2],
    [2,  1,  3],
    [3,  2,  3],
    [1,  1,  2],
    [2,  2,  1],
    [0,  3,  2],
    [3,  2,  0],
    [2,  2,  3],
  ])

  #graph_interpolation_test()
  #gen_interp_palette(pal_flame, 16, "Flame", showgraph=True)
  #gen_interp_palette(pal_blaze, 16, "Blaze", showgraph=True)
  #gen_interp_palette(pal_redfire, 16, "RedFire", showgraph=True)
  #gen_interp_palette(pal_flurple, 16, "Flurple", showgraph=True)
  gen_interp_palette(pal_bunsen, 16, "Bunsen", showgraph=True)
  #gen_interp_palette(pal_strawberry, 16, "Strawberry", showgraph=True)
  #gen_interp_palette(pal_acrid, 16, "Acrid", showgraph=True)

  gen_interp_pat(pat_hub_brazier, 148, 2, "HQHubBrazier", showgraph=True)
  #gen_interp_pat(pat_rim_brazier, 148, 2, "HQRimBrazier", showgraph=True)
  #gen_interp_pat(pat_mid_brazier, 160, 2, "HQMidBrazier", showgraph=True)

  graph_single_pulse_fn()
  graph_pulsejet_fn()
  gen_pulsejet_pat(48, "HQPulseJet", showgraph=True)

  plt.show()

if __name__ == '__main__':
  main()
