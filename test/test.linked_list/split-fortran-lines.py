#!/usr/bin/env python3
# coding=utf-8

import sys, os, os.path, shutil, glob
import shelve, pickle
import platform
# ----- my libs
import rdee
froed_dirs = [r'D:\recRoot\GitRepos\froed', # zjD windows
              '/mnt/d/recRoot/GitRepos/froed', # zjD wsl
              '/sharedata01/zjx/recRoot/GitRepos/froed'  # qlab
              ]
for fd in froed_dirs:
    if os.path.exists(fd):
        sys.path.append(fd)
        break
import froed



def main():
    # tarDir = '..'
    # files = glob.glob(f'{tarDir}/*.[fF]') + glob.glob(f'{tarDir}/*.[fF]90') + glob.glob(f'{tarDir}/iof_file_incs/*')
    # files = ['cctm5402.ssT/se_reconfig_grid_info_ext.f']
    # files = ['cctm5402.ssT/iof_file_incs/inc.se_data_send_module.se_1d_data_send.f']
    # for f in files:
        # modi1f(f)

    modi1f('inc.struct-io-interface.t1.F90')
    modi1f('inc.struct-io.t1.F90')


def modi1f(file):
    # rdee.logT("handling " + file)
    f = open(file, encoding='utf-8')
    lines = f.read().splitlines()
    f = open(file, 'w', encoding='utf-8')

    for i, L in enumerate(lines):
        # print(L)
        f.write(froed.FortranCode.FCode.check_line_length_and_split(L, getFortranFileFormat(file), 132) + "\n")
    f.close()



def getFortranFileFormat(ff):
    suffix = os.path.splitext(ff)[1]
    if suffix.upper() == '.F':
        return 'fixed'
    elif suffix.upper() == '.F90':
        return 'free'
    assert 1 == 0, 'Error, not a fortran file (f/F or f90/F90)'

if __name__ == '__main__':
    main()


